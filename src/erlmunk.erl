% vim: set expandtab softtabstop=4 shiftwidth=4:
-module(erlmunk).
-behaviour(gen_server).

-export([start_link/0]).
-export([space_new/3,
         space_add_body/5,
         body_set_position/4,
         body_set_angle/4,
         subscribe/4]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(ERLMUNK_CNODE, "priv/erlmunk_cnode").
-define(INIT_TIMEOUT, 3000).

-record(state, {
    port,
    cnode,
    init_ref,
    init_timer = undefined
}).

%% ----------------------------------------------------------
%% Public API
%% ----------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, [], []).

space_new(Pid, Iterations, {GravityX, GravityY} = Gravity)
        when is_integer(Iterations),
             is_float(GravityX),
             is_float(GravityY) ->
    gen_server:call(Pid, {space_new, {Iterations, Gravity}}).

space_add_body(Pid, SpaceRef, BodyId, Mass, Inertia)
        when is_integer(BodyId),
             is_float(Mass) ->
    gen_server:cast(Pid, {space_add_body, {SpaceRef, BodyId, Mass, Inertia}}).

body_set_position(Pid, SpaceRef, BodyId, {X, Y} = Vector)
        when is_integer(BodyId),
             is_float(X),
             is_float(Y) ->
    gen_server:cast(Pid, {body_set_position, {SpaceRef, BodyId, Vector}}).

body_set_angle(Pid, SpaceRef, BodyId, Angle)
        when is_integer(BodyId),
             is_float(Angle) ->
    gen_server:cast(Pid, {body_set_angle, {SpaceRef, BodyId, Angle}}).

subscribe(Pid, SpaceRef, SubscriberPid, {Left, Bottom, Right, Top} = BoundingBox)
        when is_pid(SubscriberPid),
             is_float(Left),
             is_float(Bottom),
             is_float(Right),
             is_float(Top) ->
    gen_server:cast(Pid, {space_subscribe_box, {SpaceRef, SubscriberPid, BoundingBox}}).

%% ------------------------------------------------------------------
%% Behaviour Function Exports
%% ------------------------------------------------------------------
init(_) ->
    CNodeName = "erlmunk" ++ integer_to_list(crypto:rand_uniform(1, 10000)),
    CNodeHost = lists:nth(2, string:tokens(atom_to_list(node()), [$@])),
    CNodeFullName = list_to_atom(CNodeName ++ "@" ++ CNodeHost),
    Port = erlang:open_port({spawn_executable, ?ERLMUNK_CNODE},
                             [binary,
                              {args, [CNodeName, CNodeHost,
                                      erlang:get_cookie()]}]),
    erlang:process_flag(trap_exit, true),
    true = erlang:link(Port),
    %% cast a message to self for initialization purposes
    Ref = erlang:make_ref(),
    Timer = erlang:send_after(?INIT_TIMEOUT, self(), init_timeout),
    gen_server:cast(self(), {init, {Ref}}),
    {ok, #state { port = Port,
                  cnode = CNodeFullName,
                  init_ref = Ref,
                  init_timer = Timer
                }}.

handle_cast({reply, _From, {inited, Ref}},
            #state{ cnode = Cnode,
                    init_timer = Timer,
                    init_ref = Ref } = State) ->
    lager:debug("chipmunk cnode ~p has been initialized", [Cnode]),
    _ = erlang:cancel_timer(Timer),
    {noreply, State#state{ init_timer = undefined }};
handle_cast({reply, From, Reply}, State) ->
    gen_server:reply(From, Reply),
    {noreply, State};
handle_cast({init, _Ref} = Msg, #state{cnode = Cnode} = State) ->
    call(Cnode, undefined, Msg),
    {noreply, State};
handle_cast(Msg, #state{cnode = Cnode} = State) ->
    lager:debug("message cast to cnode ~p: ~p",
        [Cnode, Msg]),
    io:format("message to cnode ~p: ~p\n",
        [Cnode, Msg]),
    cast(Cnode, Msg),
    {noreply, State}.

handle_call(Msg, From, #state{cnode = Cnode} = State) ->
    io:format("message call to cnode ~p, from: ~p: ~p\n",
        [Cnode, From, Msg]),
    call(Cnode, From, Msg),
    {noreply, State}.

handle_info(init_timeout, #state{cnode = Cnode} = State) ->
    lager:debug("timed out waiting for ~p initialization, re-requesting",
        [Cnode]),
    Ref = erlang:make_ref(),
    Timer = erlang:send_after(?INIT_TIMEOUT, self(), init_timeout),
    gen_server:cast(self(), {init, {Ref}}),
    {noreply, State#state { init_ref = Ref,
                            init_timer = Timer }};
handle_info({'EXIT', Port, normal}, #state{port = Port} = State) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(normal, _State) -> ok;
terminate(Reason, _State) ->
    lager:error("erlmunk_cnode died with reason ~p", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

cast(Cnode, Msg) ->
    {undefined, Cnode} ! {cast, Msg}.

call(Cnode, From, Msg) ->
    {undefined, Cnode} ! {call, From, Msg}.
