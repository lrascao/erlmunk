% vim: set expandtab softtabstop=4 shiftwidth=4:
-module(erlmunk_space).

-export([new/2,
         delete/2,
         add_body/5,
         remove_body/3,
         subscribe_collision/5,
         subscribe_box/4]).

new(Iterations, {GravityX, GravityY} = Gravity)
        when is_integer(Iterations),
             is_float(GravityX),
             is_float(GravityY) ->
    Pid = poolboy:transaction(erlmunk, fun(WorkerPid) -> WorkerPid end),
    {ok, SpaceRef} = gen_server:call(Pid, {space_new, {Iterations, Gravity}}),
    {ok, Pid, SpaceRef}.

delete(Pid, SpaceRef) ->
    gen_server:cast(Pid, {space_delete, {SpaceRef}}).

add_body(Pid, SpaceRef, BodyId, Mass, Inertia)
        when is_integer(BodyId),
             is_float(Mass) ->
    gen_server:cast(Pid, {space_add_body, {SpaceRef, BodyId, Mass, Inertia}}).

remove_body(Pid, SpaceRef, BodyId)
        when is_integer(BodyId) ->
    gen_server:cast(Pid, {space_remove_body, {SpaceRef, BodyId}}).

subscribe_collision(Pid, SpaceRef, TypeA, TypeB, SubscriberPid)
        when is_integer(TypeA),
             is_integer(TypeB),
             is_pid(SubscriberPid) ->
    gen_server:cast(Pid, {space_subscribe_collision,
                    {SpaceRef, TypeA, TypeB, SubscriberPid}}).

subscribe_box(Pid, SpaceRef, SubscriberPid, {Left, Bottom, Right, Top} = BoundingBox)
        when is_pid(SubscriberPid),
             is_float(Left),
             is_float(Bottom),
             is_float(Right),
             is_float(Top) ->
    gen_server:cast(Pid, {space_subscribe_box, {SpaceRef, SubscriberPid, BoundingBox}}).
