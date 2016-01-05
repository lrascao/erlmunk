-module(erlmunk_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD(I, Type, Restart), {I, {I, start_link, []}, Restart, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    PoolSpec = poolboy:child_spec(erlmunk,
                                  [{name, {local, erlmunk}},
                                   {worker_module, erlmunk},
                                   {size, 1}], []),
    {ok, {{one_for_one, 10, 10}, [PoolSpec]}}.
