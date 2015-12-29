-module(erlmunk_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

-export([start_child/0]).

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

start_child() ->
    supervisor:start_child(?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {{simple_one_for_one, 60, 3600}, [
        ?CHILD(erlmunk, worker, temporary)
    ]}}.
