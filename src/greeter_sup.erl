-module(greeter_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%============================================================================
%%% API
%%%============================================================================
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%============================================================================
%%% supervisor callbacks
%%%============================================================================
init([]) ->
    Server = {greeter, {greeter, start_link, []},
              permanent, 2000, worker, [greeter]},
    Children = [Server],
    RestartStrategy = {one_for_one, 1, 5},
    {ok, {RestartStrategy, Children}}.