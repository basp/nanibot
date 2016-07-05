-module(markov_sup).

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
    Server = {markov_server, {markov_server, start_link, []},
              permanent, 2000, worker, [markov_server]},
    Children = [Server],
    RestartStrategy = {one_for_one, 1, 5},
    {ok, {RestartStrategy, Children}}.