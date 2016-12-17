-module(nani_sup).

-export([start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Args) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Args).

init(Args) ->
    Event = {nani_event, {nani_event, start_link, []}, 
             permanent, 2000, worker, [nani_event]},
    Bot = {nani_bot, {nani_bot, start_link, [Args]}, 
           permanent, 2000, worker, [nani_bot]},
    Children = [Event, Bot],
    RestartStrategy = {one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.