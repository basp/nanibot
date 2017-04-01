-module(nani_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, StartArgs) ->
    case nani_sup:start_link(StartArgs) of
        {ok, Pid} -> {ok, Pid};
        Other -> {error, Other}
    end.

stop(_State) -> ok.