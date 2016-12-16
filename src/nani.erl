-module(nani).

-export([init/1]).

init(ConfigFile) ->
    case file:consult(ConfigFile) of
        {ok, Config} ->
            nani_event:start(),
            nani_conn_info:add_handler(),  
            nani_bot:start(Config),
            nani_bot:connect();
        Err -> Err
    end.