-module(nani_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, StartArgs) ->
    case nani_sup:start_link(StartArgs) of
        {ok, Pid} -> {ok, Pid};
        Other -> {error, Other}
    end.

stop(_State) -> ok.

% -export([init/0, init/1]).

% -define(DEFAULT_CONFIG, "./nani.config").

% init() -> init(?DEFAULT_CONFIG).

% init(ConfigFile) ->
%     case file:consult(ConfigFile) of
%         {ok, Config} ->
%             nani_event:start(),
%             nani_conn_info:add_handler(),  
%             nani_bot:start(Config),
%             nani_bot:connect();
%         Err -> Err
%     end.