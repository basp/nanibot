-module(nani_bot).

-behaviour(gen_statem).

%% API
-export([start/1, connect/0, stop/0, 
         send/1, join/1, say/2, emote/2, speak/2,
         nick/0, nick/1]).

% TODO: 
% nick/0 and nick/1 are a bit of a hack. We really should
% just parse the message for /NICK and other commands that We
% need to sync with the internal bot state.

%% state functions
-export([standby/3, connecting/3, registering/3, ready/3]).

%% gen_statem callbacks
-export([init/1, callback_mode/0, handle_event/4, terminate/3, code_change/4]).

-define(RPL_WELCOME,        "001").
-define(RPL_YOURHOST,       "002").
-define(RPL_CREATED,        "003").
-define(RPL_MYINFO,         "004").
-define(RPL_BOUNCE,         "005").
-define(RPL_TOPIC,          "332").
-define(RPL_NAMREPLY,       "353").
-define(RPL_ENDOFNAMES,     "366").

-define(REAL_NAME, "http://github.com/basp/nanibot").

-record(state, {nick, host, port, conn}).

name() -> nani_bot.
real_name() -> ?REAL_NAME.

%%%============================================================================
%%% API
%%%============================================================================
start(Config) -> 
    gen_statem:start({local, name()}, ?MODULE, Config, []).

connect() -> 
    gen_statem:cast(name(), connect).

join(Channel) ->
    Msg = ["JOIN ", Channel],
    gen_statem:cast(name(), {send, Msg}).

speak(To, Count) ->
    Tokens = markov_server:generate(Count),
    Text = string:join(Tokens, " "),
    say(To, Text).

say(To, Text) ->
    Msg = ["PRIVMSG ", To, " :", Text],
    gen_statem:cast(name(), {send, Msg}).

emote(To, Action) ->
    Msg = ["PRIVMSG ", To, " :", 1, "ACTION ", Action, 1],
    gen_statem:cast(name(), {send, Msg}).

send(Msg) -> 
    gen_statem:cast(name(), {send, Msg}).

stop() -> 
    gen_statem:stop(name()).

nick() ->
    gen_statem:call(name(), get_nick).

nick(Nick) ->
    gen_statem:cast(name(), {set_nick, Nick}).

%%%============================================================================
%%% gen_statem callbacks
%%%============================================================================
init(Config) ->
    Nick = proplists:get_value(nick, Config),
    Host = proplists:get_value(host, Config),
    Port = proplists:get_value(port, Config),
    Data = #state{nick = Nick, host = Host, port = Port},
    {ok, standby, Data}.

terminate(_Reason, _State, _Data) -> ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

handle_event(_EventType, _EventContent, _State, _Data) ->
    {keep_state_and_data, []}.

callback_mode() -> state_functions.

%%%============================================================================
%%% State functions
%%%============================================================================

%%%----------------------------------------------------------------------------
standby(cast, connect, Data) ->
    Host = Data#state.host,
    Port = Data#state.port,
    {ok, Pid} = nani_conn:start(self(), Host, Port),
    NewData = Data#state{conn = Pid},
    {next_state, connecting, NewData};

standby(_EventType, _EventContent, _Data) ->
    {keep_state_and_data, []}.

%%%----------------------------------------------------------------------------
connecting(cast, success, Data) ->
    Conn = Data#state.conn,
    Nick = Data#state.nick,
    send_login(Conn, Nick),
    {next_state, registering, Data};

connecting(_EventType, _EventContent, _Data) ->
    {keep_state_and_data, []}.

%%%----------------------------------------------------------------------------
registering(cast, {received, Msg}, Data) ->
    {match, Match} = nani_utils:parse(Msg),
    case Match of 
        [_, <<?RPL_WELCOME>>, _, _] ->
            {next_state, ready, Data};
        [_, _, <<"PING">>, Ping] ->
            Actions = [{next_event, internal, {ping, Ping}}],
            {keep_state_and_data, Actions};
        _ -> 
            {keep_state_and_data, []}
    end;

registering(internal, {ping, Ping}, Data) ->
    Conn = Data#state.conn,
    send_pong(Conn, Ping),
    {keep_state_and_data, []};

registering(_EventType, _EventContent, _Data) ->
    {keep_state_and_data, []}.

%%%----------------------------------------------------------------------------
ready(internal, {ping, Ping}, Data) ->
    Conn = Data#state.conn,
    send_pong(Conn, Ping),
    {keep_state_and_data, []};

ready(internal, {join, Props}, _Data) ->
    _Channel = proplists:get_value(channel, Props), 
    _Nick = proplists:get_value(nick, Props),
    {keep_state_and_data, []};

ready(internal, {part, _Props}, _Data) ->
    {keep_state_and_data, []};

ready(internal, {names, Channel, Names}, Data) ->
    Nick = Data#state.nick,
    nani_event:names(Nick, Channel, Names),
    {keep_state_and_data, []};

ready(internal, {privmsg, Props}, Data) ->
    Nick = Data#state.nick,
    From = proplists:get_value(from, Props),
    To = proplists:get_value(to, Props),
    Text = binary_to_list(proplists:get_value(text, Props)),
    nani_event:privmsg(Nick, From, To, Text),
    {keep_state_and_data, []};

ready(cast, {received, Msg}, _Data) ->
    {match, Match} = nani_utils:parse(Msg),
    case Match of
        [_, <<?RPL_NAMREPLY>>, _Nick, _, Channel, NameData] ->
            Names = string:tokens(binary_to_list(NameData), " \t\r\n"),
            Actions = [{next_event, internal, {names, Channel, Names}}],
            {keep_state_and_data, Actions}; 
        [_, _, <<"PING">>, Ping] ->
            Actions = [{next_event, internal, {ping, Ping}}],
            {keep_state_and_data, Actions};
        [Nick, <<"JOIN">>, Channel] ->
            Props = [{nick, Nick}, {channel, Channel}],
            Actions = [{next_event, internal, {join, Props}}],
            {keep_state_and_data, Actions};
        [From, <<"PRIVMSG">>, To, Text] ->
            Props = [{from, From}, {to, To}, {text, Text}],
            Actions = [{next_event, internal, {privmsg, Props}}],
            {keep_state_and_data, Actions};
        _ -> 
            % io:format("~p~n", [Match]),
            {keep_state_and_data, []}
    end;

ready(cast, {send, Msg}, Data) ->
    Conn = Data#state.conn,
    send(Conn, Msg),
    {keep_state_and_data, []};
    
ready({call, From}, get_nick, Data) ->
    Actions = [{reply, From, Data#state.nick}],
    {keep_state_and_data, Actions};

ready({cast}, {set_nick, Nick}, Data) ->
    NewData = Data#state{nick = Nick},
    {keep_state, NewData};

ready(_EventType, _EventContent, _Data) ->
    {keep_state_and_data, []}.

%%%============================================================================
%%% Internal functions
%%%============================================================================
send(Conn, Data) -> 
    nani_conn:send(Conn, Data).

send_pong(Conn, Ping) -> 
    send(Conn, [<<"PONG :", Ping/binary>>]).

send_login(Conn, Nick) ->
    send(Conn, ["NICK ", Nick]),
    send(Conn, ["USER ", Nick, " 8 * :", real_name()]).