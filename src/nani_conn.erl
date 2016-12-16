-module(nani_conn).

-behavior(gen_server).

-define(SERVER, ?MODULE).
-define(CRLF, "\r\n").

%% API
-export([start_link/3, send/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

opts() -> [binary, {active, true}, {packet, line}, {keepalive, true}].

%%%============================================================================
%%% API
%%%============================================================================
start(Host, Port) ->
    Args = [Host, Port],
    gen_server:start({local, ?SERVER}, ?MODULE, Args, []).

start_link(Parent, Host, Port) ->
    Args = [Parent, Host, Port],
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

send(Pid, Data) -> 
    gen_server:cast(Pid, {send, [Data, ?CRLF]}).

%%%============================================================================
%%% gen_server callbacks
%%%============================================================================
init([Parent, Host, Port]) ->
    case gen_tcp:connect(Host, Port, opts()) of
        {ok, Socket} ->
            gen_statem:cast(Parent, success),
            {ok, {Parent, Socket}};
        {error, Reason} ->
            io:format("error connecting: ~p~n", [Reason]),
            {stop, Reason}
    end.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({send, Data}, {_Parent, Socket} = State) ->
    io:format("=> ~p~n", [Data]),
    gen_tcp:send(Socket, Data),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, _Socket, Data}, {Parent, _Socket} = State) ->
    io:format("<= ~p~n", [Data]),
    handle_data(Parent, Data),
    {noreply, State};

handle_info({tcp_error, _Socket, Reason}, State) ->
    io:format("error: ~p~n", [Reason]),
    {noreply, State};

handle_info({tcp_closed, Socket}, State) ->
    io:format("closed: ~p~n", [Socket]),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%============================================================================
%%% Internal functions
%%%============================================================================
handle_data(Parent, Data) ->
    [Line | _] = re:split(Data, ?CRLF),
    gen_statem:cast(Parent, {received, Line}).