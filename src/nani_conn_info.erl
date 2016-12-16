-module(nani_conn_info).

-behavior(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, 
         terminate/2, code_change/3]).

-export([add_handler/0, delete_handler/0]).

init([]) ->
    {ok, []}.

handle_event({receive, Data}, State) ->
    io:format("<= ~p~n", [Data]),
    {ok, State};

handle_event({send, Data), State) ->
    io:format("=> ~p~n", [Data]),
    {ok, State};

handle_event(_Event, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

add_handler() ->
    nani_event:add_handler(?MODULE, []).

delete_handler() ->
    nani_event:delete_handler(?MODULE, []).