-module(yahtzee).

-behaviour(gen_statem).

%% API
-export([start/0,
         start_link/0,
         stop/0,
         status/0]).

%% state functions
-export([standby/3,
         player_turn/3]).

%% gen_statem callbacks
-export([init/1,
         callback_mode/0,
         handle_event/4,
         handle_call/3,
         terminate/3,
         code_change/4]).

-record(state, {results}).

name() -> yahtzee.

%%%============================================================================
%%% API
%%%============================================================================
start() ->
    gen_statem:start({local, name()}, ?MODULE, [], []).

start_link() ->
    gen_statem:start({local, name()}, ?MODULE, [], []).

status() ->
    gen_statem:call(name(), status).

stop() ->
    gen_statem:stop(name()).

%%%============================================================================
%%% gen_statem callbacks
%%%============================================================================
init([]) -> 
    Data = #state{results = []},
    {ok, standby, Data}.

terminate(_Reason, _State, _Data) -> ok.

code_change(_OldVsn, State, Data, _Extra) -> {ok, State, Data}.

handle_call({call, From}, _Msg, Data) ->
    {keep_state, Data, [{reply, From, ok}]}.

handle_event(_EventType, _EventContent, _State, _Data) ->
    {keep_state_and_data, []}.

callback_mode() -> state_functions.

%%%============================================================================
%%% State functions
%%%============================================================================

%%%----------------------------------------------------------------------------
standby(cast, _Event, _Data) ->
    {keep_state_and_data, []};

standby({call, From}, status, Data) ->
    Reply = {ok, standby},
    {keep_state, Data, [{reply, From, Reply}]};

standby({call, From}, _Msg, Data) ->
    Reply = {ok, standby},
    {keep_state, Data, [{reply, From, Reply}]}.

%%%----------------------------------------------------------------------------
player_turn(cast, _Event, _Data) ->
    {keep_state_and_data, []}.

%%%============================================================================
%%% Internal functions
%%%============================================================================
