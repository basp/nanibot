-module(greeter).

-behaviour(gen_server).

%% API
-export([start/0, start/1, 
         start_link/0, start_link/1, 
         stop/0, add/1, get/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(DEFAULT_GREETINGS, [
    "hey",
    "hello",
    "hi",
    "howdy",
    "bonjour",
    "good day",
    "aloha",
    "yo",
    "namaste",
    "howdy-do",
    "cheerio",
    "g'day",
    "good day",
    "sup",
    "salute"
]).

%%%============================================================================
%%% API
%%%============================================================================
start() -> start(?DEFAULT_GREETINGS).

start(Greetings) -> 
    gen_server:start({local, ?SERVER}, ?MODULE, Greetings, []).

start_link() -> start_link(?DEFAULT_GREETINGS).

start_link(Greetings) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Greetings, []).

stop() ->
    gen_server:stop(?SERVER).

add(Greeting) -> 
    gen_server:cast(?SERVER, {add, Greeting}).

get() -> 
    gen_server:call(?SERVER, get).

%%%============================================================================
%%% gen_server callbacks
%%%============================================================================
init(State) ->
    {ok, State}.

handle_call(get, _From, State) ->
    Index = rand:uniform(length(State)),
    Reply = lists:nth(Index, State),
    {reply, Reply, State}.

handle_cast({add, Greeting}, State) ->
    {noreply, [Greeting | State]}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.