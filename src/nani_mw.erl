-module(nani_mw).

-behaviour(gen_server).

%% API
-export([start/0, start_link/0, use/2, run/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(EVENTS, [join, names, message, pm]).

%%%============================================================================
%%% API
%%%============================================================================
start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

use(Event, Handler) ->
    gen_server:cast(?SERVER, {use, Event, Handler}).

run(Event, Args, Context) ->
    gen_server:call(?SERVER, {run, Event, Args, Context}).

%%%============================================================================
%%% gen_server callbacks
%%%============================================================================    
init([]) ->
    State = lists:map(fun (X) -> {X, queue:new()} end, ?EVENTS),
    {ok, State}.

handle_call({run, Event, Args, Context}, _From, State) ->
    Q = proplists:get_value(Event, State),
    Reply = run_queue(queue:to_list(Q), Args, Context, []),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({use, Event, Handler}, State) ->
    Q = proplists:get_value(Event, State),
    NewQueue = queue:in(Handler, Q),
    % remove the old queue... 
    proplists:delete(Event, State),
    % ...and replace it with the new one
    {noreply, [{Event, NewQueue} | State]};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%============================================================================
%%% Internal functions
%%%============================================================================
run_queue([], _Args, Context, Actions) -> 
    {Context, Actions}; 

run_queue([F | Rest], Args, Context, Actions) ->
    {NewContext, NewActions} = F(Args, Context),
    run_queue(Rest, Args, NewContext, NewActions ++ Actions).