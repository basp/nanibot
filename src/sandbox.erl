-module(sandbox).

-behaviour(gen_server).

%% API
-export([start/0, start_link/0, stop/0, msg/1, pm/1, run/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(EVENTS, [join, names, msg, pm]).

%%%============================================================================
%%% API
%%%============================================================================
start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?SERVER).

msg(Handler) ->
    gen_server:cast(?SERVER, {msg, Handler}).

pm(Handler) ->
    gen_server:cast(?SERVER, {pm, Handler}).

run(Event, Context) ->
    gen_server:call(?SERVER, {run, Event, Context}).

%%%============================================================================
%%% gen_server callbacks
%%%============================================================================    
init([]) ->
    Stacks = [{X, queue:new()} || X <- ?EVENTS],
    State = maps:from_list(Stacks),
    {ok, State}.

handle_call({run, Event, Context}, _From, State) ->
    _Nick = proplists:get_value(nick, Context),
    _Args = proplists:get_value(args, Context),
    Q = maps:get(Event, State),
    Reply = 
        try 
            run_queue(queue:to_list(Q), Context, [])
        catch
            Err -> {reply, Err, State}
        end, 
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({pm, _Handler}, State) ->
    {noreply, State};

handle_cast({msg, Handler}, S1) ->
    Q1 = maps:get(msg, S1),
    Q2 = queue:in(Handler, Q1),
    S2 = maps:put(msg, Q2, S1),
    {noreply, S2};

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
run_queue([], Context, Actions) -> 
    {Context, Actions};

run_queue([Handler | Rest], Context, Actions) ->
    {Flag, NewActions} = Handler(Context),
    case Flag of
        continue -> run_queue(Rest, Context, NewActions ++ Actions);
        _ -> {Context, NewActions ++ Actions}
    end. 
