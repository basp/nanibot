-module(credits_server).

-behavior(gen_server).

-define(SERVER, ?MODULE).

%% API
-export([start/0, 
         start_link/0, 
         stop/0,
         deposit/2,
         withdraw/2,
         status/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(ACCOUNTS_TABLE, accounts).

-record(state, {accounts}).

%%%============================================================================
%%% API
%%%============================================================================
start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?SERVER).

deposit(Who, Amount) ->
    gen_server:call(?SERVER, {deposit, Who, Amount}).

withdraw(Who, Amount) ->
    gen_server:call(?SERVER, {withdraw, Who, Amount}).

status(Who) ->
    gen_server:call(?SERVER, {status, Who}).

%%%============================================================================
%%% gen_server callbacks
%%%============================================================================
init([]) ->
    Accounts = ets:new(?ACCOUNTS_TABLE, []),
    State = #state{accounts = Accounts},
    {ok, State}.

handle_call({status, Who}, _From, State) ->
    Accounts = State#state.accounts,
    case ets:lookup(Accounts, Who) of
        [] ->
            {reply, {Who, 0}, State};
        [{Who, Balance}] ->
            {reply, {Who, Balance}, State}
    end;

handle_call({deposit, Who, Amount}, _From, State) ->
    Accounts = State#state.accounts,
    case ets:lookup(Accounts, Who) of
        [] -> 
            Total = 0 + Amount,
            ets:insert(Accounts, {Who, Total}),
            {reply, {Who, Amount}, State};
        [{Who, Balance}] -> 
            Total = Balance + Amount,
            ets:insert(Accounts, {Who, Total}),
            {reply, {Who, Total}, State}
    end;

handle_call({withdraw, Who, Amount}, _From, State) ->
    Accounts = State#state.accounts,
    case ets:lookup(Accounts, Who) of
        [] ->
            Total = 0 - Amount,
            ets:insert(Accounts, {Who, Total}),
            {reply, {Who, -Amount}, State};
        [{Who, Balance}] ->
            Total = Balance - Amount,
            ets:insert(Accounts, {Who, Total}),
            {reply, {Who, Total}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

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