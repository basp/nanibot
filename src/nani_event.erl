-module(nani_event).

-export([start/0, start_link/0, add_handler/2, delete_handler/2]).

-export([privmsg/4, names/3, received/Data]).

-define(SERVER, ?MODULE).

start() ->
    gen_event:start({local, ?SERVER}).

start_link() ->
    gen_event:start_link({local, ?SERVER}).

add_handler(Handler, Args) ->
    gen_event:add_handler(?SERVER, Handler, Args).

delete_handler(Handler, Args) ->
    gen_event:delete_handler(?SERVER, Handler, Args).

privmsg(Nick, From, To, Text) ->
    gen_event:notify(?SERVER, {privmsg, Nick, From, To, Text}).

names(Nick, Channel, Names) ->
    gen_event:notify(?SERVER, {names, Nick, Channel, Names}).

receive(Data) ->
    gen_event:notify(?SERVER, {receive, Data}).

send(Data) ->
    gen_event:notify(?SERVER, {send, Data}).

tcp_error(Reason) ->
    gen_event:notify(?SERVER, {tcp_error, Reason}).

tcp_closed(Socket) ->
    gen_event:notify(?SERVER, {tcp_closed, Socket}).