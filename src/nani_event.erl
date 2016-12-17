-module(nani_event).

-export([start/0, start_link/0, add_handler/2, delete_handler/2]).

-export([privmsg/4, 
         names/3,
         join/3,
         part/3,
         tcp_receive/1, 
         tcp_send/1, 
         tcp_error/1, 
         tcp_closed/1]).

-define(SERVER, ?MODULE).

start() ->
    gen_event:start({local, ?SERVER}).

start_link() ->
    gen_event:start_link({local, ?SERVER}).

add_handler(Handler, Args) ->
    gen_event:add_handler(?SERVER, Handler, Args).

delete_handler(Handler, Args) ->
    gen_event:delete_handler(?SERVER, Handler, Args).

join(Bot, Channel, User) ->
    gen_event:notify(?SERVER, {join, Bot, Channel, User}).

part(Bot, Channel, User) ->
    gen_event:notify(?SERVER, {part, Bot, Channel, User}).

privmsg(Bot, From, To, Text) ->
    gen_event:notify(?SERVER, {privmsg, Bot, From, To, Text}).

names(Bot, Channel, Names) ->
    gen_event:notify(?SERVER, {names, Bot, Channel, Names}).

tcp_receive(Data) ->
    gen_event:notify(?SERVER, {tcp_receive, Data}).

tcp_send(Data) ->
    gen_event:notify(?SERVER, {tcp_send, Data}).

tcp_error(Reason) ->
    gen_event:notify(?SERVER, {tcp_error, Reason}).

tcp_closed(Socket) ->
    gen_event:notify(?SERVER, {tcp_closed, Socket}).