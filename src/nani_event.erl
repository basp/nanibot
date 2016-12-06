-module(nani_event).

-export([start_link/0, add_handler/2, delete_handler/2, privmsg/4]).

-define(SERVER, ?MODULE).

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