# nanibot
An Erlang IRC bot wrapped up as an OTP application.

# overview
Nanibot is at heart a state machine implemented as a `gen_statem` behaviour. 
This will parse incoming IRC messages and emit events via `nani_event`
depending on which state it is in. All interesting stuff is mostly implemented
via `gen_event` handlers listening to the events emitted by `nani_event` or 
processes involved with or directly manipulating the `nani_bot` process itself.

The `nani_bot` process is (for now) the main client API to the bot. It allows 
for some basic utility commands such as `say`, `emote`, `join` and `part` but
also allows you to send any IRC command directly using the `send` API.

Note that the `nani_conn` process that manages the socket is currently directly 
linked to the `nani_bot` process.

# setup
Check the `nanibot.app` file and make sure the `host`, `port` and `nick` config
is correct. Startup the application:

    application:start(nanibot).

It's recommended to add the `nani_logger` handler so you can see what the bot
is doing while you connect. This will use `sasl` so we'll startup that first:

    application:start(sasl).

And then add the handler:

    nani_logger:add_handler().

We then connect according to the settings in the `nanibot.app` file:

    nani_bot:connect().

After you connected you can issue commands to the bot:

    nani_bot:join("##somechannel").
    nani_bot:say("Hi all!").
    nani_bot:emote("pets something").

Or attach `gen_event` handlers to respond to IRC messages that the bot 
receives.