# nanibot
An Erlang IRC bot wrapped up as an OTP application.

# overview
Nanibot is at heart a state machine implemented as a `gen_statem` behaviour in
the `nani_bot` module. This will parse incoming IRC messages and emit events 
via `nani_event` depending on which state it is in. All interesting stuff is 
mostly implemented via `gen_event` handlers listening to the events emitted 
by `nani_event` or processes involved with or directly manipulating the 
`nani_bot` process itself.

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

# commands
Nani can recognizes a command when a message is prefixed with a *bang* (`!`) 
character, for example:

    !frotz quux baz

Would be a (hypothetical) command `frotz` with arguments `quux` and `baz`.

Commands are implemented as standard `gen_event` handlers listening to events
emitted by `nani_event` so they work the same as any other handler. Command
handlers just listen for the `cmd` event and use the information supplied to
see if they can run a known command. For an example, take a look at 
`commands.erl` which has a few basic commands implemented.

One note of warning, when implementing handlers of any kind, be aware that the
capabilities of the Erlang VM probably vastly overshadow that of the IRC
connection. In other words, Erlang will happily pump pages of digits to your 
IRC connection if you don't put in a *floodgate* yourself (some examples of
these are also in `commands.erl`).

## format strings
If you wanna convert a number to binary use `!fmt ~.2b 12345`, if you want to
convert a number to hex use `!fmt ~.16b 12345` etc.

If you want to read a number from binary user `!fread ~2u 1010`, if you want to 
read a number from hex use `!fread ~16u DEADBEEF` etc.

# events
TODO: Document all the standard events.