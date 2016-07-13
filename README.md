# nanibot
Erlang IRC bot.

## getting started
### the erlang shell
Make sure you have some kind of Erlang running. There's a specialized shell 
for Win Windows. Look for instructions in the documentation.

Once you have an Erlang shell up and running we can continue.

### compiling
There's no Rebar or something yet so we have toi do this the clunky way. Once
in your Erlang shell:
```
> cd("./dir/where/nani/is/installed/src").
```

After that we compile all modulels:
```
> lc([markov, markov_server, nani_utils, nani_conn, nani_bot]).
```

### starting
We need some `Config` such as:
```
> Config = [{host, "irc.freenode.net"}, {port, 6667}, {nick, "YourBotNick"}].
```

So we know how to connect to the IRC network. Note that in most cases you 
have to register your bot (nick) first.

Now we are ready to connect. First start the bot:
```
> nani_bot:start(Config).
```

And then tell the bot to connect:
```
> nani_bot:connect().
``` 

And... Nothing happens. You might see some output going across your screen, 
that's normal. The bot is connecting. Once it's ready you can tell it to join
some channel:
```
> nani_bot:join("##somechannel").
```

### doing stuff
As of yet, the bot doesn't do anything by itself. However you can do some
control runs using the API.

Once you have executed `nani_bot:join(Channel)` and after you received the
`NAMREPLY` message you can participate in the chat. You can use the API to
easy `say` and `emote` stuff but you can always use the low-level `send` 
API as well.
```
> nani_bot:say("##somechannel", "Hiya all!").
```

Or emote something:
```
> nani_bot:emote("##somechannel", "hops around nervously").
```

You can generate random text using the `markov_server` process. 
First we start it up:
```
> markov_server:start().
```

And then we generate some tokens:
```
% Generate 13 (or less if it can't find links) tokens of random text
Tokens = markov_server:generate(13).
```

A quick hint, we can join this easily using the `string:join` function:
```
Text = string:join(markov_server:generate(13), " ").
```

Just remember, the `generate` function returns tokens.

# TODO
* Add proper supervisor tree(s)
* Split off markov_server to seperate app
* Complete proper OTP application(s)