# nanibot
Erlang IRC bot.

## Getting started
### The Erlang Shell
Make sure you have some kind of Erlang running. There's a specialized shell 
for Win Windows. Look for instructions in the documentation.

Once you have an Erlang shell up and running we can continue.

### Compiling
There's no Rebar or something yet so we have toi do this the clunky way. Once
in your Erlang shell:
```
> cd("./dir/where/nani/is/installed").
```

After that we compile all modulels:
```
> lc([markov, markov_server, nani_conn, nani_botr]).
```

### Starting
We need some `Config` such as:
```
> Config = {Host = "irc.freenode.net", Port = 6667, Nick = "YourBotNick"}
```

Now we are ready to connect. First start the bot:
```
> nani_bot:start_link(Config).
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

There's some more API commands but I'm too lazy to document them now. Check 
the source.