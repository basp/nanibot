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
> Config = [{host, "irc.freenode.net"}, {port, 6667}, {nick, "YourBotNick"}].
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

### Doing stuff
Once you're connected to a channel you might wanna say some stuff:
```
> nani_bot:say("##somechannel", "You guys are fab!").
```

You might also wanna emote something with the bot:
```
> nani_bot:emote("##somechannell", "dances across the room")).
```

The above will turn out into an **action** on the IRC chat:
```
Bot dances acress the room
```

You can also send raw commands with the  `send` function. 
This will change your name to `Foobot`.
```
> nani_bot:send("NICK Foobot").
```

## Markov Server
There's a tiny Markov text chain server. You can get markov by asking:
```
> markov_server:generate(20).
```

And this will generate `20` words of Markov-chain-based text. 
```
> markov_server:seed_file("./path/to/file")).
```

That should at least give it something to work with. Or you can just seed
it with a string:
```
> markov_server:seed("foo bar quux, random stuff").
```

Note that you can just feed it while it's running. I recommend feeding it 
the output of the channels its participating in. Just seed all the 
incoming `PRIVMSG` stuff into `markoV_server:seed/1`.

# Notes
* If you don't have any source, I recomment The Tales Of Grimm. Just seed 
a few of those,  there's some in the repo as well.
* You probably have to `seed` the server first before it can actually
serve you something.
