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

And... Eventually you should get some notifications in your shell. 
The bot is connecting. Once it's ready you can tell it to join some channel:
```
> nani_bot:join("##somechannel").
```

Note that the bot is ready once it received the `RPL_WELCOME` message 
from the server. At this point you can send it other commands (see below).

When dealing with the bot in an interactive way (as we are doing)
in the Erlang shell, we have to watch for this event. When you register
middleware (plugins) this will be handled automatically and you can be sure
it will only run once the bot is really ready.

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

Or do more or less everything the IRC protocol supports using the `send` API:
```
> nani_bot:send("NICK Mebibot").
```

### markov text
You can generate random text using the `markov_server` process. 

#### Notes 
* The `markov_server` should be able to crash without impacting the bot.
* Currently it needs to be seeded with at least somehting *reasonable*;
where reasonable is something that results in at least one lookup 
(i.e. *bigram* and at least one candidate *token*).

First we start it up:
```
> markov_server:start().
```

Currently, the `markov_server` will crash if we try to `generate/1` something
before it's seeded. So let's do seed it then, there's a few API's:

* `seed` takes a string (the text to seed the generator with)
* `seed_file` takes a path to a text file
* `seed_dir` takes a path to a directory containing text files

```
> markov_server:seed_file("./chatlog.txt").
```

Note that seeding is additive. For example, you can grow the 
bot's markov potential in real-time by using the chat messages 
you receive and the `seed` function. You could opt to filter
out anything the bot's own messages or even decide to inlude
a percentage of them (this can work surprisingly well). 

Or you can increase the bot's vocabulary just by seeding it
more stuff while it's running using any of the API's and 
the Erlang shell. 

A fun thing to do is to seed the bot with a minimal amount of 
text (basically enough to generate at least one ngram) and have
it seed from the chat itself from there.

Note that there's no way to save the bots markov memory just yet 
(it's two ETS tables) although it should be trivial to implement. 
The bot is still in very early stages so for now it's convenient 
just to wipe memory on process exit. 

After we have seeded the server we are ready to generate some tokens:
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
* Finish and incorporate middle-ware (plugin) server
* Add proper supervisor tree(s)
* Split off markov_server to seperate app
* Complete proper OTP application(s)

# notes on the random text generation
This is just for those who are interested or wanna make sense
of the stuff in `markov.erl` (this includes me in a few months).

## it all starts with ngrams
The algorithm works with lists (or sequences) of tokens. What your
token is doesn't really matter. In this case we use strings. It
starts by converting tokens into so called *ngrams*. An `ngram`
is basically a tuple of tokens that appeared in that order in some
source of tokens.

The goal is to create something that can give us a random sequences
of tokens of a particular length in which the order of the tokens
is based on the likeleyhood they where found in some kind of 
source material (e.g. existing tokens).

Let's consider this sentence. In tokens it would like:
```
Tokens = ["let's", "consider", "this", "sentence"].
```

Note that we normalized whitespace, capitalization and most of 
the punctuation. Depending on your scenario, it's often a good
idea to sanitize your source somewhat before you use it to feed 
your markov generator.

Once we have a list of tokens (whatever they might be) we can use
this to create ngrams. Let's start with *bigrams* (ngrams of rank 2, 
e.g. normal tuples):
```
Bigrams = [
    {"let's", "consider"}, 
    {"consider", "this"}, 
    {"this", "sentence"}].
```

This is what `markov:bigrams/1` does. This is just a helper that
calls the more generic `markov:ngrams/2` function which can be used
to create ngrams up to rank 5.

Now we have the start of something interesting but we're not there
yet. The next is to use these bigrams in order to create a tuple
consisting of the bigram and a list of words that are likely to
follow it.

So what the algorithm does next is basically scan through the ngrams
and depending on whether it's a new `ngram()` or a known one, either
remember `{ngram(), [token()]}` or retrieve it, append `token()` to the list 
of known tokens and store it again.

In other words, what you're creating is a map from `ngram()` to `[token()]`.
Let's call this *map* (or dictionary) `memory`.

Note that this implementation is not efficient on memory as we are storing
tokens more than one time. This conveniently allows us to pick any random
one without any work. 

We could (for example) make it more efficient to pack up *L* into
a list `[{integer(), token()}]` tuples so that we can still perform a 
(random) lookup based on chance as well as store them in a more efficient 
manner.  

For now I kinda like the simplicity of the algorithm and to be honest,
the `memory` is not meant to grow *that* big at this point in development
so I don't wanna overload the bot with stuff that might be better implemented
when the design is more stable.

## generation of (random) tokens

Once you have such a map you're able to generate random stuff that's famous
for utterly nonsense most of the time even though it seems to make sense... 
Sometimes.

0. Our initial state is an empty list `[token()]` *S* and the `memory` dictionary
as described above. Additionaly we picked a key *K* from the known keys in `memory`.  
1. We get the value associated with *K* (which is, a `ngram()` tuple) from `memory`. 
This will give us a `{ngram(), Q = [token()]}`. That is, the key we looked for and 
a list of tokens.
2. We'll pick some `token()` from the list of tokens `[token()]` (*Q*).
3. We'll append this `token()` *T* to *S* (the list of selected tokens `[token()]`.
4. Now we need to combine *K* with *T* in some way that it produces a new key *K2*;
how to do this depends on the rank of ngram(s) your dealing with. For illustration
We'll focus on the bigram case. This assumes that *K* is a tuple `{token(), token()}`.
5. We combine *K* `{A, B}` with *T* so that we have a new tuple *K2* `{B, T}`. 
6. Repeat from step 1 substituting *K* with our new *K2* until we are satisfied with
the length of *S*.

Now we'll end up with a bunch of random tokens in *S* which we basically can just return, 
join and use as some jibberish.

## how it's stored internally
We're using a very simple setup of a table consiting tuples of tokes (ngrams) and a list
of tokens (candidates). It's a map of *K* `ngram()` to *V* `[token()]` where:

```
token() :: term() % basically anything your language can support
ngram() :: {token(), token()}
         | ...  
         | {token(), token(), token(), token(), token()}
```

You can deal with ngrams of a particalar rank only or mix and match if you want. Although
you will have to extend the algorithm which only is supported to deal with ngrams
of a uniform rank (and only bigrams too currently).

The `{Key :: ngram(), Value :: [token()]` values are basically stored as is. The key is
the `ngram()` and the value is the token list `[token()]`. However, we wanna lookup
random keys efficiently and scanning the table is highly undesirable so we'll use an 
additional index table. This is just an `index() :: integer()` and an `ngram()` key:
`{index :: integer(), ngram()}` where `index()` is our key. 

Now we just keep track of the number of keys in our runtime state (we need that anyway 
to generate new index numbers) and basicallly use that as our upper limit whenever we 
need to generate a new random key. Then we'll update the ngrams table and the index
table as necessary. Depending on whether we found an exisitng ngram or a new one when
updating the `memory`.