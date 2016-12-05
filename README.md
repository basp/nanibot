# nanibot
Erlang IRC bot.

## getting started
### the erlang shell
The instructions below assume you are running an Erlang shell. If you're 
unsure how to do this on your system please checkout [the official site](https://www.erlang.org).

Once you have an Erlang shell up and running we can continue.

### compiling
There's no Rebar or something yet so we have to do this the clunky way (sorry). 

Once in your Erlang shell:
```
> cd("./dir/where/nani/is/installed/src").
```

After that we compile all modulels:
```
> lc([markov, markov_server, nani_utils, nani_conn, nani_bot]).
```

#### note
There might be a missing (required) module in the list above. Usually, while I'm working
on the bot or related I just have it running and pickup the changes as I go. This usually
does result in the bot crashing whenever I can't be bothered to update the `code_change` 
callback but that's not so frequently and most of the development is in middleware nowadays
which should not have an impact on the bot server process anyways.

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

#### notes 
* The `markov_server` should be able to crash without impacting the bot.
* This also means the bot should function without the `markov_server`.
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

Seeding is **additive**. For example, you can grow the 
bot's markov potential in real-time by using the chat messages 
you receive and the `seed` function. You could opt to filter
out anything the bot's own messages or even decide to inlude
a percentage of them (this can work surprisingly well). 

By **default**, the bot *is* [seeding itself](https://github.com/basp/nanibot/blob/master/src/nani_bot.erl#L148)
with incoming chat messages.

Or you can increase the bot's vocabulary just by seeding it
more stuff while it's running using any of the API's. Manually
using the Erlang shell or via any other means. 

A fun thing to do is to seed the bot with a minimal amount of 
text (basically enough to generate at least one ngram) and have
it seed from the chat itself from there.

Note that there's no API to save the bot's markov memory just yet 
(it's two ETS tables) although it should be trivial to implement
(just convert to DETS :)). 
The bot is still in very early stages so for now it's convenient 
just to wipe memory on process exit. 

After we have seeded the server we are ready to generate some tokens:
```
% Generate 13 (or less if it can't find links) tokens of random text
Tokens = markov_server:generate(13).
```

A quick hint, we can join this easily using the `string:join/2` function:
```
Text = string:join(markov_server:generate(13), " ").
```

And another tip, in you can easily define a random response function as 
well, just make sure the `markov_server` is running and seeded (see below).
```
Response = fun(Len) -> string:join(markov_server:generate(Len), " ") end.
```

Just remember, the `generate/1` function returns tokens.

# todo
* Finish and incorporate middle-ware (plugin) server
* Add proper supervisor tree(s)
* Complete proper OTP application(s)
* Split off markov_server to seperate app (maybe)

# plugins
There's a seperate `sandbox` process that is responsible for running
any middleware. This is better than hacking it onto the bot itself.

The idea is that only the middleware server will crash (at worst) if
something goes wrong and not the whole bot.
```
> sandbox:start(), Respond = fun respond:handler/1, sandbox:msg(Respond).
``` 

This will register a random answering handler from the `respond` module.
There's also a `commands` module that has a basic `info` command:
```
> Info = fun commands:info/1, sandbox:msg(Info).
```

This will output some information on the vocab memory whenever someone 
types `!info`.

# notes on the random text generation
This is just for those who are interested or wanna make sense
of the stuff in `markov.erl` (this includes me in a few months).

## it all starts with ngrams
The algorithm works with lists (or sequences) of tokens. What your
token is doesn't really matter. In this case we use strings. It
starts by converting tokens into so called *ngrams*. An `ngram`
is basically a tuple of tokens that appeared in that order in some
source of tokens.

The goal is to create something that can give us a random sequence
of tokens of a particular length in which the order of the tokens
is based on the likeleyhood they where found in some kind of 
source material (e.g. existing tokens).

Let's consider this sentence. In tokens it would like:
```
Tokens = ["let's", "consider", "this", "sentence"].
```

We normalized whitespace, capitalization and most of 
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
yet. Next we need to use these bigrams in order to create a tuple
consisting of the bigram and a list of words that are likely to
follow it.

So what the algorithm does next is basically scan through the ngrams
and depending on whether it's a new `ngram()` or a known one, either
remember `{ngram(), [token()]}` or retrieve it, append `token()` to the list 
of known tokens and store it again.

In other words, what you're creating is a map from `ngram()` to `[token()]`.
Let's call this *map* (or dictionary) `memory`.

This implementation is not efficient on memory as we are storing
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

Concluding, even when seeding the bot with a substantial amount of text
the actual memory required by the `memory` ETS tables is quite low. At least
compared to everything else you're running.

## generation of (random) tokens

Once you have such a map you're able to generate random stuff that's famous
for being utterly nonsense most of the time (even though it seems to make 
sense at a glimpse) and hauntingly insightful and some times (when the stars 
align).

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

Below is the code in *pseudo* Erlang corresponding to the steps mentioned above:
```
S = [].                                 % 1
K = {A, B} = memory:get_random_key().   % 1, `bigram' case
{K, Q} = memory:get(K).                 % 2
T = utils:random_element(Q).            % 3
S2 = [T | S].                           % 4/5
K2 = {B, T}.                            % 5/6

% Functionally, we would recurse with `K2` and accumulator `S2`.
% Imperatively we can say that `K <- K2` and `S <- S2`.
```

## how it's stored internally
We're using a very simple setup of a table consiting tuples of tokes (ngrams) and a list
of tokens (candidates). It's a map of *K* `ngram()` to *V* `[token()]` where:

```
token() :: term(). % basically anything your language can support

ngram() :: {token(), token()}
         | ...  
         | {token(), token(), token(), token(), token()}. % ngrams!

% a key (ngram) and a list of candidate following tokens
entry() :: {ngram, [token()]}.

```

You can deal with ngrams of a particalar rank only or mix and match if you want. Although
you will have to extend the algorithm which only is supported to deal with ngrams
of a uniform rank (and only bigrams too currently).

The `{Key :: ngram(), Value :: [token()]` values are basically stored as is. The key is
the `ngram()` and the value is the candidate list `[token()]`. However, we wanna lookup
random keys efficiently and **scanning** the table is **undesirable** so we'll use an 
additional index table. This is just an `index() :: integer()` key and an `ngram()` value:
`{index :: integer(), ngram()}`.  

Now we just keep track of the number of keys in our runtime state (we need that anyway 
to generate new index numbers) and basicallly use that as our upper limit whenever we 
need to generate a new random key. Then we'll update the ngrams table and the index
table as necessary. Depending on whether we found an exisitng ngram or a new one when
updating the `memory`. 

Now we can just roll any kind of number between `StartIndex` and `NextIndex` and we
fetch any key from `memory` at *O(1)* speed. No scanning.

## about `memory`
If we have been a bit opaque about how memory itself is implemented that is
because it doesn't really matter. In fact, it might even be better of as pair of 
functions. Below is the required interface for any `memory` substitute.
```
remember(Key :: ngram(), Candidate :: token()) -> ignored.
retrieve(Key :: ngram()) -> Candidates :: [token()]. 
```

#### notes
* This API will probably be formalized in the next (0.0.2) version.
* Having `ignored` feels like a bit of a cop out but I kinda like it for stuff
that is really a **command** and not a **query**. In other words, commands should
return `void`, `ignored`, `0` whatever and mutate some kind of state. The point is, 
their return value is not that interesting. And queries should return something 
useful. This might sound logical (or illogical depending where you're coming from) 
but a lot of people still get it wrong. However if you're reading this you probably 
know a thing or two about functional programming so hopefully you know better.

In any case, this concept is deeply ingrained in the **OTP** framework itself in the
the form of `handle_call/3` (a query) and `handle_cast/2` (a command).

Check out [Martin Fowler](http://martinfowler.com/bliki/CQRS.html) and his wise words. 

## random stuff
Remember you do func assignment in the shell, I usually have something like:
```
> R = fun (Count) -> 
            Msg = string:join(markov_server:generate(Count), " "),
            nani_bot:say("##somechan", Msg) 
        end.
```

And then I can just go `R(13)` to spew some random dribble on some kind of 
channel. You could parametrize that as well if you're really 
[fansy](http://www.notaddicted.com/fansythefamous.php).