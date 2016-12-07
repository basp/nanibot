# nanibot
Erlang IRC bot.

## goal
> Nanibot was created with the explicit goal of modifying and growing the
> bot *while* it is running. I was happy working in Node-land and enjoying
> the `npm` ecosystem but it was frustrating seeing the bot err (or just make
> silly responses) and having to take it offline to do basic fixes.
>
> I looked into implementing a script language for it or even hot-loading
> for Node and although both are possible they didn't feel like the right 
> path to take. I felt like Erlang might be a good fit but after looking 
> into the ecosystem I felt a bit dishearted. To be honest, none of it was 
> to my liking so that meant I had to write the whole thing from scratch
> including a basic IRC client and markov library.
>
> Still, the thought of having hot-load capability and the other benefits
> (first class processes, functional programming, dynamic typing, OTP) 
> really convinced me to give it a try anyway. The result is Nanibot and
> so far I'm pleased on how things are falling together.

## overview
Nanibot is library but it also claims to be a bot, by virtue
of all the little pieces that are included. By design, the aim has always
been to be a library and definitely not a framework. As such it can
hopefully be useful in a variety of scenarios.

> Nanibot is *wannabe* OTP. It uses a lot of OTP stuff but the top level 
> is still unsupervised (except from the `markov_server` but more on that 
> later). 
>
> The main reason for this is that it was unclear on how the plugins (real 
> functionality) would be implemented. Now that `gen_event` came out on top 
> we should have a pretty stable surface layer.
> 
> We will be full OTP at some point though, that has always been the goal.

### nani
The `nani` modules are part of the core bot. When considering only those
modules even *bot* is an overstatement. Frankly it's a pretty basic Erlang
IRC client. The core of the bot is implemented in `nani_bot` as a `gen_statem`
behavior. Its basic job is to listen to the IRC socket and emit events
via the `nani_event` event emitter. Clients in the form of event handlers
and other processes using the bot can use the `nani_bot` interface to
interact with the underlying IRC connection in both high and reasonably
low level ways. 

TODO: The `nani_utils` module contains some helper functions which 
might not even be used anymore.

> The bot process should never ever crash. Nor should the *memory* or
> `markov_server`. At this point the only way to implement actual 
> functionality should be by listening to events emitted by `nani_event`.
> Those handlers should never ever crash or kill the bot unintendedly.

### markov
The `markov` modules are part of the markov-chain generation service.
This is a server that can generate random text and also *learn* on the fly.
You can dynamically *seed* it using any of the `seed` methods and it will
incorporate that text into its vocabulary. It uses ETS memory backed
tables as the default storage mechanism and as such is quite fast but your
bot might suffer from *amnesia* if the `markov_server` process dies.

Note that it would be trivial to use disk based tables but there is really
no need. The bot is pretty idempotent (and functional) in that when you seed
it with exactly the same data, you get exactly the same behavior (barring
any funky handlers you have registered of course but even they should not
be terribly hampared by a total failure of bot memory if you design them 
well). In other words - if you log the IRC data you can always bring the 
bot back to where it was (again, barring any funky stuff).

And else, you can just use ETS disk-based tables which is a trivial
change to implement in `markov_server` if you really need them (you don't).

> Nowadays the bot doesn't even care about the `markov_server` anymore 
> but at some point it really *depended* on it and would crash without
> it being available. That explains why currently it still is the only
> service with an actual proper supervisor (`markov_sup`) process.

The `markov` module is just a bunch of methods that combine nicely in order
to do procedural generation of tokens based on *ngrams*. It should be quite 
useful on it's own outside the `markov_server` process which uses it to 
generate responses. Note that the `markov` module is a pure functional
module. All persistence is handled by the `markov_server` and its internal ETS tables.

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

After that we compile the bot modules:
```
> lc([nani_utils, nani_conn, nani_event, nani_bot]).
```

This will allow you to run the bot with `nani_bot:start(Config)` (no 
worries, `Config` is explained later) but you probably want a bit 
more bits and pieces like for example the markov service:
```
> lc([markov, markov_server]).
```

You can start this one with `markov_server:start()` but don't forget 
to seed it as well. There's various ways to do this. Those are described
below or you can just check module info with `m(markov_server)` as the 
`seed` methods are very straightforward.

Finally, you might want the *plugins* which are just `gen_event` handlers:
```
> lc([greeter, markov_respond]).
```

All stuff is explained in more detail below but hopefully at least now
you can make a little bit of sense of the included modules, their importance
and where they fit into the big picture. Next, we'll look into actually
running the bot.

### starting
We need some `Config` such as:
```
> Config = [{host, "irc.freenode.net"}, {port, 6667}, {nick, "YourBotNick"}].
```

So we know how to connect to the IRC network. Note that you might have to register 
your bot (nick) first. This mostly depends on the network you're trying to connect
to.

Now we are ready to start the bot. Note that this only starts the process, the 
bot won't actually do anything yet. It's just waiting there for commands.
```
> nani_bot:start(Config).
```

Next we tell the bot to connect:
```
> nani_bot:connect().
``` 

This will prompt the bot to go ahead and try and make an actual connection
using the `Config` we gave earlier. After a bit it should give some (server
dependent) output on your console and the bot should return to idling.

> At this point the bot is connected and you can inspect this by running `regs().`
> in your console. Somewhere in that list should be a `nani_bot` as well as a 
> `nani_conn` process. If either one is missing then something went horribly wrong
> so please file a bug in that case.

Note that the bot is ready once it received the `RPL_WELCOME` message 
from the server. At this point you can send it other commands (see below).

> In code you don't usually have to worry about this as you are writing 
> event handler modules and registering them with `nani_event`. That means
> that your code will get executed as you expect and you don't have to worry
> about anything except implementing the desired behavior.

At this point the bot *should* be connected and ready to receive commands. You
could probably interact with it using `send` maybe but the most intersting thing
would be to try and join a channel:
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

# plugins
At first the idea of having a middleware pipeline seemed like a nice fit.
After all, it's what the Node bots use so why not here? Turns out it's 
not a very nice fit for Erlang after all. You can do it, it's not even
that hard and it works well enough but it just feels a bit clunky in the 
context of OTP. So `gen_event` seems like the most obvious choice for now.

If you wanna do something interesting with the bot you can subscribe
a module to the `nani_event` event emitter process. The major benefit is
that it is now incredibly easy to add stuff to the bot without worrying
that something might crash the whole thing. The downside is that now we
don't have a built-in way to have any handler stop other handlers from
executing (the equivalent of calling `done()` in callback land).

> I darenot say in what order event handlers are executed. I
> can only assume (and hope) that it's something logical like the order 
> in which they are registered. However, it's not something that you
> should depend upon anyway even if you are 100% sure about the order.
>
> If you need dependent event handlers (which is a valid use case) then
> you're encouraged to implement them in the way that makes most sense 
> to you in the spirit of Erlang/OTP.
>
> If you think of it from another way: what if a single handler could 
> block any other event handler from executing? Even if it was unrelated, 
> like a logging or debug handler? That would be a very bad thing. Hence, 
> Nanibot always sends all events to all registered handler modules and 
> pushes responsibility for any *bubbling* effects back to the client 
> (implementers of the handlers).

Note that it's usually a good idea to implement a catch-all clause in
for your `handle_event/2` implementation. This handler should do nothing
except pushback `{ok, State}` back to the client.

To start, take a look at either the `greeter` or `markov_respond` module.
Both of them are implemented as a handler for the `nani_event` process.
Everything is boilerplate mostly except for the `handle_event/2` function.

The `greeter` module is quite simple, it responds to the `names` event and
emits a basic greeting that is customized when there's only a single other
person in the channel. The `markov_respond` module uses the registered
`markov_server` service that can be used to generate responses. The markov
generator service is described in a seperate section.

> Note that even though `markov_server` and `nani_bot` do work well together,
> they are totally obvious about eachother and should remain so. The only way
> they should be related is via an `gen_event` module registed as a handler
> to the `nani_event` process.

You can see that from inside the handler, you can easily interact with the
bot using the `nani_bot` registered process. As mentioned before you can
easily use the `say/2` and `emote/2` functions for basic responses and for
more advanced (low level) stuff you can use `send/1` as well. Oh and there's
a `join/1` method to join channels (a `part/1` method is planned as well
because it just makes sense in the context of having a `join` method). 

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
sense at a glimpse) and hauntingly insightful and other times (when the stars 
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

## random notes
Anything below should be taken with a grain of salt. In fact you should always be
critical of anything you read but after re-reading the stuff below some of it 
doesn't make sense anymore and some of it might be hopeful thinking. And most of 
it just seems to be general rambling which doesn't even make sense to me anymore.

I'm keeping it for mostly amusement value and in the vain hope that there might be 
some useful insight in there after all.

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

## todo
* Add proper supervisor tree(s)
* Complete proper OTP application(s)
* Split off markov_server to seperate app (maybe)