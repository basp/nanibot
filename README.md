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
except pushback `{ok, State}` to the client.

To start, take a look at either the `greeter` or `markov_respond` module.
Both of them are implemented as a handler for the `nani_event` process.
Everything is boilerplate mostly except for the `handle_event/2` function.

The `greeter` module is quite simple, it responds to the `names` event and
emits a basic greeting that is customized when there's only a single other
person in the channel. The `markov_respond` module uses the registered
`markov_server` service that can be used to generate responses. The markov
generator service is described in a seperate section.

> Note that even though `markov_server` and `nani_bot` do work well together,
> they are totally oblivious about eachother and should remain so. The only way
> they should be related is via an `gen_event` module registed as a handler
> to the `nani_event` process.

You can see that from inside the handler, you can easily interact with the
bot using the `nani_bot` registered process. As mentioned before you can
simply use the `say/2` and `emote/2` functions for basic responses and for
more advanced (low level) stuff you can use `send/1`. Oh and there's
a `join/1` method to join channels (a `part/1` method is planned
because it just makes sense in the context of having a `join` method). 

If you want to interact with the markov server you can easily do that 
by using the API exposed by the `markov_server` process.

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

## maintanance
If you execute `ps -aux` you will see a list of all processes. Increase your shell size and look for some program that has a invocation that contains `botname@yourhost`. This is the Erlang process hosting the bot.

We can connect to this bot as follows:

        erl -remsh nani@zookeepers -sname dev@zookeepers

Once in there, check the processes with `regs()`.

Usually you want to restart the bot application:

        application:stop(nanibot).
        application:start(nanibot).

Keep in mind that you *will* have to add any handlers that you want to use. Most likely at least the `greeter` and `markov_respond`. The latter is found in the `nanikov` application directory.

# events
TODO: Document all the standard events.
