# Nani
Nani will love and will hate you. It will be as dumb as possible and sometimes
it comes up with remarkable insights. And the best thing is: you start out with
nothing at all. Just a program. 

This guide will give a little more insight into Nani and it's architecture
because the readme is already getting way too long.

> Note that Nani is still in development so some components might not function 
> according to the way they are described in this document. Any discrepance 
> should be considered as a malfunction of the component and not this
> documention which should always take precedence when in doubt.

The `nani_bot` and in lesser degree the `nani_connection` might be the two most
important components to interact with. To be honest, you shouldn't have to deal
with the `nani_connection` process and the `nani_bot` should function as your
main *client* process. It *should* contain all the API necessary to do all the
things that you would want to do with a bot.

The `nani_bot` API is deliberately designed to be somewhat low level. Although 
there are of course the `say` and `emote` functions for convenience (more about 
those later). At the same time, it's also designed to be controlled explictly by
a human. This might sound strange for a bot but it can be really convenient when
testing and at the same time offers for some intersting possibilites. This is 
mostly due to the power of Erlang though.

# Learning
By default, Nani is designed to learn from basically scratch. Included is a
`markov_server` component with a `markov` library. You can either use the
included server or write your own one possibly utilizing the `markov` library.
However, Nani doesn't really care about what responder library you use.

By default though it will seed all the input it receives to the `markov_server`
component. This means that you can boot up the bot with a minimal seed, for
example:

    > markov_server:start().
    > markov_server:seed("I know nothing!").
    > nani_bot:start(Config).
    > ...

Nani will make use of the `markov_server` if it's available. This means you
can start to ramble and she'll gradually get better at making sense and
responding appropriately. Of course, mostly she'll be rambling though.

The best thing is: you can try out the `markov_server` without activating the
bot.

    > cd("directory/to/nani/src").
    > c(markov_server).
    > markov_server:start().
    > markov_server:seed("Hell yeah!").
    > markov_server:generate(5). % or any number of words you desire

The goal is to be able to play around with your response generators before
you hook them into the bot or even after that.

# Bootstrapping
TODO: Describe boostrapping all the bot components

# Interacting
TODO: Describe interacting with the `nani_bot` API.
