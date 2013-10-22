#Topical
It's topic routing event pub-sub. It wants to save you from creating tons of event handlers and help you decouple your processes.

## Why
Erlang gen_event is awesome. However, should you want the ability to send events to various components in the system, constantly creating/modding event handlers to forward events to different components becomes a soul-draining, DRY-violating chore.

Conditional event handling is also a bit odd. Pattern matching is great but has some limitations that I think AMQP style topic routing handles rather well.

## Alphaware-tastic!
Stuff that needs moar testing/improvements:
 * My trie could be one of the worst implementations ever.
 * If the processes dies, all subscriptions get lost.
 * The handler spawns a process for every subscriber long enough to publish.

Basically, it's worth trying. I plan on using it quite a bit, so in theory, it should get better. Did you know that every time you send a PR, I shed a single tear that turns to a precious gem when I then gift to a bunnicorn? It's true*.

## API

### A Note About Topics
Topics are '.' seperated namespaces. When publishing to a topic, you provide the full topic specifications (no wild cards).

When subscribing to a topic, you can use two types of wildcards.

 * '*' - matches 1 and exactly 1 namespace segment
 * '#' - matches 0 to many namespace segments

### Methods 
```erlang
topical:publish(Topic, Event).
```
Publish an event with a given topic.


```erlang
topical:subscribe(Topic, Callback).
```
Add the handler to the topic.


```erlang
topical:unsubscribe(Topic, Callback).
```
Remove the handler from the given topic.

### Callback Format
The callback is any function of the form
```erlang
fun(Event, Topic) -> end.
```

__Why the reversal of arguments??__: my experience with this style of internal pub-sub is that you rarely care about the topic so this puts the 'most significant' argument to the receiver first.

## Usage

Put the following into your rebar.config's [{deps,[]}] collection:
```erlang
	{topical, "",
		{git, "git://github.com/arobson/topical",
		{branch, "master" } } }
```

Put the following into your .app.src file's [{application,[]}] collection:
```erlang
{applications, [
		kernel,
		stdlib,
		%%% other stuff? %%%
		topical
	]},
```

If you use a Makefile like I do that will start your app in an interactive shell, don't forget to add a -s topical to the arg list so that it starts topical for you.

## Build & Test
 * make to build.
 * make eunit to run the trie unit tests
 * make ct to test whether the app starts and routes the event correctly.
 * make test to run both test suites.
 * make start will launch the application and a shell so you can play.

## How It Do
It's not magic. It's really just a single event manager and handler wrapped around a trie implementation. I wrote the trie, so it might be the worst thing ever.

## TO DO
 * Prevent lost subscriptions on process restarts.


*but really, that's not true. There's no such thing as crystal-tears.