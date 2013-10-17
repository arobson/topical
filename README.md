#Topical

## Why
Erlang gen_event is awesome. However, should you want the ability to send events to various components in the system, constantly creating/modding event handlers to forward events all over feels rather gross to me.

Aside from the tight coupling, it can also be a pain should you want to conditionally handle events based on variants of the event. Yes. Pattern matching is great. Pattern matching has some limitations though and topical routing (like AMQP has) is pretty fantastic.

## Alphware-tastic!
It has more tests than some of the crap I've written. However, my guess is that my trie could be one of the worst implementations of a trie ever conceived. This *could* have a direct impact on both memory and processor. Also, if the event manager or handler processes die, all subscriptions get lost. I'll fix that eventually.

## API

### A Note About Topics
Topics are '.' seperated namespaces. When publishing to a topic, you provide the full topic specifications (no wild cards).

When subscribing to a topic, you can use two types of wildcards.

 * '*' - matches 1 and exactly 1 namespace segment
 * '#' - matches 0 to many namespace segments

### Methods 
'''erlang
topical:publish(Topic, Event).
'''
Publish an event with a given topic.


'''erlang
topical:subscribe(Topic, Callback).
'''
Add the handler to the topic.


'''erlang
topical:unsubscribe(Topic, Callback).
'''
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
	{json_erl_jones, "",
		{git, "git://github.com/arobson/json_erl_jones",
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
make to build.

make eunit to run the trie unit tests

make ct to test whether the app creates everything and routes the event correctly.

make test to run both test suites.

make start will launch the application and a shell so you can play. The API is pretty simple.

## How It Do
It's not magic. It's really just a single event manager and handler wrapped around a trie implementation. I wrote the trie, so it might be the worst thing ever.

## TO DO
 * Prevent lost subscriptions on process restarts.