# topical

An Erlang/OTP application providing AMQP-style topic-based pub/sub using gen_event. Decouples process communication without the boilerplate of hand-rolled gen_event handlers. The Erlang sibling of `topic-dispatch`.

## Mental Model

A single gen_event manager wraps a trie for pattern matching. `publish/2` routes events to all matching subscribers. Subscriptions survive only as long as the process — no persistence across restarts.

## Pattern Matching

Topics are dot-separated atoms: `'order.created'`, `'user.profile.updated'`.

| Pattern | Matches |
|---|---|
| `'order.created'` | exactly `'order.created'` |
| `'order.*'` | any one-segment suffix: `'order.created'`, `'order.deleted'` |
| `'user.#'` | any suffix (zero or more segments): `'user'`, `'user.profile'`, `'user.profile.updated'` |

## Quick Start

Add to `rebar.config` deps:
```erlang
{topical, "", {git, "git://github.com/arobson/topical", {branch, "master"}}}
```

Add to your `.app.src` applications list:
```erlang
{applications, [kernel, stdlib, topical]}
```

Start topical (or add `-s topical` to your shell args in a Makefile):
```erlang
application:start(topical).
```

Subscribe and publish:
```erlang
% Subscribe — Callback is fun(Event, Topic) -> end
topical:subscribe('order.*', fun(Event, Topic) ->
    io:format("Got ~p on ~p~n", [Event, Topic])
end).

% Publish — Topic must be the full, exact topic (no wildcards)
topical:publish('order.created', #{id => 42, amount => 100}).

% Unsubscribe
topical:unsubscribe('order.*', CallbackFun).
```

## API

```erlang
topical:publish(Topic, Event)      % Publish event to all matching handlers
topical:subscribe(Pattern, Fun)    % Register handler; Fun :: fun(Event, Topic) -> any()
topical:unsubscribe(Pattern, Fun)  % Remove a specific handler
```

Note the argument order in the callback: `fun(Event, Topic)` — Event first, because you usually don't care about the topic in the handler body.

## Build & Test

```bash
make           # compile
make eunit     # unit tests (trie)
make ct        # integration tests (app start + routing)
make test      # both
make start     # interactive shell with topical running
```

## Known Limitations

- Subscriptions are lost if the topical process or application crashes — no persistence.
- The trie implementation is not performance-optimized for high-churn scenarios.
- The handler process is spawned per-publish per-subscriber (short-lived process per delivery).
