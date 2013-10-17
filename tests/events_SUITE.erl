%%% @author Alex Robson
%%% @copyright 2013
%%% @doc
%%%
%%% Working with JSON in Erlang hurts. Jiffy makes that a bit easier.
%%% This only exists to make it simpler to use Jiffy.
%%%
%%% @end
%%% Licensed under the MIT license - http://www.opensource.org/licenses/mit-license
%%% Created October 16, 2013 by Alex Robson

-module(events_SUITE).
-compile([export_all]).
-include_lib("common_test/include/ct.hrl").

all() -> [test_publish].

init_per_suite(Config) ->
	topical:start(),
	Me = self(),
	topical:subscribe("#", fun(E,T) -> Me ! {event, E, T} end),
	Config.

end_per_suite(_) -> ok.

test_publish(_) ->
	topical:publish("this.is.a.test", "hello world!"),
	receive
		{event, "this.is.a.test", "hello world!"} -> {comment, success}
	after 
		500 ->
			failey_fail
	end.