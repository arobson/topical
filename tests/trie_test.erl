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

-module(trie_test).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").


trie_test_() ->
	{setup,
		fun create_trie/0,
		fun (T) -> T end,
		fun (T) ->
			[
				?_assertEqual(true, trie:is_member("this.is.a.test", T)),
				?_assertEqual(["all", "blank is a blank", "ta-da"], trie:get("this.is.a.test", T)),
				?_assertEqual(["all", "one", "two"], trie:get("a.b.c", T)),
				?_assertEqual(["all"], trie:get("a.b", T))
			]
		end
	}.

contains_test() ->
	L = [1,2,3,4,5,6],
	L2 = lists:filter(trie:contains(3), L),
	?assertEqual([1,2,4,5,6],L2).

create_trie() ->
	T = [],
	T1 = trie:add("this.#.test", "ta-da", T),
	T2 = trie:add("a.b.c", "one", T1),
	T3 = trie:add("a.b.c", "two", T2),
	T4 = trie:add("a.b", "ignore", T3),
	T5 = trie:remove("a.b", "ignore", T4),
	T6 = trie:add("#", "all", T5),
	T7 = trie:add("*.is.a.*", "blank is a blank", T6),
	T7.
