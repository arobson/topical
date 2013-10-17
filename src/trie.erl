%%% @author Alex Robson
%%% @copyright 2013
%%% @doc
%%%
%%% A trie for topic routing that works like AMQP
%%%
%%% @end
%%% Licensed under the MIT license - http://www.opensource.org/licenses/mit-license
%%% Created October 16, 2013 by Alex Robson

-module(trie).

-export([
		add/3,
		get/2,
		is_member/2,
		remove/3
	]).

%% Hi, haters. 
%% That's right. I put this in here. I prefer this to cramming
%% tons of test code into my relatively clean module.
%% I've heard the arguments against, but tests IN the module =:= gross.
-ifdef(TEST). 
-compile([export_all]).
-endif.

-type trie() :: [{list(),{[term()],[trie()]}}].

%% ===================================================================
%%  API
%% ===================================================================

-spec add(list(), term(), trie()) -> trie().
add(Key, Value, []) ->
	add_leaf([], parts(Key), Value);
add(Key, Value, Trie) ->
	add_leaf(Trie, parts(Key), Value).

-spec get(list(), trie()) -> [term()] | [].
get(Pattern, Trie) ->
	find_leaf(Trie, parts(Pattern)).

-spec is_member(list(), trie()) -> true | false.
is_member(Key, Trie) ->
	case find_leaf(Trie, parts(Key)) of
		nomatch -> false;
		_ -> true
	end.

-spec remove(list(), term(), trie()) -> trie().
remove(Key, Value, Trie) ->
	remove_leaf(Trie, parts(Key), Value).

%% ===================================================================
%%  Internal functions
%% ===================================================================

add_leaf([], [Part], V) ->
	[{Part, {[V],[]}}];

add_leaf([], [Part|Parts], V) ->
	[{Part, {[], add_leaf([], Parts, V)}}];

add_leaf([{Part, {V, L}}|T], [Part], NV) ->
	[{Part, {[NV|V], L}}|T];

add_leaf([{Part, {V, []}}|T], [Part|Parts], NV) ->
	[{Part, {V, add_leaf([], Parts, NV)}}|T];

add_leaf([{Part, {V, L}}|T], [Part|Parts], NV) ->
	[{Part, {V, add_leaf(L, Parts, NV)}}|T];

add_leaf([H|T], Parts, V) ->
	[H|add_leaf(T, Parts, V)].

contains(X) -> fun(Y) -> Y =/= X end.

find_leaf(Trie, Parts) -> find_leaf(Trie, Parts, []).

find_leaf([], _, Matches) -> Matches;

find_leaf([{Part, {V, _}}|T], [Part], Matches) -> 
	find_leaf(T, [Part], Matches ++ V);

find_leaf([{"*", {V, _}}|T], [Part], Matches) -> 
	find_leaf(T, [Part], Matches ++ V);

find_leaf([{"#", {V, _}}|T], [Part], Matches) -> 
	find_leaf(T, [Part], Matches ++ V);

find_leaf([{Part, {_, []}}|T], [Part|_]=All, Matches) -> 
	find_leaf(T, All, Matches);

find_leaf([{"*", {_, L}}|T], [_|Parts]=All, Matches) -> 
	find_leaf(L, Parts, find_leaf(T, All, Matches));

find_leaf([{"#", {V, _}}|[{Part, {_, L}}|T]], [Part|Parts]=All, Matches) ->
	find_leaf(L, Parts, find_leaf(T, All, Matches ++ V));

find_leaf([{"#", {V, L}}|T], [_|_]=All, Matches) -> 
	find_leaf(L, All, find_leaf(T, All, Matches ++ V));

find_leaf([{Part, {_, L}}|T], [Part|Parts]=All, Matches) -> 
	find_leaf(L, Parts, find_leaf(T, All, Matches));

find_leaf([_|T], Parts, Matches) -> find_leaf(T, Parts, Matches).

parts(Key) -> string:tokens(Key, ".").

remove_leaf([], _, _) -> [];

remove_leaf([{Part, {V, L}}|T], [Part], RV) ->
	NV = lists:filter(contains(RV), V),
	[{Part, {NV, L}}|T];

remove_leaf([{_, {_, []}}|_]=L, [_|_], _) -> L;

remove_leaf([{Part, {V, L}}|T], [Part|Parts], NV) ->
	[{Part, {V, remove_leaf(L, Parts, NV)}}|T];

remove_leaf([H|T], Parts, V) ->
	[H|remove_leaf(T, Parts, V)].
