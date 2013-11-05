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
-on_load(init_ets/0).
-export([
		add/2,
		add/3,
		get/1,
		get/2,
		init_ets/0,
		is_member/2,
		remove/2,
		remove/3
	]).

%% Hi, haters. 
%% That's right. I put this in here. I prefer this to cramming
%% tons of test code into my relatively clean module.
%% I've heard the arguments against, but tests IN the module =:= gross.
-ifdef(TEST). 
-compile([export_all]).
-endif.

%% First part is the key (topic/pattern segment),
%% Second part is a tuple of a list of Values attached
%% to this particular node and the child nodes.
-type trie() :: [{list(),{[term()],[trie()]}}].

%% ===================================================================
%%  API
%% ===================================================================

%% Add a key to the trie stored in ETS.
-spec add(Pattern::list(), Value::term()) -> ok.
add(Pattern, Value) ->
	Trie = get_trie(),
	NewTrie=add(Pattern, Value, Trie),
	store_trie(NewTrie),
	NewTrie.

%% Add key to the trie. Pattern is expected to be a string of the form:
%%  "#"
%%  "*"
%%  "this.#.test"
-spec add(Pattern::list(), Value::term(), trie()) -> trie().
add(Pattern, Value, []) ->
	add_leaf([], get_segments(Pattern), Value);
add(Pattern, Value, Trie) ->
	add_leaf(Trie, get_segments(Pattern), Value).

%% Return a list of all values found that contain a
%% subscription matching the Topic
-spec get(Topic::list()) -> [term()] | [].
get(Topic) -> get(Topic, get_trie()).

%% Return a list of all values found that contain a
%% subscription matching the Topic
-spec get(Topic::list(), Trie::trie()) -> [term()] | [].
get(Topic, Trie) ->
	case get_matches(Topic) of
		[] -> 
			Matches = lists:usort(find_leaf(Trie, get_segments(Topic))),
			write_matches(Topic, Matches),
			Matches;
		X -> X
	end.

%% Determine if Pattern presently exists in Trie
-spec is_member(Pattern::list(), Trie::trie()) -> true | false.
is_member(Pattern, Trie) ->
	case find_leaf(Trie, get_segments(Pattern)) of
		[] -> false;
		_ -> true
	end.

%% Removes the Pattern and Value from the Trie if they exist.
-spec remove(Pattern::list(), Value::term()) -> trie().
remove(Pattern, Value) ->
	Trie = get_trie(),
	NewTrie=remove(Pattern, Value, Trie),
	store_trie(NewTrie),
	NewTrie.

%% Removes the Pattern and Value from the Trie if they exist.
-spec remove(Pattern::list(), Value::term(), Trie::trie()) -> trie().
remove(Pattern, Value, Trie) ->
	remove_leaf(Trie, segment(Pattern), Value).

%% ===================================================================
%%  Internal functions
%% ===================================================================

%% No more nodes to check at this level of the trie, 
%% create a new node and return it.
add_leaf([], [Segment], NewValue) ->
	[{Segment, {[NewValue],[]}}];

%% No more nodes to check, but there are additional
%% segments to create. 
add_leaf([], [Segment|Segments], NewValue) ->
	[{Segment, {[], add_leaf([], Segments, NewValue)}}];

%% Matching node found on the last segment,
%% add the new value to this node and return.
add_leaf([{Segment, {Values, Children}}|T], [Segment], NewValue) ->
	[{Segment, {[NewValue|Values], Children}}|T];

%% Matching node found with additional segments,
%% to create.
add_leaf([{Segment, {Values, []}}|T], [Segment|Segments], NewValue) ->
	[{Segment, {Values, add_leaf([], Segments, NewValue)}}|T];


add_leaf([{Segment, {Values, Children}}|T], [Segment|Segments], NewValue) ->
	[{Segment, {Values, add_leaf(Children, Segments, NewValue)}}|T];

add_leaf([H|T], Segments, Values) ->
	[H|add_leaf(T, Segments, Values)].

contains(X) -> fun(Y) -> Y =/= X end.

%% Determine wether or not any children of a '#' node
%% match the current segment. This is used so that the
%% patterns that don't end in '#' will still be correctly matched.
find_child(_,[]) -> nomatch;
find_child(Segment, [{Segment, {_,_}}=Child|_]) -> Child;
find_child(Segment, [_|T]) -> find_child(Segment, T).

%% Convenience method; provides empty match list to start.
find_leaf(Trie, Segments) -> find_leaf(Trie, Segments, []).

%% At the end of the trie. Return any found matches.
find_leaf([], _, Matches) -> Matches;

%% At the end of the segments provided in the published topic.
%% Returns any values associated with this trie node since it
%% matches the last segment of the published topic.
%% Continues checking the remainder of siblings at this level.
find_leaf([{Segment, {Values, _}}|T], [Segment], Matches) -> 
	find_leaf(T, [Segment], Matches ++ Values);

%% At the end of the segments provided in the published topic.
%% Returns any values associated with this trie node since it
%% matches the last segment of the published topic.
%% Continues checking the remainder of siblings at this level.
find_leaf([{"*", {Values, _}}|T], [Segment], Matches) -> 
	find_leaf(T, [Segment], Matches ++ Values);

%% At the end of the segments provided in the published topic.
%% Returns any values associated with this trie node and also
%% includes values from any child node that matches the end
%% segment.
%% Continues checking the remainder of siblings at this level.
find_leaf([{"#", {Values, Children}}|T], [Segment], Matches) -> 
	find_leaf(T, [Segment], find_leaf(Children, [Segment], Matches ++ Values));

%% Node matches the current segment exactly but has no child nodes.
%% Continue checking siblings for '#' or '*' nodes.
find_leaf([{Segment, {_, []}}|T], [Segment|Segments], Matches) ->
	find_leaf(T, Segments, Matches);

%% Node matches one segment. Continue checking its children against
%% remaining segments and check topic against siblings.
find_leaf([{"*", {_, Children}}|T], [_|Segments]=All, Matches) -> 
	find_leaf(Children, Segments, find_leaf(T, All, Matches));

%% Node matches one or more segments. Continue checking its children against
%% remaining segments and check topic against siblings.
%% If its next child matches the current topic, move to the next node.
%% Otherwise, continue using this node to match against remaining segments.
find_leaf([{"#", {Values, Children}}|T]=Trie, [Segment|Segments]=All, Matches) -> 
	case find_child(Segment, Children) of
		nomatch -> find_leaf(Trie, Segments, find_leaf(T, All, Matches ++ Values));
		Child -> find_leaf(Child, All, find_leaf(T, All, Matches ++ Values))
	end;

%% Node matches the current segment exactly. Continue checking its children
%% and siblings for matches.
find_leaf([{Segment, {_, Children}}|T], [Segment|Segments]=All, Matches) -> 
	find_leaf(Children, Segments, find_leaf(T, All, Matches));

%% Node does not match. Continue checking siblings.
find_leaf([{_,_}|T], Segments, Matches) ->
	find_leaf(T, Segments, Matches).

remove_leaf([], _, _) -> [];

remove_leaf([{Segment, {Values, Children}}|T], [Segment], Remove) ->
	NewValues = lists:filter(contains(Remove), Values),
	[{Segment, {NewValues, Children}}|T];

remove_leaf([{_, {_, []}}|_]=Children, [_|_], _) -> Children;

remove_leaf([{Segment, {Values, Children}}|T], [Segment|Segments], NewValues) ->
	[{Segment, {Values, remove_leaf(Children, Segments, NewValues)}}|T];

remove_leaf([H|T], Segments, Values) ->
	[H|remove_leaf(T, Segments, Values)].

segment(Key) -> string:tokens(Key, ".").

%% ===================================================================
%%  ETS
%% ===================================================================

init_ets() ->
	ets:new(trie, [ordered_set, public, named_table, {read_concurrency, true}]),
	ok.

get_matches(Topic) ->
	case ets:lookup(trie, {matches, Topic}) of
		[] -> [];
		[{{_,_}, Matches}] -> Matches
	end.

get_segments(Topic) ->
	case ets:lookup(trie, {segments, Topic}) of
		[] -> 
			Segments = segment(Topic),
			ets:insert(trie, {{segments, Topic}, Segments}),
			Segments;
		[{{_,_},X}] -> X
	end.

get_trie() ->
	case ets:lookup(trie, trie) of
		[] -> [];
		[{trie,Trie}] -> Trie
	end.

store_trie(Trie) ->
	ets:insert(trie, {trie, Trie}).

write_matches(Topic, Matches) ->
	ets:insert(trie, {{matches, Topic}, Matches}).