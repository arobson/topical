%%% @author Alex Robson
%%% @copyright 2013
%%% @doc
%%%
%%% Event broker using topic routing
%%%
%%% @end
%%% Licensed under the MIT license - http://www.opensource.org/licenses/mit-license
%%% Created October 16, 2013 by Alex Robson

-module(event_broker).

-behaviour(gen_event).

-export([
		start/0,
		stop/0
	]).

-export([
		init/1, 
		handle_event/2, 
		handle_call/2, 
		handle_info/2, 
		code_change/3,
		terminate/2
	]).

-record(state, {cache=dict:new(), trie=[]}).
 
%% ===================================================================
%%  API
%% ===================================================================
-spec start() -> pid().
start() ->
	Pid = gen_event:start_link({local, ?MODULE}),
	gen_event:add_handler(?MODULE, ?MODULE, []),
	Pid.

-spec stop() -> ok.
stop() -> gen_event:stop(?MODULE).


%% ===================================================================
%%  gen_event
%% ===================================================================
init([]) ->
	{ok, #state{}}.

%% Responsds to an event of the form 
%% {"topic.specification", { event } }
%% and uses the trie to retrieve a list of
%% subscribers to call with the event.
-spec handle_event(Topic::{list(), term()}, State::term()) ->{ok, term()}.
handle_event({Topic, Event}, #state{cache=Cache, trie=Trie}=State) ->
	Callbacks = case dict:find(Topic, Cache) of
		{ok, X} ->  X;
		_ -> trie:get(Topic, Trie)
	end,
	NewCache = dict:update(Topic, fun(_) -> Callbacks end, Callbacks, Cache),
	publish_events(Topic, Event, Callbacks),
	{ok, State#state{cache=NewCache}}.
 
handle_call(state, State) ->
	{ok, State, State};

handle_call({subscribe, Topic, Callback}, #state{trie=Trie}=_State) ->
	NewState=#state{trie=trie:add(Topic, Callback, Trie)},
	{ok, ok, NewState};

handle_call({unsubscribe, Topic, Callback}, #state{trie=Trie}=_State) ->
	NewState=#state{trie=trie:remove(Topic, Callback, Trie)},
	{ok, ok, NewState}.
 
handle_info(_, State) ->
	{ok, State}.
 
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
 
terminate(_Reason, _State) ->
	ok.

publish_events(Topic, Event, Callbacks) ->
	lists:foreach(
		fun(Call) ->
			% spawn(fun() -> Call(Event, Topic) end)
			Call(Event, Topic)
		end,
	Callbacks).