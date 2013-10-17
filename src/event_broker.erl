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
	{ok, []}.
 
handle_event({Topic, Event}, State) ->
	Callbacks = trie:get(Topic, State),
	lists:foreach(
		fun(Call) ->
			spawn(fun() -> Call(Event, Topic) end)
		end,
	Callbacks),
	{ok, State}.
 
handle_call(state, State) ->
	{ok, State, State};

handle_call({subscribe, Topic, Callback}, State) ->
	{ok, ok, trie:add(Topic, Callback, State)};

handle_call({unsubscribe, Topic, Callback}, State) ->
	{ok, ok, trie:remove(Topic, Callback, State)}.
 
handle_info(_, State) ->
	{ok, State}.
 
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
 
terminate(_Reason, _State) ->
	ok.