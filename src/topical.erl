%%% @author Alex Robson
%%% @doc
%%%
%%% Convenience module for application control
%%% 
%%% @end
%%% Licensed under the MIT license - http://www.opensource.org/licenses/mit-license
%%% Created Oct 17, 2013

-module(topical).

-export([
		publish/2,
		start/0,
		stop/0,
		subscribe/2,
		unsubscribe/2
	]).

-define(BROKER, event_broker).
-define(HANDLER, event_broker).

%% ==================================================================
%%  API
%% ==================================================================

-spec publish(list(), term()) -> ok.
publish(Topic, Event) ->
	gen_event:notify(?BROKER, {Topic, Event}).

-spec start() -> ok.
start() ->
	application:load(topical),
	application:start(topical).

-spec stop() -> ok.
stop() ->
	application:stop(topical).

-spec subscribe(list(), term()) -> ok.
subscribe(Topic, Callback) ->
	gen_event:call(?BROKER, ?HANDLER, {subscribe, Topic, Callback}).

-spec unsubscribe(list(), term()) -> ok.
unsubscribe(Topic, Callback) ->
	gen_event:call(?BROKER, ?HANDLER, {unsubscribe, Topic, Callback}).