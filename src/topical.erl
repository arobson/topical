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

%% Publishes an event to a given topic so that all
%% matching subscriptions receive the message.
%% Topic takes the form of a '.' separated namespace:
%% - "example.topic.value"
-spec publish(Topic::list(), Event::term()) -> ok.
publish(Topic, Event) when is_bitstring(Topic) ->
	publish(binary_to_list(Topic), Event);
publish(Topic, Event) ->
	gen_event:notify(?BROKER, {Topic, Event}).

%% Starts the topicla application
-spec start() -> ok.
start() ->
	application:load(topical),
	application:start(topical).

%% Stops the topical application
-spec stop() -> ok.
stop() ->
	application:stop(topical).

%% Subscribes to a topic pattern providing a callback
%% for handling messages. Topic takes the form of
%% a '.' seperated namespace with AMQP style wildcard support:
%%	 "#" - matches 0 to any number of topic segment
%% 	 "*" - matches 1 and only 1 topic segment
%% 	 "*.world" - any topic with 2 segments ending in "world"
%%	 "#.world" - any topic ending with "world" including only "world"
-spec subscribe(Topic::list(), Callback::fun((term(), list()) -> ok)) -> ok.
subscribe(Topic, Callback) ->
	gen_event:call(?BROKER, ?HANDLER, {subscribe, Topic, Callback}).

%% Removes the callback from a topic. Must provide the
%% exact Topic pattern and callback reference.
-spec unsubscribe(Topic::list(), Callback::fun((term(), list()) -> ok)) -> ok.
unsubscribe(Topic, Callback) ->
	gen_event:call(?BROKER, ?HANDLER, {unsubscribe, Topic, Callback}).