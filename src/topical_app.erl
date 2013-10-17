%%% @author Alex Robson
%%% @doc
%%%
%%% @end
%%% Licensed under the MIT license - http://www.opensource.org/licenses/mit-license
%%% Created Oct 17, 2013

-module(topical_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%%  Application callbacks
%% ===================================================================

start() ->
	start([], []).

start(_StartType, _StartArgs) ->
	topical_sup:start_link().

stop(_State) ->
	ok.