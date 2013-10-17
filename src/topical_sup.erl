%%% @author Alex Robson
%%% @doc
%%%
%%% App level supervisor
%%%
%%% @end
%%% Licensed under the MIT license - http://www.opensource.org/licenses/mit-license
%%% Created Oct 17, 2013

-module(topical_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

-define(SERVER, ?MODULE).

%% ===================================================================
%%  API
%% ===================================================================

start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
	RestartStrategy = one_for_one,
	MaxRestarts = 3,
	MaxSecondsBetweenRestarts = 60,
	SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
	Events = create_child_spec(event_broker, worker, permanent, 1000, []),
	{ok, {SupFlags, [Events]}}.

%% ===================================================================
%%  Internal functions
%% ===================================================================

create_child_spec(Child, Type, Restart, Shutdown, Args) ->
	{Child, { Child, start, Args }, Restart, Shutdown, Type, [Child]}.