-module(tic_tac_toe_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(ServName) ->
    io:format("Starting link~n"),
    supervisor:start_link(?MODULE, [ServName]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([ServName]) ->
    io:format("Init gs~n"),
    SupFlags = #{strategy => one_for_all,
      intensity => 5,
      period => 10
    },
    ChildSpecs = [#{id => tic_tac_toe_game_app,
                    start => {game_serv, start_link, [ServName]},
                    restart => permanent,
                    type => worker,
                    modules => [game_serv]
    }],
    {ok, {SupFlags, ChildSpecs}}.
