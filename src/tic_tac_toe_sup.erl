-module(tic_tac_toe_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    io:format("========tic_tac_toe_sup starting link=============~n"),
    supervisor:start_link(?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    io:format("========tic_tac_toe_sup init starting game server========~n"),
    SupFlags = #{strategy => one_for_one,
      intensity => 5,
      period => 10
    },
    ChildSpecs = [#{id => tic_tac_toe_game_app,
                    start => {game_serv, start_link, [self()]},
                    restart => permanent,
                    type => worker,
                    modules => [game_serv]
    }],
    {ok, {SupFlags, ChildSpecs}}.
