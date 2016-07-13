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
    supervisor:start_link({local, ?MODULE}, ?MODULE, [ServName]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([ServName]) ->
    io:format("Init gs"),
    {ok, {{one_for_all, 5, 100}, [
            {tic_tac_toe_game_app, {game_serv, start_link, [ServName]},
            permanent, 1000, worker, [game_serv]}
    ]}}.

