-module(game_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link([GameName, P1, P2]) ->
    io:format("Game Sup start link~n"),
    supervisor:start_link(?MODULE, [GameName, P1, P2]).

init([_GameName, _P1, _P2]=GameSettings) -> 
    io:format("START GAME~n"),
    SupFlags = #{
      strategy => one_for_one,
      intesity => 5,
      period => 5
    },
    ChildSpecs = [#{id => game,
                    start => {game_fsm, start_link, [[self()|GameSettings]]},
                    restart => permanent,
                    type => worker,
                    modules => [game_fsm]}],
    {ok, {SupFlags, ChildSpecs}}.


