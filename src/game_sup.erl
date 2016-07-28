-module(game_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link([GameName, P1, P2]) ->
    io:format("==========game_sup start link=============~n"),
    supervisor:start_link(?MODULE, [GameName, P1, P2]).

init([GameName, P1, P2]=GameSettings) -> 
    io:format("==========game_sup init starts game============~n"),
    io:format("GAME NAME: ~p~n", [GameName]),
    io:format("PLAYER1: ~p~n", [P1]),
    io:format("PLAYER2: ~p~n", [P2]),
    io:format("================================================~n"),
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


