-module(games_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(Server) ->
    supervisor:start_link(?MODULE, [Server]).

init([Server]) -> 
    io:format("GAMES SUP START SUPERVISOR~n"),
    SupFlags = #{
        strategy => simple_one_for_one,
        intesity => 5,
        period => 5
    },
    ChildSpecs = [#{
        id => games_sup,
        start => {game_fsm, start_link, [Server]},
        restart => temporary,
        type => worker,
        modules => [game_fsm]
    }],
    {ok, {SupFlags, ChildSpecs}}.
