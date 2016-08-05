-module(games_sup).
-behaviour(supervisor).

-export([start_link/2]).
-export([init/1]).

start_link(Server, PlayersSup) ->
    supervisor:start_link(?MODULE, [Server, PlayersSup]).

init([Server, PlayersSup]) -> 
    io:format("GAMES SUP START SUPERVISOR~n"),
    SupFlags = #{
        strategy => simple_one_for_one,
        intesity => 5,
        period => 5
    },
    ChildSpecs = [#{
        id => games_sup,
        start => {game_fsm, start_link, [Server, PlayersSup]},
        restart => transient,
        type => worker,
        modules => [game_fsm]
    }],
    {ok, {SupFlags, ChildSpecs}}.
