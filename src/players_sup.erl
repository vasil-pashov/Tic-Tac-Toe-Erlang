-module(players_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(Server) ->
    supervisor:start_link(?MODULE, [Server]).

init([Server]) ->
    io:format("PLAYER SUPERVISOR START SUPERVISOR~n"),
    SupFlags = #{
        strategy => simple_one_for_one,
        intesity => 5,
        period => 20
    },
    ChildSpecs = [#{
        id => players_sup,
        start => {player_fsm, start, [Server]},
        restart => transient,
        type => worker,
        modules => [player_fsm]
    }],
    {ok, {SupFlags, ChildSpecs}}.
