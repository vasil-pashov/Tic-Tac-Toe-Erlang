-module(player_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link({_Players, _GamePid}=Args) ->
    io:format("======player_sup start link=========~n"),
    supervisor:start_link(?MODULE, Args).

init({Players, GamePid}) ->
    io:format("======player_sup init===============~n"),
    SupFlags = #{strategy => one_for_one,
                 intesity => 5,
                 period => 20},
    ChildSpecs = [#{
        id => Name,
        start => {player_fsm, start_link, [{Name, GamePid}]},
        restart => permanent,
        type => worker,
        modules => [player_fsm]} || Name <- Players],
    {ok, {SupFlags, ChildSpecs}}.
