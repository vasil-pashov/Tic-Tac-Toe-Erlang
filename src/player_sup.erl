-module(player_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intesity => 5,
                 period => 20},
    ChildSpecs = [#{
        id => player,
        start => {player_fsm, start_link, []},
        restart => permanent,
        type => worker,
        modules => [player_fsm]}],
    {ok, {SupFlags, ChildSpecs}}.
