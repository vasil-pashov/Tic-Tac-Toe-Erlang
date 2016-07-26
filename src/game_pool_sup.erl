-module(game_pool_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link(?MODULE,  []).

init([]) ->
    io:format("Start game pool sup~n"),
    SupFlags = #{
        strategy => simple_one_for_one,
        intesity => 3,
        period => 10
    },
    ChildSpecs = [#{id => game_instance,
                    start => {game_sup, start_link, []},
                    restart => permanent,
                    type => supervisor,
                    modules => [game_sup]}],
    {ok, {SupFlags, ChildSpecs}}.
