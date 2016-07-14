-module(game_pool_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link({_M, _F, _A}=MFA) -> supervisor:start_link({local, ?MODULE}, ?MODULE, MFA).

init({M, _F, _A}=MFA) ->
    io:format("Start game pool sup~n"),
    SupFlags = #{
        strategy => simple_one_for_one,
        intesity => 3,
        period => 10
    },
    ChildSpecs = [#{id => game_instance,
                    start => MFA,
                    restart => transient,
                    type => supervisor,
                    modules => [M]}],
    {ok, {SupFlags, ChildSpecs}}.
