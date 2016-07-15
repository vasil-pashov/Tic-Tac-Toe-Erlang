-module(plaer_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link({_P1, _P2}=Players) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Players).

init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intesity => 5,
                 period => 20},
    ChildSpecs = [#{id => player_instance,
                    start => {player_fsm, start_link, []},
                    restart => permanent,
                    type => worker,
                    modules => [player_fsm]}],
    {ok, {SupFlags, ChildSpecs}}.
