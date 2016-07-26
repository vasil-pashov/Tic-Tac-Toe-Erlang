-module(game_fsm).
-behaviour(gen_fsm).

-export([test_state/2]).
-export([start/1, start_link/1]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
        terminate/3, code_change/4]).

-record(state, {
    game_name,
    players = maps:new() :: map(),
    draws = 0 :: integer(),
    sup_pid :: pid(),
    player_sup_pid :: pid()
}).

-record(player_state, {
    wins=0 :: integer(),
    draws=0 :: integer(),
    player_pid :: pid()
}).

-define(PLAYERS_SUP_SPEC, #{id => players_sup,
                    start => {player_sup, start_link, []},
                    restart => permanent,
                    type => worker,
                    modules => [player_sup]}).

start([_,_,_,_]=Args) ->
    gen_fsm:start(?MODULE, Args).

start_link([_,_,_,_]=Args) ->
    io:format("start link for game: ~n"),
    gen_fsm:start_link(?MODULE, Args, []).

test_state(Event, StateData) ->
    io:format("Arrived in test state of the game_fsm with event: ~p",[Event]),
    {next_state, test_state, StateData}.

%===============rGENERIC PART=============================================
init([SupPid, GameName, P1, P2]) ->
    io:format("Init game_fsm: ~p, with players: ~p and ~p~n", [GameName, P1, P2]),
    self() ! {start_players, [P1, P2]},
    {ok, test_state, #state{sup_pid=SupPid, game_name=GameName}}.

handle_event(Event, StateName, StateData) ->
    io:format("Handle Event. Unknown event: ~p in fsm_state: ~p~n", [Event, StateName]),
    {next_state, test_state, StateData}.

handle_sync_event(Event, _From, StateName, StateData) ->
    io:format("Handle sync. Unknown event: ~p in fsm_state: ~p~n", [Event, StateName]),
    {next_state, test_state, StateData}.

handle_info({start_players, Players}, test_state, #state{sup_pid=SupPid}=State) ->
    io:format("Handle info start_players~n"),
    {ok, PlayersSupPid} = supervisor:start_child(SupPid, ?PLAYERS_SUP_SPEC),
    PlayersMap = lists:foldl(fun(El, Acc) ->
                                  {ok, Pid} = supervisor:start_child(PlayersSupPid, [[El, self()]]),
                                  maps:put(El, #player_state{player_pid=Pid}, Acc)
                          end, #{}, Players),
    {next_state, test_state, State#state{players=PlayersMap}};
handle_info(Info, StateName, State) ->
    io:format("Unknown info: ~p, in state: ~p", [Info, StateName]),
    {next_state, test_state, State}.

code_change(_OldVsn, _StateName, _StateData, _Extra) -> ok.

terminate(Reason, StateName, _State) ->
    io:format("Tic-Tac-Toe GAME FSM terminate with from state: ~p, with reason: ~p~n",[StateName, Reason]),
    ok.

