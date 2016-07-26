-module(player_fsm).
-behaviour(gen_fsm).

-export([start/1, start_link/1]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
        terminate/3, code_change/4]).

-record(state, {
    game_pid :: pid()
}).

start([Player, GamePid]) ->
    gen_fsm:start(?MODULE, [Player, GamePid], []).

start_link([Player, GamePid]) ->
    io:format("Player start link~n"),
    gen_fsm:start_link(?MODULE, [Player, GamePid], []).

init([Player, GamePid]) ->
    io:format("Creating player_fsm for player: ~p", [Player]),
    {ok, test_state, #state{game_pid=GamePid}}; 
init(S) -> io:format("WTF ~p~n", [S]).

handle_event(Event, StateName, StateData) ->
    io:format("Handle Player Event. Unknown event: ~p in fsm_state: ~p~n", [Event, StateName]),
    {next_state, test_state, StateData}.

handle_sync_event(Event, _From, StateName, StateData) ->
    io:format("Handle Player sync. Unknown event: ~p in fsm_state: ~p~n", [Event, StateName]),
    {next_state, test_state, StateData}.

handle_info(Info, StateName, State) ->
    io:format("Unknown Player info: ~p, in state: ~p", [Info, StateName]),
    {next_state, test_state, State}.

code_change(_OldVsn, _StateName, _StateData, _Extra) -> ok.

terminate(Reason, StateName, _State) ->
    io:format("Tic-Tac-Toe Player FSM terminate with from state: ~p, with reason: ~p~n",[StateName, Reason]),
    ok.

