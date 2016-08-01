-module(player_fsm).
-behaviour(gen_fsm).

-export([make_move/2]).
-export([start/1, start_link/1]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
        terminate/3, code_change/4]).

-record(state, {
    game_pid :: pid(),
    player_mark,
    player_name
}).

start({Player, GamePid, Mark}) ->
    gen_fsm:start(?MODULE, [Player, GamePid, Mark], []).

start_link({Player, GamePid, Mark}) ->
    io:format("=======player_fsm start link=======~n"),
    gen_fsm:start_link(?MODULE, {Player, GamePid, Mark}, []).

make_move({make_move, Row, Col}, #state{game_pid=GamePid,
                                       player_mark=Mark,
                                       player_name=Player}=State) ->
    case gen_fsm:sync_send_event(GamePid, {move, Row, Col, Mark, Player}) of
        ok -> {next_state, wait_opponent, State};
        error -> {next_state, make_move, State}
    end.

init({Player, GamePid, Mark}) ->
    io:format("=======player_fsm init=======~n"),
    io:format("PLAYER NAME: ~p~n", [Player]),
    io:format("GAME PID: ~p~n", [GamePid]),
    io:format("=============================~n"),
    gen_fsm:send_event(GamePid, {register_player, Player, self()}),
    {ok, test_state, #state{game_pid=GamePid, player_name=Player, player_mark=Mark}}.

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

