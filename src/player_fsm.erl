-module(player_fsm).
-behaviour(gen_fsm).

%async
-export([make_move/2, wait_oponent/2]).
%sync
-export([choose_mark/3]).
%start
-export([start/1, start_link/1]).
%gen
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
        terminate/3, code_change/4]).

-record(state, {
    game_pid :: pid(),
    player_mark,
    player_name
}).

start({Player, GamePid}) ->
    gen_fsm:start(?MODULE, [Player, GamePid], []).

start_link({Player, GamePid}) ->
    io:format("GEN_FSM PLAYER =======player_fsm start link=======~n"),
    gen_fsm:start_link(?MODULE, {Player, GamePid}, []).

wait_oponent(game_end, State) ->
    {next_state, new_game, State};
wait_oponent(make_move, State) ->
    {next_state, make_move, State}.

make_move({make_move, Row, Col}, #state{game_pid=GamePid,
                                       player_mark=Mark,
                                       player_name=Player}=State) ->
    case gen_fsm:sync_send_event(GamePid, {move, Row, Col, Mark, Player}) of
        game_end -> {next_state, new_game, State};
        continue -> {next_state, wait_oponent, State};
        {error, ErrorMsg} ->
            io:format("GEN_FSM PLAYER ~p~n", [ErrorMsg]),
            {next_state, make_move, State}
    end.

choose_mark({set_mark, Mark}, _From, #state{game_pid=Server,
                                           player_name=Player}=State) ->
    case gen_fsm:sync_send_event(Server, {register_mark, Mark, Player}) of
        {error, taken} ->
            io:format("GEN_FSM PLAYER This mark is taken~n"),
            {reply, {error, taken}, choose_mark, State};
        {ok, mark_set, 1} ->
            io:format("GEN_FSM PLAYER Mark is set you are first~n"),
            {reply, {ok, mark_set, 1}, make_move, State};
        {ok, mark_set, 2} ->
            io:format("GEN_FSM PLAYER Mark is set you are second~n"),
            {reply, {ok, mark_set, 2}, wait_opponent, State}
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%% GENERIC PART %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init({Player, GamePid}) ->
    io:format("GEN_FSM PLAYER =======player_fsm init=======~n"),
    io:format("GEN_FSM PLAYER PLAYER NAME: ~p~n", [Player]),
    io:format("GEN_FSM PLAYER GAME PID: ~p~n", [GamePid]),
    io:format("GEN_FSM PLAYER =============================~n"),
    gen_fsm:send_event(GamePid, {register_player, Player, self()}),
    {ok, choose_mark, #state{game_pid=GamePid, player_name=Player}}.

handle_event(Event, StateName, StateData) ->
    io:format("GEN_FSM PLAYER Handle Player Event. Unknown event: ~p in fsm_state: ~p~n", [Event, StateName]),
    {next_state, test_state, StateData}.

handle_sync_event(Event, _From, StateName, StateData) ->
    io:format("GEN_FSM PLAYER Handle Player sync. Unknown event: ~p in fsm_state: ~p~n", [Event, StateName]),
    {next_state, test_state, StateData}.

handle_info(Info, StateName, State) ->
    io:format("GEN_FSM PLAYER Unknown Player info: ~p, in state: ~p", [Info, StateName]),
    {next_state, test_state, State}.

code_change(_OldVsn, _StateName, _StateData, _Extra) -> ok.

terminate(Reason, StateName, _State) ->
    io:format("GEN_FSM PLAYER Tic-Tac-Toe Player FSM terminate with from state: ~p, with reason: ~p~n",[StateName, Reason]),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

