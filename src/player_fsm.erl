-module(player_fsm).
-behaviour(gen_fsm).

%async
-export([wait_oponent/2, register_mark/2]).
%sync
-export([make_move/3, wait_oponent/3, register_mark/3]).
%start
-export([start/2, start_link/3]).
%gen
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
        terminate/3, code_change/4]).

-record(state, {
    game_pid :: pid(),
    player_mark,
    player_name
}).

start(Player, GamePid) ->
    gen_fsm:start(?MODULE, [Player, GamePid], []).

start_link(Player, GamePid, ProcName) ->
    io:format("GEN_FSM PLAYER =======player_fsm start link=======~n"),
    gen_fsm:start_link({local, ProcName}, ?MODULE, [Player, GamePid], []).

wait_oponent(game_end, State) ->
    {next_state, new_game, State};
wait_oponent(make_move, State) ->
    {next_state, make_move, State}.

wait_oponent({make_move, _Row, _Col}, _From, State) ->
    {reply, {error, not_your_turn}, make_move, State}.

make_move({make_move, Row, Col}, _From, #state{game_pid=GamePid,
                                       player_mark=Mark,
                                       player_name=Player}=State) ->
    case gen_fsm:sync_send_event(GamePid, {make_move, Row, Col, Mark, Player}) of
        game_end -> {reply,{ok, game_end}, new_game, State};
        continue -> {reply, {ok, continue}, wait_oponent, State};
        {error, ErrorMsg} -> {reply, {error, ErrorMsg}, make_move, State}
    end.

register_mark({register_mark, Mark}, _From, #state{game_pid=Game,
                                           player_name=Player}=State) ->
    io:format("PLAYER FSM REGISTER MARK~n"),
    case gen_fsm:sync_send_event(Game, {register_mark, Mark, Player}) of
        {error, ErrorMsg} ->
            io:format("GEN_FSM PLAYER MARK ERROR: ~p~n", [ErrorMsg]),
            {reply, {error, ErrorMsg}, register_mark, State};
        {ok, mark_set} ->
            {reply, {ok, mark_set}, register_mark, State#state{player_mark=Mark}};
        {ok, mark_set, 1} ->
            {reply, {ok, mark_set}, make_move, State#state{player_mark=Mark}};
        {ok, mark_set, 2} ->
            {reply, {ok, mark_set}, wait_oponent, State#state{player_mark=Mark}}
    end.

register_mark({start_game, Turn}, State) ->
    case Turn of 
        1 -> 
            io:format("PLAYER FSM PLAYER: ~p is first~n", [State#state.player_name]),
            {next_state, make_move, State};
        2 ->
            io:format("PLAYER FSM PLAYER: ~p is second~n", [State#state.player_name]),
            {next_state, wait_oponent, State}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%% GENERIC PART %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([Player, GamePid]) ->
    io:format("GEN_FSM PLAYER =======player_fsm init=======~n"),
    io:format("GEN_FSM PLAYER PLAYER NAME: ~p~n", [Player]),
    io:format("GEN_FSM PLAYER GAME PID: ~p~n", [GamePid]),
    io:format("GEN_FSM PLAYER OWN PID: ~p.~n", [self()]),
    io:format("GEN_FSM PLAYER =============================~n"),
    gen_fsm:send_event(GamePid, {register_player, Player, self()}),
    {ok, register_mark, #state{game_pid=GamePid, player_name=Player}}.

handle_event(stop, _StateName, StateData) ->
    {stop, normal, StateData};
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

