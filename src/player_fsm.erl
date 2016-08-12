-module(player_fsm).
-behaviour(gen_fsm).

%async
-export([wait_data/2,
         wait_oponent/2,
         new_game/2,
         wait_order/2]).
%sync
-export([wait_oponent/3,
         register_mark/3,
         make_move/3]).
%start
-export([start/2, start_link/2]).
%gen
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
        terminate/3, code_change/4]).

-record(state, {
    game_pid :: pid(),
    player
}).

start(Server, Player) ->
    %TO REMOVE NAMING
    gen_fsm:start({local, binary_to_atom(Player, utf8)}, ?MODULE, [Server], []).

start_link(Server, Player) ->
    %io:format("GEN_FSM PLAYER =======player_fsm start link=======~n"),
    %TO REMOVE NAMING
    gen_fsm:start_link({local, binary_to_atom(Player, utf8)}, ?MODULE, [Server], []).

new_game(accept, #state{game_pid=Game,
                       player=Player}=State) ->
    gen_fsm:send_event(Game, {accept, Player}),
    {next_state, wait_order, State};
new_game(decline, #state{game_pid=Game,
                       player=_Player}=State) ->
    gen_fsm:send_event(Game, decline),
    {next_state, wait_order, State}.

wait_data({data, Player, Game}, State) ->
    gen_fsm:send_event(Game, {register_player, Player, self()}),
    {next_state, register_mark, State#state{player=Player, game_pid=Game}}.

wait_order({start_game, Turn}, State) ->
    case Turn of
        1 ->
            io:format("PLAYER FSM PLAYER: ~p IS FIRST~n", [self()]),
            {next_state, make_move, State};
        2 -> 
            io:format("PLAYER FSM PLAYER: ~p IS SECOND~n", [self()]),
            {next_state, wait_oponent, State}
    end.

wait_oponent(make_move, State) ->
    {next_state, make_move, State};
wait_oponent(game_end, State) ->
    {next_state, new_game, State}.

wait_oponent({make_move, _Row, _Col}, _From, State) ->
    {reply, {error, not_your_turn}, wait_oponent, State}.

make_move({make_move, Row, Col}, _From, #state{game_pid=Game,
                                       player=Player}=State) ->
    case gen_fsm:sync_send_event(Game, {make_move, Row, Col, Player}) of
        game_end -> {reply,{ok, game_end}, new_game, State};
        continue -> {reply, {ok, continue}, wait_oponent, State};
        {error, ErrorMsg} -> {reply, {error, ErrorMsg}, make_move, State}
    end.

register_mark({register_mark, Mark}, _From, #state{player=Player,
                                           game_pid=Game}=State) ->
    io:format("PLAYER FSM REGISTER MARK~n"),
    case gen_fsm:sync_send_event(Game, {register_mark, Mark, Player}) of
        {error, ErrorMsg} ->
            io:format("GEN_FSM PLAYER MARK ERROR: ~p~n", [ErrorMsg]),
            {reply, {error, ErrorMsg}, register_mark, State};
        {ok, mark_set} ->
            {reply, {ok, mark_set}, wait_order, State};
        {ok, mark_set, 1} ->
            io:format("PLAYER FSM PLAYER: ~p IS FIRST~n", [Player]),
            {reply, {ok, mark_set}, make_move, State};
        {ok, mark_set, 2} ->
            io:format("PLAYER FSM PLAYER: ~p IS SECOND~n", [Player]),
            {reply, {ok, mark_set}, wait_oponent, State}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%% GENERIC PART %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([Server]) ->
    io:format("~nGEN_FSM PLAYER =======INIT PLAYER FSM=======~n"),
    io:format("GEN_FSM PLAYER OWN PID: ~p.~n", [self()]),
    %gen_fsm:send_event(GamePid, {register_player, Player, self()}),
    {Player, GamePid} = Server ! {get_game_info, self()},
    io:format("GEN_FSM PLAYER =======END INIT PLAYER FSM=======~n~n"),
    {ok, wait_data, #state{player=Player,
                              game_pid=GamePid}}.

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

