-module(player_fsm).
-behaviour(gen_fsm).

%async
-export([make_move/2,
         move_result/2,
         wait_oponent/2,
         new_game/2,
         wait_order/2,
         register_mark/2]).
%sync
-export([wait_oponent/3]).
%start
-export([start/2, start_link/2]).
%gen
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
        terminate/3, code_change/4]).

-record(state, {
    game_pid :: pid(),
    server :: pid()
}).

start(Player, Server) ->
    %TO REMOVE NAMING
    gen_fsm:start({local, binary_to_atom(Player, utf8)}, ?MODULE, [Player, Server], []).

start_link(Player, GamePid) ->
    %io:format("GEN_FSM PLAYER =======player_fsm start link=======~n"),
    %TO REMOVE NAMING
    gen_fsm:start_link(?MODULE, [Player, GamePid], []).

new_game(accept, #state{server=Server}=State) ->
    %gen_fsm:send_event(Game, {accept, Player}),
    gen_server:cast(Server, {forward_to_game, self(), accept}),
    {next_state, wait_order, State};
new_game(decline, #state{server=Server}=State) ->
    gen_server:cast(Server, {forward_to_game, self(), accept}),
    %gen_fsm:send_event(Game, decline),
    {next_state, wait_order, State}.

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

move_result({ok, new_game}, State) -> {next_state, new_game, State};
move_result({ok, continue}, State) -> {next_state, wait_opponent, State};
move_result({error, Err}, State) ->
    io:format("ERROR: ~p~n", [Err]),
    {next_state, make_move, State}.

make_move({make_move, Row, Col}, #state{server=Server}=State) ->
   % case gen_fsm:sync_send_event(GamePid, {make_move, Row, Col, Mark, Player}) of
   %     game_end -> {reply,{ok, game_end}, new_game, State};
   %     continue -> {reply, {ok, continue}, wait_oponent, State};
   %     {error, ErrorMsg} -> {reply, {error, ErrorMsg}, make_move, State}
   % end.
    gen_server:cast(Server, {forward_to_game, self(), {make_move, Row, Col}}),
    {next_state, move_result, State}.

register_mark({register_mark, Mark}, #state{server=Server}=State) ->
    %io:format("PLAYER FSM REGISTER MARK~n"),
    %case gen_fsm:sync_send_event(Game, {register_mark, Mark, Player}) of
    %    {error, ErrorMsg} ->
    %        io:format("GEN_FSM PLAYER MARK ERROR: ~p~n", [ErrorMsg]),
    %        {reply, {error, ErrorMsg}, register_mark, State};
    %    {ok, mark_set} ->
    %        {reply, {ok, mark_set}, wait_order, State#state{player_mark=Mark}};
    %    {ok, mark_set, 1} ->
    %        io:format("PLAYER FSM PLAYER: ~p IS FIRST~n", [Player]),
    %        {reply, {ok, mark_set}, make_move, State#state{player_mark=Mark}};
    %    {ok, mark_set, 2} ->
    %        io:format("PLAYER FSM PLAYER: ~p IS SECOND~n", [Player]),
    %        {reply, {ok, mark_set}, wait_oponent, State#state{player_mark=Mark}}
    %end.
    gen_server:cast(Server, {forward_to_game, self(), {register_mark, Mark}}),
    {next_state, reg_mark_result, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%% GENERIC PART %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([Server]) ->
    io:format("~nGEN_FSM PLAYER =======INIT PLAYER FSM=======~n"),
    io:format("GEN_FSM PLAYER OWN PID: ~p.~n", [self()]),
    %gen_fsm:send_event(GamePid, {register_player, Player, self()}),
    io:format("GEN_FSM PLAYER =======END INIT PLAYER FSM=======~n~n"),
    {ok, register_mark, #state{server=Server}}.

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

