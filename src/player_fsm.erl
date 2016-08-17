-module(player_fsm).
-behaviour(gen_fsm).

%async
-export([new_game/2,
         in_game/2]).
%sync
-export([register_mark/3,
         in_game/3]).
%start
-export([start/2, start_link/2]).
%gen
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
        terminate/3, code_change/4]).

-record(state, {
    game_pid :: pid(),
    player
}).

start(Player, GamePid) ->
    gen_fsm:start(?MODULE, [Player, GamePid], []).

start_link(Player, GamePid) ->
    gen_fsm:start_link(?MODULE, [Player, GamePid], []).

new_game(accept, #state{game_pid=Game,
                       player=Player}=State) ->
    gen_fsm:send_event(Game, {accept, Player}),
    {next_state, in_game, State};
new_game(decline, #state{game_pid=Game,
                       player=_Player}=State) ->
    gen_fsm:send_event(Game, decline),
    {next_state, in_game, State}.

in_game({make_move, Row, Col}, _From, #state{game_pid=Game,
                                       player=Player}=State) ->
    case gen_fsm:sync_send_event(Game, {make_move, Row, Col, Player}) of
        game_end -> {reply,{ok, game_end}, new_game, State};
        continue -> {reply, {ok, continue}, in_game, State};
        {error, ErrorMsg} -> {reply, {error, ErrorMsg}, in_game, State}
    end.

in_game(game_end, State) ->
    {next_state, new_game, State}.

register_mark({register_mark, Mark}, _From, #state{player=Player,
                                           game_pid=Game}=State) ->
    io:format("PLAYER FSM REGISTER MARK: ~p~n", [Game]),
    case gen_fsm:sync_send_event(Game, {register_mark, Mark, Player}) of
        {error, ErrorMsg} ->
            io:format("GEN_FSM PLAYER MARK ERROR: ~p~n", [ErrorMsg]),
            {reply, {error, ErrorMsg}, register_mark, State};
        {ok, mark_set} ->
            {reply, {ok, mark_set}, in_game, State}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%% GENERIC PART %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([Player, GamePid]) ->
    io:format("~nGEN_FSM PLAYER =======INIT PLAYER FSM=======~n"),
    io:format("GEN_FSM PLAYER OWN PID: ~p ~p.~n", [self(), Player]),
    erlang:register(binary_to_atom(Player, utf8), self()),
    GamePid ! {register_player, Player, self()},
    io:format("GEN_FSM PLAYER =======END INIT PLAYER FSM=======~n~n"),
    {ok, register_mark, #state{player=Player, game_pid=GamePid}}.

handle_event({next_state, mark}, _StateName, State) ->
    {next_state, register_mark, State};
handle_event({next_state, in_game}, _StateName, State) ->
    {next_state, in_game, State};
handle_event({next_state, new_game}, _StateName, State) ->
    {next_state, new_game, State};
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

