-module(game_fsm).
-behaviour(gen_fsm).

-export([test_state/2, wait_players/2, wait_player_move/2]).
-export([start/1, start_link/1]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
        terminate/3, code_change/4]).

-record(state, {
    game_name,
    players = maps:new() :: map(),
    draws = 0 :: integer(),
    sup_pid :: pid(),
    players_sup_pid :: pid(),
    logged_players = 0 :: integer(),
    current_player,
    board = game_board:new()
}).

-record(player_state, {
    wins=0 :: integer(),
    draws=0 :: integer(),
    player_pid :: pid()
}).

-define(PLAYERS_SUP_SPEC(Args), #{id => players_sup,
                    start => {player_sup, start_link, [Args]},
                    restart => permanent,
                    type => supervisor,
                    shutdown => infinity,
                    modules => [player_sup]}).

start([_,_,_,_]=Args) ->
    gen_fsm:start(?MODULE, Args).

start_link([_,_,_,_]=Args) ->
    io:format("=======game_fsm start link for game==============~n"),
    gen_fsm:start_link(?MODULE, Args, []).

test_state(event, StateData) ->
    io:format("Arrived in test state of the game_fsm with event: ~p",[event]),
    {next_state, test_state, StateData}.

wait_player_move(_Event, State) ->
    io:format("$$$$$$$$$$$$$$$$$$WAIT PLAYER MOVE$$$$$$$$$$$$$$$$$$$$$~n"),
    {next_state, wait_player_move, State}.

wait_players({register_player, PlayerName, PlayerPid}, #state{players=Players0,
                                                             logged_players=Logged}=State) ->
    io:format("==============WAITIN FOR PLAYERS======================~n"),
    Players = case maps:find(PlayerName, Players0) of
        {ok, Player} ->
            maps:update(PlayerName, Player#player_state{player_pid=PlayerPid}, Players0);
        error -> maps:put(PlayerName, #player_state{player_pid=PlayerPid}, Players0)
    end,
    NewState = State#state{logged_players=Logged+1, players=Players},
    if
        NewState#state.logged_players =:= 2 -> {next_state, wait_player_move, NewState};
        NewState#state.logged_players < 2 -> {next_state, wait_players, NewState};
        NewState#state.logged_players > 2 -> {stop, {error, wrong_number_of_players}, NewState}
    end.

%===============rGENERIC PART=============================================
init([SupPid, GameName, P1, P2]) ->
    %io:format("Init game_fsm: ~p (~p), with players: ~p and ~p~n", [GameName, self(), P1, P2]),
    io:format("=======game_fsm init==============================~n"),
    io:format("GAME NAME: ~p~n", [GameName]),
    io:format("PLAYER1: ~p~n", [P1]),
    io:format("PLAYER2: ~p~n", [P2]),
    io:format("SELF: ~p~n", [self()]),
    io:format("SUP PID: ~p~n", [SupPid]),
    io:format("==================================================~n"),
    self() ! {start_players, [P1, P2]},
    {ok, wait_players, #state{sup_pid=SupPid, game_name=GameName}}.

handle_event(Event, StateName, StateData) ->
    io:format("Handle Event. Unknown event: ~p in fsm_state: ~p~n", [Event, StateName]),
    {next_state, test_state, StateData}.

handle_sync_event(Event, _From, StateName, StateData) ->
    io:format("Handle sync. Unknown event: ~p in fsm_state: ~p~n", [Event, StateName]),
    {next_state, test_state, StateData}.

handle_info({start_players, Players}, wait_players, #state{sup_pid=SupPid}=State) ->
    io:format("======game_fsm handle info start_players===========~n"),
    io:format("CHILDREN ~p~n",[supervisor:count_children(SupPid)]),
    NewState = case supervisor:start_child(SupPid, ?PLAYERS_SUP_SPEC({Players, self()})) of
        {ok, Pid} -> State#state{players_sup_pid=Pid};
        {error, {already_started, _Pid}} -> State
    end,
    {next_state, wait_players, NewState};
handle_info(Info, StateName, State) ->
    io:format("Unknown info: ~p, in state: ~p", [Info, StateName]),
    {next_state, test_state, State}.

code_change(_OldVsn, _StateName, _StateData, _Extra) -> ok.

terminate(Reason, StateName, _State) ->
    io:format("===========GAME FSM terminate===============~n"),
    io:format("STATE: ~p~n",[StateName]),
    io:format("REASON: ~p~n",[Reason]),
    io:format("============================================~n"),
    ok.

