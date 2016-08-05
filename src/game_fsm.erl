-module(game_fsm).
-behaviour(gen_fsm).

%async
-export([wait_players/2]).
%sync
-export([wait_move/3, register_marks/3]).
%start
-export([start/5, start_link/5]).
%gen
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
        terminate/3, code_change/4]).

-record(state, {
    game_name,
    players = maps:new() :: map(),
    players_sup_pid :: pid(),
    server_pid :: pid(),
    logged_players = 0 :: integer(),
    marks_registered = 0 :: integer(),
    current_player,
    board = game_board:new(),
    moves_made = 0 :: integer()
}).

-record(player_data, {
    pid :: pid(),
    mark = undefined
}).

start(Server, PlayersSup, GameName, P1, P2) ->
    gen_fsm:start(?MODULE, [Server, PlayersSup, GameName, P1, P2], []).

start_link(Server, PlayersSup, GameName, P1, P2) ->
    io:format("GEN_FSM GAME =======game_fsm start link for game==============~n"),
    gen_fsm:start_link(?MODULE, [Server, PlayersSup, GameName, P1, P2], []).

wait_players({register_player, PlayerName, PlayerPid}, #state{players=Players0,
                                                logged_players=Logged}=State) ->
    io:format("GEN_FSM GAME ==============WAITIN FOR PLAYERS======================~n"),
    Players = case maps:find(PlayerName, Players0) of
        {ok, Value} ->
                io:format("GEN_FSM GAME UPDATING PLAYER PID FOR PLAYER: ~p~n", [PlayerName]),
                maps:update(PlayerName, Value#player_data{pid=PlayerPid}, Players0);
        error ->
                io:format("GEN_FSM GAME REGISTER PLAYER: ~p~n", [PlayerName]),
                maps:put(PlayerName, #player_data{pid=PlayerPid}, Players0)
    end,
    NewState = State#state{logged_players=Logged+1, players=Players},
    if
        NewState#state.logged_players =:= 2 ->
            io:format("GEN_FSM GAME ALL PLAYERS REGISTERD~n"),
            {next_state, register_mark, NewState};
        NewState#state.logged_players < 2 ->
            io:format("GEN_FSM GAME ONE PLAYER REGISTERD~n"),
            {next_state, wait_players, NewState};
        NewState#state.logged_players > 2 -> 
            io:format("GEN_FSM GAME MORE THAT 2 PLAYERS REGISTERD~n"),
            {stop, {error, wrong_number_of_players}, NewState}
    end.

register_marks({register_mark, Mark, Player}, _From, #state{players=Players,
                                                           marks_registered=Marks
                                                           }=State) when is_binary(Mark) ->
    OpponentName = switch_players(Players, Player),
    OpponentData = maps:find(OpponentName, Players),
    PlayerData = maps:find(Player, Players),
    case OpponentData#player_data.mark =:= Mark of
        true ->
            io:format("GEN_FSM GAME ERROR MARK ALREADY USED~n"),
            {reply, {error, taken}, register_mark, State};
        false -> 
            NewPlayers = maps:update(Player, PlayerData#player_data{mark=Mark},
                                  State#state{marks_registered=Marks+1}),
            if
                Marks+1 =:= 2 ->
                    io:format("GEN_FSM GAME PLAYER: ~p MARK: ~p REGISTERD~n", [Player, Mark]),
                    {reply, {ok, mark_set, 2}, wait_move,
                                State#state{players=NewPlayers}};
                Marks+1 < 2 ->
                    io:format("GEN_FSM GAME PLAYER: ~p MARK: ~p REGISTERD~n", [Player, Mark]),
                    {reply, {ok, mark_set, 1}, register_marks,
                                State#state{players=NewPlayers}}
            end
    end.

wait_move({make_move, Row, Col, Mark, Player}, _From, #state{current_player=CurrentPlayer,
                                                       board=Board,
                                                       moves_made=MovesMade0,
                                                       players=Players}=State) -> 
    case CurrentPlayer =:= Player of
        false ->
            io:format("GEN_FSM GAME PLAYER: ~p CANNOT MOVE. NOT YOUR TURN.", [Player]),
            {reply, {error, not_your_turn}, wait_move, State};
        true -> 
            case game_board:set(Row, Col, Mark, Board) of
                {error, ErrorMsg} ->
                    io:format("GEN_FSM GAME PLAYER: ~p. MOVE ERROR: ~p~n", [Player, ErrorMsg]),
                    {reply, {error, ErrorMsg}, wait_move, State};
                {ok, NewBoard} -> 
                    NextPlayer = switch_players(Players, CurrentPlayer),
                    io:format("GEN_FSM GAME PLAYER: ~p MADE MOVE. NOW IS PLAYER: ~p~n",[Player, NextPlayer]),
                    game_board:print(NewBoard),
                    MovesMade=MovesMade0+1,
                    OtherPlayerData = maps:get(NextPlayer, Players),
                    OtherPlayerPid = OtherPlayerData#player_data.pid,
                    case process_move(Row, Col, Mark, State) of
                        win -> 
                            io:format("GEN_FSM GAME PLAYER: ~p WINS!!!~n", [Player]),
                            gen_fsm:send_event(OtherPlayerPid, game_end),
                            {reply, game_end, wait_new_game, State#state{board=game_board:new()}};
                        draw -> 
                            io:format("GEN_FSM GAME GAME IS DRAW~n"),
                            gen_fsm:send_event(OtherPlayerPid, game_end),
                            {reply, game_end, wait_new_game, State#state{board=game_board:new()}};
                        continue ->
                            io:format("GEN_FSM GAME CONTINUE NEXT MOVE~n"),
                            gen_fsm:send_event(OtherPlayerPid, make_move),
                            {reply, continue, wait_move, State#state{board=NewBoard,
                                                                      moves_made=MovesMade,
                                                                      current_player=NextPlayer}}
                    end
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%% GENERIC PART %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([Server, PlayersSup, GameName, P1, P2]) ->
    io:format("GEN_FSM GAME =======game_fsm init==============================~n"),
    io:format("GEN_FSM GAME GAME NAME: ~p~n", [GameName]),
    io:format("GEN_FSM GAME PLAYER1: ~p~n", [P1]),
    io:format("GEN_FSM GAME PLAYER2: ~p~n", [P2]),
    io:format("GEN_FSM GAME SELF: ~p~n", [self()]),
    io:format("GEN_FSM GAME SUP PID: ~p~n", [PlayersSup]),
    io:format("GEN_FSM GAME ==================================================~n"),
    random:seed(erlang:phash2([node()]),
                erlang:monotonic_time(),
                erlang:unique_integer()),
    self() ! {start_players, [P1, P2]},
    {ok, wait_players, #state{game_name=GameName,
                              server_pid=Server,
                              players_sup_pid=PlayersSup}}.

handle_event(Event, StateName, StateData) ->
    io:format("GEN_FSM GAME Handle Event. Unknown event: ~p in fsm_state: ~p~n", [Event, StateName]),
    {next_state, test_state, StateData}.

handle_sync_event(Event, _From, StateName, StateData) ->
    io:format("GEN_FSM GAME Handle sync. Unknown event: ~p in fsm_state: ~p~n", [Event, StateName]),
    {next_state, test_state, StateData}.

handle_info({start_players, Players}, wait_players, #state{players_sup_pid=PlayersSupPid}=State) ->
    lists:foreach(fun(Player) -> 
                          supervisor:start_child(PlayersSupPid, [Player, self()])
                  end, Players),
    {next_state, wait_players, State};
handle_info(Info, StateName, State) ->
    io:format("GEN_FSM GAME Unknown info: ~p, in state: ~p", [Info, StateName]),
    {next_state, test_state, State}.

code_change(_OldVsn, _StateName, _StateData, _Extra) -> ok.

terminate(Reason, StateName, _State) ->
    io:format("GEN_FSM GAME ===========GAME FSM terminate===============~n"),
    io:format("GEN_FSM GAME STATE: ~p~n",[StateName]),
    io:format("GEN_FSM GAME REASON: ~p~n",[Reason]),
    io:format("GEN_FSM GAME ============================================~n"),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNC %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

switch_players(Players, CurrentPlayer) when erlang:is_map(Players) ->
    switch_players(maps:to_list(Players), CurrentPlayer);
switch_players([{Player, _PlayerData}|Tail], CurrentPlayer) ->
    case Player =:= CurrentPlayer of
        true -> switch_players(Tail, CurrentPlayer);
        false -> Player
    end.
                
process_move(Row, Col, Mark, #state{current_player=Player,
                                   moves_made=MovesMade,
                                   server_pid=Server,
                                   board=Board,
                                   players=Players}) ->
    case game_board:check_win(Row, Col, Mark, Board, MovesMade) of
        true -> 
            process_win(Player, Players, Server), 
            win;
        draw -> 
            process_draw(Server, Players),
            draw;
        false ->
            process_continue(),
            continue
    end.   

process_win(CurrentPlayer, Players, Server) ->
    io:format("GEN_FSM GAME Send win msg to server~n"),
    Fun = fun(Player, _Val, _Acc) ->
            case CurrentPlayer =:= Player of
                    true -> 
                        gen_server:cast(Server, {win, Player});
                    false ->
                        gen_server:cast(Server, {lose, Player})
            end
         end,
    loop_players(Players, Fun).
    
process_draw(Server, Players) ->
    io:format("GEN_FSM GAME Send drawn msg to server~n"),
    Fun = fun(Player, _Val, _Acc) ->
            gen_server:cast(Server, {draw, Player})
          end,
    loop_players(Players, Fun).
            
process_continue() ->
    io:format("GEN_FSM GAME Continue game~n").

loop_players(Players, Fun) ->
    maps:fold(Fun, [], Players).
