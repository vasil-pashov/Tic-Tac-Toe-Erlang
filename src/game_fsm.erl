-module(game_fsm).
-behaviour(gen_fsm).

%async
-export([wait_new_game/2]).
%sync
-export([wait_move/3, register_mark/3]).
%start
-export([start/5, start_link/5]).
%gen
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
        terminate/3, code_change/4]).

-record(state, {
    game_name,
    players = maps:new() :: map(),
    server_pid :: pid(),
    current_player = undefined,
    board = game_board:new(),
    moves_made = 0 :: integer(),
    logged_players = 0 :: integer(),
    marks_registered = 0 :: integer(),
    new_game_confirmed = 0 :: integer()
}).

-record(player_data, {
    pid :: pid(),
    mark = undefined
}).

start(Server, GameName, PlayersSup, Player1, Player2) ->
    gen_fsm:start(?MODULE, [Server, GameName, PlayersSup, Player1, Player2], []).

start_link(Server, GameName, PlayersSup, Player1, Player2) ->
    %io:format("GEN_FSM GAME =======game_fsm start link for game==============~n"),
    %GAME PROCCESS NAME TO BE REMOVED AFTER TESTING
    gen_fsm:start_link(?MODULE, [Server, GameName, PlayersSup, Player1, Player2], []).

wait_new_game({accept, PlayerName}, #state{new_game_confirmed=NewGame,
                                          players=Players}=State) ->
    case NewGame + 1 of
        1 -> {next_state, wait_new_game, State#state{new_game_confirmed=NewGame+1}};
        2 -> 
            {ok, PlayerData} = maps:find(PlayerName, Players),
            OtherPlayerName = switch_players(Players, PlayerName),
            {ok, OtherPlayerData} = maps:find(OtherPlayerName, Players),
            Turn = random:uniform(2),
            CurrentPlayer = case Turn of
                                1 -> PlayerName;
                                2 -> OtherPlayerName
                            end,
            io:format("~p~n ~p~n", [PlayerData, OtherPlayerData]),
            gen_fsm:send_event(PlayerData#player_data.pid, {start_game, Turn}),
            gen_fsm:send_event(OtherPlayerData#player_data.pid, {start_game, 3 - Turn}),
            io:format("GAME FSM START NEW GAME~n"),
            {next_state, wait_move, State#state{current_player=CurrentPlayer,
                                               new_game_confirmed=0}}
    end;
wait_new_game(decline, State) ->
    {stop, normal, State}.

register_mark({register_mark, Mark, PlayerName}, _From, #state{players=Players,
                                                           marks_registered=Marks
                                                           }=State) ->
    io:format("GAME FSM REGISTER MARK~n"),
    case can_set_mark(Mark, PlayerName, Players) of
        {error, ErrorMsg} ->
            io:format("GEN_FSM GAME ERROR CANNOT SET MARK REASON: ~p~n", [ErrorMsg]),
            {reply, {error, ErrorMsg}, register_mark, State};
        ok -> 
            {ok, PlayerData} = maps:find(PlayerName, Players),
            NewPlayers = maps:update(PlayerName, PlayerData#player_data{mark=Mark}, Players),
            if
                Marks+1 =:= 2 ->
                    io:format("GEN_FSM GAME PLAYER: ~p MARK: ~p REGISTERD~n", [PlayerName, Mark]),
                    Turn = random:uniform(2),
                    OpponentName = switch_players(Players, PlayerName),
                    {ok, OpponentData} = maps:find(OpponentName, Players),
                    CurrentPlayer = case Turn of
                        1 -> PlayerName;
                        2 -> OpponentName
                    end,
                    gen_fsm:send_event(OpponentData#player_data.pid, {start_game, 3 - Turn}),
                    {reply, {ok, mark_set, Turn}, wait_move,
                                State#state{players=NewPlayers,
                                            current_player=CurrentPlayer}};
                Marks+1 < 2 ->
                    io:format("GEN_FSM GAME PLAYER: ~p MARK: ~p REGISTERD~n",
                              [PlayerName, Mark]),
                    {reply, {ok, mark_set}, register_mark,
                                State#state{players=NewPlayers,
                                            marks_registered=Marks+1}}
            end
    end.

wait_move({make_move, Row, Col, Player}, _From, #state{current_player=CurrentPlayer,
                                                       board=Board,
                                                       server_pid=Server,
                                                       moves_made=MovesMade0,
                                                       players=Players}=State) -> 
    case CurrentPlayer =:= Player of
        false ->
            io:format("GEN_FSM GAME PLAYER: ~p CANNOT MOVE. NOT YOUR TURN.", [Player]),
            {reply, {error, not_your_turn}, wait_move, State};
        true -> 
            {ok, PlayerData} = maps:find(Player, Players),
            Mark = PlayerData#player_data.mark,
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
                    case process_move(Row, Col, Mark, State#state{board=NewBoard,
                                                                 moves_made=MovesMade}) of
                        win -> 
                            io:format("GEN_FSM GAME PLAYER: ~p WINS!!!~n", [Player]),
                            gen_fsm:send_event(OtherPlayerPid, game_end),
                            gen_server:cast(Server, {loose, NextPlayer}),
                            {reply, game_end, wait_new_game, State#state{board=game_board:new()}};
                        draw -> 
                            io:format("GEN_FSM GAME GAME IS DRAW~n"),
                            gen_fsm:send_event(OtherPlayerPid, game_end),
                            gen_server:cast(Server, {draw, NextPlayer}),
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

init([Server, GameName, PlayersSup, Player1, Player2]) ->
    io:format("~nGEN_FSM GAME =======INIT GAME FSM==============================~n"),
    io:format("GEN_FSM GAME GAME NAME: ~p~n", [GameName]),
    io:format("GEN_FSM GAME SELF: ~p~n", [self()]),
    {ok, Pid1} = supervisor:start_child(PlayersSup, [Player1, self()]),
    {ok, Pid2} = supervisor:start_child(PlayersSup, [Player2, self()]),
    Players0 = maps:new(),
    Players1 = maps:put(Player1, #player_data{pid=Pid1}, Players0),
    Players2 = maps:put(Player2, #player_data{pid=Pid2}, Players1),
    erlang:monitor(process, Pid1),
    erlang:monitor(process, Pid2),
    io:format("GEN_FSM GAME =======END INIT GAME FSM==============================~n~n"),
    random:seed(erlang:phash2([node()]),
                erlang:monotonic_time(),
                erlang:unique_integer()),
    {ok, register_mark, #state{game_name=GameName,
                              server_pid=Server,
                              players=Players2}}.

handle_event(stop, _State, Data) ->
    {stop, normal, Data};
handle_event(Event, StateName, StateData) ->
    io:format("GEN_FSM GAME Handle Event. Unknown event: ~p in fsm_state: ~p~n", [Event, StateName]),
    {next_state, test_state, StateData}.

handle_sync_event(Event, _From, StateName, StateData) ->
    io:format("GEN_FSM GAME Handle sync. Unknown event: ~p in fsm_state: ~p~n", [Event, StateName]),
    {next_state, test_state, StateData}.

handle_info({register_player, Player, Pid}, StateName, #state{players=Players0,
                                                             current_player=Current}=State) ->
    {ok, PlayerData} = maps:find(Player, Players0),
    io:format("~n+++++++++++++++++++++HERE ~p++++++++++++++++++~n", [StateName]),
    case Pid =:= PlayerData#player_data.pid of
        true ->
            io:format("~n+++++++++++++++++++++HERE ~p++++++++++++++++++~n", [StateName]),
            {next_state, StateName, State};
        false ->
            NewData = PlayerData#player_data{pid=Pid},
            Players1 = maps:update(Player, NewData, Players0),
            io:format("~n+++++++++++++++++++++HERE ~p++++++++++++++++++~n", [StateName]),
            case PlayerData#player_data.mark =:= undefined orelse Current =:= udefined of
                undefined ->
                    io:format("~n+++++++++++++++++++HERE ~p+++++++++++~n", [StateName]),
                    gen_fsm:send_all_state_event(Pid, {next_state, mark});
                _ ->
                    io:format("~n+++++++++++++++++++++HERE ~p++++++++++++++++++~n", [StateName]),
                    case Current =:= Player of
                        true ->
                            io:format("~n+++++++++++++++++++HERE ~p+++++++++++~n", [StateName]),
                            gen_fsm:send_all_state_event(Pid, {next_state, move});
                        false ->
                            io:format("~n+++++++++++++++++++HERE ~p+++++++++++~n", [StateName]),
                            gen_fsm:send_all_state_event(Pid, {next_state, wait})
                    end
            end,
            io:format("~n============HERE===============~n"),
            {next_state, StateName, State#state{players=Players1}}
    end;
handle_info({'DOWN', _Ref, _Type, _Object, _Info}, StateName, State) ->
    %io:format("TYPE: ~p~n", [Type]),
    %io:format("OBJECT: ~p~n", [Object]),
    %io:format("Info: ~p~n", [Info]),
    {next_state, StateName, State};
handle_info(Info, StateName, State) ->
    io:format("GEN_FSM GAME Unknown info: ~p, in state: ~p", [Info, StateName]),
    {next_state, test_state, State}.

code_change(_OldVsn, _StateName, _StateData, _Extra) -> ok.

terminate(Reason, StateName, #state{players=Players}) ->
    io:format("GEN_FSM GAME ===========GAME FSM terminate===============~n"),
    io:format("GEN_FSM GAME STATE: ~p~n",[StateName]),
    io:format("GEN_FSM GAME REASON: ~p~n",[Reason]),
    loop_players(Players, fun(_K, #player_data{pid=Pid}, Acc) -> 
                         gen_fsm:send_all_state_event(Pid, stop),
                         Acc
                 end),
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
    io:format("GAME_FSM PROCCESS MOVE~n"),
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

can_set_mark(Mark, PlayerName, Players) ->
    OpponentName = switch_players(Players, PlayerName),
    {ok, OpponentData} = maps:find(OpponentName, Players),
    {ok, PlayerData} = maps:find(PlayerName, Players),
    if
        OpponentData#player_data.mark =:= Mark ->
            {error, taken};
        PlayerData#player_data.mark =/= undefined ->
            {error, already_set};
        PlayerData#player_data.mark =:= undefined ->
            ok
    end.
