-module(game_serv).
-behaviour(gen_server).

-record(state,{
    queue = [] :: list(),
    q_size = 0 :: integer(),
    players = maps:new() :: map(),
    games_sup :: pid(),
    players_sup :: pid()
}).

-record(user, {
    player :: binary(),
    wins = 0 :: integer(),
    losses = 0 :: integer(),
    draws = 0 :: integer(),
    status = logged :: logged | in_q | in_game,
    game = undefined :: pid() | undefined
}).


-define(GAMES_SUP_SPEC(Args), #{
          id => games_sup,
          start => {games_sup, start_link, Args},
          restart => permanent,
          type => supervisor,
          shutdown => infinity,
          modules => [games_sup]
}).

-define(PLAYERS_SUP_SPEC(Args), #{
          id => players_sup,
          start => {players_sup, start_link, Args},
          restart => permanent,
          type => supervisor,
          shutdown => infinity,
          modules => [players_sup]
}).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_info/2, handle_cast/2,
         code_change/3, terminate/2]).

start_link(SupPid) ->
    gen_server:start_link({local, gs}, ?MODULE, [SupPid], []).

init([SupPid]) ->
    io:format("GAME SERVER =======GAME SERVER INIT===========~n"),
    io:format("GAME SERVER SERVER PID: ~p~n", [self()]),
    io:format("GAME SERVER SUPERVISOR PID: ~p~n", [SupPid]),
    io:format("GAME SERVER =======GAME SERVER END INIT=======~n"),
    self() ! matchmake,
    self() ! {spawn_supervisors, SupPid},
    {ok, #state{}}.

handle_call({login, Player}, _From, #state{players=Players} = S) when is_binary(Player) ->
    io:format("GAME SERVER ~p~n", [S]),
    case maps:is_key(Player, S#state.players) of
        true -> {reply, {err, user_logged}, S};
        false ->
            User=#user{player=Player},
            {reply, {ok, logged}, S#state{players=maps:put(Player, User, Players)}}
    end;
handle_call({set_in_q, Player}, _From, #state{queue=Q,
                                                players=Players,
                                                q_size=QSize}=State) ->
    CanQ = is_logged(Players, Player) andalso not is_in_game(Players, Player) andalso not is_in_q(Players, Player),
    case CanQ of
        true ->
            Players1 = set_player_status(Players, Player, in_q),
            {reply, {ok, set_in_q}, State#state{queue=Q ++ Player,
                                            q_size=QSize + 1,
                                            players=Players1}};
        false ->
            io:format("GAME SERVER Cannot set player in Q~n"),
            {reply, {err, cannot_set_in_q}, State}
    end;
handle_call(get_state, _From, State) ->
    {reply, {ok, State}, State};
%=======Start Game for testing purpose==================
handle_call({start_game, Player1, Player2}, _From, #state{games_sup=GamePoolPid,
                                                         players_sup=PlayersSup,
                                                         players=_PlayersPlayers1}=State) ->
    io:format("GAME SERVER GS: start game with players ~p ~p. Sup: ~p~n",[Player1,
                                                                          Player2,
                                                                          GamePoolPid]),
    {ok, Game} = supervisor:start_child(GamePoolPid, [<<"MyGame">>, PlayersSup, Player1, Player2]),
    [Player1Pid, Player2Pid] = gen_fsm:sync_send_all_state_event(Game, get_players),
    {reply, {Player1Pid, Player2Pid}, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast({game_end, [P1, P2]}, #state{players=Players0}=State) ->
    Players1 = set_player_status(Players0, P1, logged),
    Players2 = set_player_status(Players1, P2, logged),
    {noreply, State#state{players=Players2}};
handle_cast({logout, Player}, #state{players=Players,
                                    queue=Q,
                                    q_size=QSize}=State) ->
    case maps:find(Player, Players) of
        error -> ok;
        {ok, PlayerData} -> 
            case PlayerData#user.status of
                in_game ->
                    gen_fsm:sync_send_all_state_event(PlayerData#user.game, {logout, Player}),
                    {noreply, State#state{players=maps:remove(Player, Players)}};
                in_queue -> 
                    NewQ = lists:delete(Player, Q),
                    {noreply, State#state{queue=NewQ,
                                         q_size=QSize-1,
                                         players=maps:remove(Player, Players)}};
                logged -> {noreply, State#state{players=maps:remove(Player, Players)}}
            end
    end;
handle_cast({win, Player}, #state{players=Players}=State) ->
    io:format("~nMAIN SERVER: SAVE WIN FOR: ~p~n", [Player]),
    {ok, PlayerData} = maps:find(Player, Players),
    io:format("PLAYER DATA: ~p~n", [PlayerData]),
    Wins = PlayerData#user.wins,
    Players1 = maps:update(Player, PlayerData#user{wins=Wins+1}, Players),
    io:format("~nSAVED WIN FOR: ~p~n", [Player]),
    {noreply, State#state{players=Players1}};
handle_cast({loose, Player}, #state{players=Players}=State) ->
    io:format("~nSAVE LOSE FOR: ~p~n", [Player]),
    {ok, PlayerData} = maps:find(Player, Players),
    Losses = PlayerData#user.losses,
    Players1 = maps:update(Player, PlayerData#user{losses=Losses+1}, Players),
    {noreply, State#state{players=Players1}};
handle_cast({draws, Player}, #state{players=Players}=State) ->
    io:format("~nSAVE DRAW FOR: ~p~n", [Player]),
    {ok, PlayerData} = maps:find(Player, Players),
    Draws = PlayerData#user.draws,
    Players1 = maps:update(Player, PlayerData#user{draws=Draws+1}, Players),
    {noreply, State#state{players=Players1}};
handle_cast(_Msg, _State) ->
    {noreply, _State}.

handle_info({spawn_supervisors, FatherSupPid}, State) ->
    io:format("~n==========GAME SERVER SPAWN SUPERVISORS==========~n"),
    {ok, PlayersSupPid} = supervisor:start_child(FatherSupPid, ?PLAYERS_SUP_SPEC([])),
    {ok, GamesSupPid} = supervisor:start_child(FatherSupPid, ?GAMES_SUP_SPEC([self()])),
    io:format("~n==========GAME SERVER END SPAWN SUPERVISORS======~n~n"),
    {noreply, State#state{games_sup=GamesSupPid, players_sup=PlayersSupPid}};
handle_info(matchmake, #state{q_size=QSize}=State) when QSize < 2->
    %io:format("GAME SERVER Matchmake < 2~n"),
    timer:send_after(5000, matchmake),
    {noreply, State};
handle_info(matchmake, #state{q_size=QSize,
                            queue=[P1, P2|T],
                            players=Players,
                            players_sup=PlayersSup,
                            games_sup=GamePoolPid}=State) ->
    supervisor:start_child(GamePoolPid, [<<"MyGame">>, PlayersSup, P1, P2]),
    Players1 = set_player_status(Players, P1, in_game),
    Players2 = set_player_status(Players1, P2, in_game),
    timer:send_after(5000, matchmake),
    {noreply, State#state{queue=T,
                          q_size=QSize-2,
                         players=Players2}};
handle_info(Msg, _State) ->
    io:format("GAME SERVER MSG: ~p~n", [Msg]),
    {noreply, _State}.

terminate(normal, _State) ->
    io:format("GAME SERVER Tic-Tac-Toe Game Server terminating~n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_in_game(Players, Player) -> 
    {ok, PlayerData} = maps:find(Player, Players),
    case PlayerData#user.status of
        in_game -> true;
        _ -> false
    end.

is_in_q(Players, Player) -> 
    {ok, PlayerData} = maps:find(Player, Players),
    case PlayerData#user.status of
        in_q -> true;
        _ -> false
    end.

is_logged(Players, Player) -> maps:is_key(Player, Players).

set_player_status(Players, Player, Status) ->
    {ok, PlayerData} = maps:find(Player, Players),
    maps:update(Player, PlayerData#user{status=Status}, Players).

