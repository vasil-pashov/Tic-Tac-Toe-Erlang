-module(game_serv).
-behaviour(gen_server).

-record(state,{
    queue = queue:new(),
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
    status = logged :: logged | in_queue | in_game
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

handle_call({login, Username}, _From, #state{players=Players} = S) when is_binary(Username) ->
    io:format("GAME SERVER ~p~n", [S]),
    case maps:is_key(Username, S#state.players) of
        true -> {reply, {err, user_logged}, S};
        false ->
            User=#user{player=Username},
            {reply, {ok, logged}, S#state{players=maps:put(Username, User, Players)}}
    end;
handle_call({logout, _Username}, _From, #state{players=_Players}) ->
    io:format("GAME SERVER TO DO: LOGOUT");
handle_call({set_in_q, Username}, _From, #state{queue=Q,
                                                players=Players,
                                                q_size=QSize}=State) ->
    CanQ = not is_in_game(Players, Username) and not is_in_q(Players, Username) and is_logged(Players, Username),
    case CanQ of
        true ->
            {reply, {ok, set_in_q}, State#state{queue=queue:in(Username, Q), q_size=QSize + 1}};
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
    supervisor:start_child(GamePoolPid, [<<"MyGame">>, PlayersSup, Player1, Player2]),
    {reply, {ok, State}, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

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
                              queue=Q,
                              players=_Players}=State) ->
    %io:format("GAME SERVER Matchmake > 2~n"),
    {{value, _P1}, Q1} = queue:out(Q),
    {{value, _P2}, Q2} = queue:out(Q1),
    timer:send_after(5000, matchmake),
    {noreply, State#state{queue=Q2, q_size=QSize-2}};
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

is_in_game(IngamePlayers, Username) -> maps:is_key(Username, IngamePlayers).

is_in_q(Q, Username) -> lists:member(Username, Q).

is_logged(Logged, Username) -> maps:is_key(Username, Logged).

