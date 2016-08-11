-module(game_serv).
-behaviour(gen_server).

-record(state,{
    queue = queue:new(),
    q_size = 0 :: integer(),
    players = maps:new() :: map(),
    players_proc = maps:new() :: map(),
    games_sup :: pid(),
    players_sup :: pid()
}).

-record(user, {
    username :: binary(),
    wins = 0 :: integer(),
    losses = 0 :: integer(),
    draws = 0 :: integer(),
    status = logged :: logged | in_queue | in_game,
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
          modules => [games_sup]
}).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_info/2, handle_cast/2,
         code_change/3, terminate/2]).

start_link(SupPid) ->
    io:format("GAME SERVER =======game_serv start link=================~n"),
    gen_server:start_link({local, gs}, ?MODULE, [SupPid], []).

init([SupPid]) ->
    io:format("GAME SERVER =======game_serv game server init===========~n"),
    io:format("GAME SERVER SERVER PID: ~p~n", [self()]),
    io:format("GAME SERVER SUPERVISOR PID: ~p~n", [SupPid]),
    io:format("GAME SERVER ============================================~n"),
    self() ! matchmake,
    self() ! {spawn_supervisors, SupPid},
    {ok, #state{}}.

handle_call({login, Username}, _From, #state{players=Players} = S) when is_binary(Username) ->
    io:format("GAME SERVER ~p~n", [S]),
    case maps:is_key(Username, S#state.players) of
        true -> {reply, {err, user_logged}, S};
        false ->
            User=#user{username=Username},
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
                                                         players=Players,
                                                         players_proc=PProc}=State) ->
    io:format("GAME SERVER GS: start game with players ~p ~p. Sup: ~p~n",[Player1, Player2, GamePoolPid]),
    {ok, Game} = supervisor:start_child(GamePoolPid, [<<"MyGame">>]),
    {ok, P1} = supervisor:start_child(PlayersSup, []),
    {ok, P2} = supervisor:start_child(PlayersSup, []),
    PProc1 = maps:put(P1, Player1, PProc),
    PProc2 = maps:put(P2, Player1, PProc1),
    Players1 = maps:put(Player1, #user{username=Player1,
                                       status=in_game,
                                       game=Game}, Players),
    Players2 = maps:put(Player1, #user{username=Player2,
                                       status=in_game,
                                       game=Game}, Players1),
    {reply, {ok, State}, State#state{players=Players2,
                                    players_proc=PProc2}};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast({forward_to_game, PlayerName, PlayerPid, Command}, {players=Players}=State) ->
    PlayerData = maps:find(PlayerName, Players),
    Game = PlayerData#user.game,
    gen_fsm:send_event(Game, {Command, PlayerPid}),
    {noreply, State};
handle_cast(_Msg, _State) ->
    {noreply, _State}.

handle_info({spawn_supervisors, FatherSupPid}, State) ->
    io:format("GAME SERVER SPAWN PLAYERS SUPERVISOR~n"),
    {ok, PlayersSupPid} = supervisor:start_child(FatherSupPid, ?PLAYERS_SUP_SPEC([])),
    io:format("GAME SERVER PLAYERS SUPERVISOR PID: ~p~n",[PlayersSupPid]),
    io:format("GAME SERVER SPAWN GAMES SUPERVISOR~n"),
    {ok, GamesSupPid} = supervisor:start_child(FatherSupPid, ?GAMES_SUP_SPEC([self()])),
    io:format("GAME SERVER GAMES SUPERVISOR PID: ~p~n",[GamesSupPid]),
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

