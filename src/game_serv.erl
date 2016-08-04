-module(game_serv).
-behaviour(gen_server).

-record(state,{
    in_game = maps:new() :: map(),
    in_queue = [],
    q_size = 0 :: integer(),
    logged = maps:new() :: map(),
    games_sup :: pid(),
    players_sup :: pid()
}).

-record(user, {
    username :: binary(),
    wins = 0 :: integer(),
    losses = 0 :: integer(),
    draws = 0 :: integer()
}).

-define(GAMES_SUP_SPEC(Args), #{
          id => games_sup,
          start => {games_sup, start_link, [Args]},
          restart => permanent,
          type => supervisor,
          shutdown => infinity,
          modules => [games_sup]
}).

-define(PLAYERS_SUP_SPEC(Args), #{
          id => players_sup,
          start => {players_sup, start_link, [Args]},
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

handle_call({login, Username}, _From, #state{logged=Logged} = S) when is_binary(Username) ->
    io:format("GAME SERVER ~p~n", [S]),
    case maps:is_key(Username, S#state.logged) of
        true -> {reply, {err, user_logged}, S};
        false ->
            User=#user{username=Username},
            {reply, {ok, logged}, S#state{logged=maps:put(Username, User, Logged)}}
    end;
handle_call({logout, _Username}, _From, #state{logged=_Logged, in_game=_InGame, in_queue=_Q}) ->
    io:format("GAME SERVER TO DO: LOGOUT");
handle_call({set_in_q, Username}, _From, #state{in_queue=Q, in_game=InGame,
                                         logged=Logged, q_size=QSize}=State) ->
    CanQ = not is_in_game(InGame, Username) and not is_in_q(Q, Username) and is_logged(Logged, Username),
    case CanQ of
        true ->
            {reply, {ok, set_in_q}, State#state{in_queue=[Username|Q], q_size=QSize + 1}};
        false ->
            io:format("GAME SERVER Cannot set player in Q~n"),
            {reply, {err, cannot_set_in_q}, State}
    end;
handle_call(get_state, _From, State) ->
    {reply, {ok, State}, State};
%=======Start Game for testing purpose==================
handle_call({start_game, Player1, Player2}, _From, #state{games_sup=GamePoolPid}=State) ->
    io:format("GAME SERVER GS: start game with players ~p ~p. Sup: ~p~n",[Player1, Player2, GamePoolPid]),
    supervisor:start_child(GamePoolPid, [[<<"MyGame">>, Player1, Player2, self()]]),
    {reply, {ok, State}, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, _State) ->
    {noreply, _State}.

handle_info({spawn_supervisors, FatherSupPid}, State) ->
    io:format("GAME SERVER SPAWN GAMES SUPERVISOR~n"),
    GamesSupPid = spawn_sup(FatherSupPid, ?GAMES_SUP_SPEC(self())),
    io:format("GAME SERVER GAMES SUPERVISOR PID: ~p~n",[GamesSupPid]),
    io:format("GAME SERVER SPAWN PLAYERS SUPERVISOR~n"),
    PlayersSupPid = spawn_sup(FatherSupPid, ?PLAYERS_SUP_SPEC(self())),
    io:format("GAME SERVER PLAYERS SUPERVISOR PID: ~p~n",[PlayersSupPid]),
    {noreply, State#state{games_sup=GamesSupPid, players_sup=PlayersSupPid}};
handle_info(matchmake, #state{q_size=QSize}=State) when QSize < 2->
    %io:format("GAME SERVER Matchmake < 2~n"),
    timer:send_after(5000, matchmake),
    {noreply, State};
handle_info(matchmake, #state{q_size=QSize, in_queue=[P1, P2|T],
                              in_game=InGame, logged=Logged}=State) ->
    %io:format("GAME SERVER Matchmake > 2~n"),
    InGame1 =  set_player_in_game(P1, InGame, Logged),
    InGame2 = set_player_in_game(P2, InGame1, Logged),
    timer:send_after(5000, matchmake),
    {noreply, State#state{in_queue=T, in_game=InGame2, q_size=QSize-2}};
handle_info(Msg, _State) ->
    io:format("GAME SERVER MSG: ~p", [Msg]),
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

set_player_in_game(Username, InGame, Logged) ->
    maps:put(Username, maps:get(Username, Logged), InGame).

spawn_sup(Sup, Specs) ->
    {ok, Pid} = supervisor:start_child(Sup, Specs),
    Pid.
