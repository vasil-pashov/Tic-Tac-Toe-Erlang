-module(player).

-export([choose_mark/2, make_move/3]).

choose_mark(PlayerPid, Mark) ->
    case gen_fsm:sync_send_event(PlayerPid, {register_mark, Mark}) of
        {error, ErrorMsg} -> 
            io:format("Error: ~p.~n", [ErrorMsg]);
        {ok, mark_set} -> 
            io:format("You have choosen mark: ~p.~n", [Mark])
    end.

make_move(PlayerPid, Row, Col) ->
    case gen_fsm:sync_send_event(PlayerPid, {make_move, Row, Col}) of
        {ok, game_end} -> io:format("MOVE ACCEPTED. GAME ENDS.~n");
        {ok, continue} -> io:format("MOVE ACCEPTED. OPPONENTS TURN NOW.~n");
        {error, ErrorMsg} -> io:format("ERROR MOVE NOT ACCEPTED. ERROR: ~p.~n", [ErrorMsg])
    end.


