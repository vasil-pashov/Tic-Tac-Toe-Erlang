-module(player).

-export([choose_mark/2, make_move/3]).

choose_mark(PlayerPid, Mark) ->
    case gen_fsm:sync_send_event(PlayerPid, {set_mark, Mark}) of
        {error, taken} -> 
            io:format("This mark is not free. Try again.~n");
        {ok, mark_set, 1} -> 
            io:format("You have choosen mark: ~p. You are to make the first move.~n", [Mark]);
        {ok, mark_set, 2} -> 
            io:format("You have choosen mark: ~p. You are second.~n", [Mark])
    end.

make_move(PlayerPid, Row, Col) ->
    case gen_fsm:sync_send_event(PlayerPid, {make_move, Row, Col}) of
        {ok, game_end} -> io:format("MOVE ACCEPTED. GAME ENDS.~n");
        {ok, continue} -> io:format("MOVE ACCEPTED. OPPONENTS TURN NOW.~n");
        {error, ErrorMsg} -> io:format("ERROR MOVE NOT ACCEPTED. ERROR: ~p.~n", [ErrorMsg])
    end.


