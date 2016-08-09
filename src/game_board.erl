-module(game_board).

-export([new/0, get/3, set/4, check_win/5, print/1]).
-define(SIZE, 3).
-define(RIGHT, 1).
-define(LEFT, -1).
-define(MAIN_DIAG_START, 1).

new() -> lists:duplicate(3, lists:duplicate(3,0)).

get(Row, Col, Board) ->
    case is_in_range({Row, Col}) of
        true -> {ok, lists:nth(Col, lists:nth(Row, Board))};
        false -> {error, wrong_coords}
    end.

set(Row, Col, Mark, Board) ->
    case is_in_range({Row, Col}) of 
        true ->
            {PrefixRow, [CurrentRow|TailRow]} = lists:split(Row-1, Board),
            {PrefixCol, [CurrentEl|TailCol]} = lists:split(Col-1, CurrentRow),
            case is_free(CurrentEl) of
                true ->  
                    NewRow = PrefixCol ++ [Mark|TailCol],
                    NewBoard = PrefixRow ++ [NewRow|TailRow],
                    {ok, NewBoard};
                false -> {error, not_free}
            end;
        false -> {error, wrong_coords}
    end.

check_win(Row, Col, Mark, Board, MovesMade) -> 
    io:format("GAME BOARD CHECK WIN~n"),
    io:format("GAME BOARD ROW: ~p~n", [Row]),
    io:format("GAME BOARD COL: ~p~n", [Col]),
    io:format("GAME BOARD MARK: ~p~n", [Mark]),
    io:format("GAME BOARD MOVES: ~p~n", [MovesMade]),
    io:format("GAME BOARD BOARD: ~n"),
    print(Board),
    WinRow = check_row_win(Row, Mark, Board),
    WinCol = check_col_win(Col, Mark, Board),
    WinDiag = check_diagonals_win(Row, Col, Mark, Board),
    Win = WinRow or WinCol or WinDiag,
    case Win =:= false andalso MovesMade =:= 9 of
        true -> draw;
        false -> Win
    end.
        


print(Board) ->
    lists:foreach(fun(Row) ->
                          lists:foreach(fun(Element) ->
                                                io:format("~p ", [Element])
                                        end, Row),
                        io:format("~n")
                  end, Board).
                          

        
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_row_win(RowNumber, Mark, Board) ->
    Row = lists:nth(RowNumber, Board),
    check_lists_for_win(Row, Mark).

check_col_win(_ColNumber, _Mark, []) -> true;
check_col_win(ColNumber, Mark, [H|T]) -> 
    case lists:nth(ColNumber, H) =:= Mark of
        true -> check_col_win(ColNumber, Mark, T);
        false -> false
    end.

check_diagonals_win(Row, Col, Mark, Board) ->
    MainWin = case is_on_main_diagonal(Row, Col) of
                  false -> false;
                  true -> check_diag_win(?MAIN_DIAG_START, Mark, Board, ?RIGHT)
              end,
    SecondaryWin = case is_on_secondary_diagonal(Row, Col) of
                       false -> false;
                       true -> check_diag_win(?SIZE, Mark, Board, ?LEFT)
                   end,
    MainWin or SecondaryWin.

check_diag_win(_Position, _Mark, [], _Direction) -> true;
check_diag_win(Position, Mark, [H|T], Direction) ->
    case lists:nth(Position, H) =:= Mark of 
        true -> check_diag_win(Position + Direction, Mark, T, Direction);
        false -> false
    end.
    
check_lists_for_win([Mark, Mark, Mark], Mark) -> true;
check_lists_for_win(_List, _Mark) -> false.

is_in_range({Row, Col}) -> is_in_range(Row) andalso is_in_range(Col);
is_in_range(El) ->
    El >= 1 andalso El =< 3.

is_on_main_diagonal(Row, Col) ->
    case Row =:= Col of
        true -> true;
        false -> false
    end.

is_on_secondary_diagonal(Row, Col) ->
    case Row + Col =:= ?SIZE + 1 of
        true -> true;
        false -> false
    end.

is_free(El) ->
    case El of
        0 -> true;
        _ -> false
    end.

