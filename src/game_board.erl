-module(game_board).

-export([new/0, get/3, set/4]).

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
                    NewRow = lists:merge(PrefixCol, [Mark|TailCol]),
                    {ok, lists:merge(PrefixRow, [NewRow|TailRow])};
                false -> {error, not_free}
            end;
        false -> {error, wrong_coords}
    end.

win(Mark, Row, Col, Board) -> ok.

        
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

check_diag_win(_Mark, [], _El, _Direction) -> true;
check_diag_win(Mark, [H|T], El, Direction) ->
    case lists:nth(El, H) =:= Mark of 
        true -> check_diag_win(Mark, T, El + Direction, Direction);
        false -> false
    end.
    
check_lists_for_win([Mark, Mark, Mark], Mark) -> true;
check_lists_for_win(_List, _Mark) -> false.

is_in_range({Row, Col}) -> is_in_range(Row) andalso is_in_range(Col);
is_in_range(El) ->
    El >= 1 andalso El =< 3.

is_free(El) ->
    case El of
        0 -> true;
        _ -> false
    end.

