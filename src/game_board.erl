-module(game_board).

-export([new/0, get/3, set/4]).

new() -> lists:duplicate(9, 0).

get(Row, Col, List) ->
    case get_index(Row, Col) of
        {ok, Index} -> {ok, lists:nth(Index, List)};
        {error, wrong_coords} -> {error, wrong_coords}
    end.

set(Row, Col, Mark, List) ->
    case get_index(Row, Col) of 
        {ok, Index} -> set(Index, Mark, List);
        {error, wrong_coords}=Rez -> Rez
    end.
        
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_index(Row, Col) ->
    case is_in_range(Row) andalso is_in_range(Col) of
        true ->
            Index = (Row - 1) * 3 + Col,
            {ok, Index};
        false -> {error, wrong_coords}
    end.

is_in_range(El) ->
    El >= 1 andalso El =< 3.

is_free(Index, List) ->
    case lists:nth(Index, List) of
        0 -> true;
        _ -> false
    end.

set(Index, Mark, List) ->
    case is_free(Index, List) of
        true -> 
            {Beg, [_|End]} = lists:split(Index-1, List),
            {ok, lists:merge(Beg, [Mark|End])};
        false -> {error, field_not_free}
    end.
