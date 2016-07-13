-module(tic_tac_toe_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, {name, ServName}) ->
    io:format("Starting app~n"),
    tic_tac_toe_sup:start_link(ServName).

stop(_State) ->
    ok.
