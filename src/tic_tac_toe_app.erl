-module(tic_tac_toe_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _Args) ->
    io:format("Starting the app~n"),
    tic_tac_toe_sup:start_link([gs]).

stop(_State) ->
    ok.
