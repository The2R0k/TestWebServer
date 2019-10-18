-module(test_web_server_app).

-behaviour(application).

-include("test_web_server.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, PID} = eredis:start_link(),
    Dispatch = cowboy_router:compile([
        {'_', [
            {'_', correct_handler, #handle_state{redis_pid = PID}}]}
    ]),
    {ok, _} = cowboy:start_clear(?MODULE, [{port, 8080}], #{env => #{dispatch => Dispatch}}),
    test_web_server_sup:start_link().

stop(_State) ->
    ok.
