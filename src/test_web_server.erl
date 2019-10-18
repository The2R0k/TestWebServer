%%%-------------------------------------------------------------------
%%% @author ivan
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. окт. 2019 17:44
%%%-------------------------------------------------------------------
-module(test_web_server).
-author("ivan").

-include_lib("eunit/include/eunit.hrl").

%% API
-export([
  start/0,
  stop/0
]).

-define(APPS, [crypto, asn1, public_key, ssl, cowlib, ranch, cowboy, test_web_server]).
-define(APPS_REVERSE, lists:reverse(?APPS)).

start() ->
  ok = ensure_started(?APPS),
  ok = sync:go(),
  io:format("~nAll services started. Port: 8080~n").

stop() ->
  sync:stop(),
  ok = stop_apps(?APPS_REVERSE).

ensure_started([]) -> ok;
ensure_started([App | Tail]) ->
  case application:start(App) of
    ok -> ensure_started(Tail);
    {error, {already_started, App}} -> ensure_started(Tail)
  end.

stop_apps([]) -> ok;
stop_apps([App | Tail]) ->
  application:stop(App),
  stop_apps(Tail).

%%%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
%%% Tests
%%%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

start_test() ->
  ?assertEqual(ok, ensure_started(?APPS)).
%%%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
%%% End
%%%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
