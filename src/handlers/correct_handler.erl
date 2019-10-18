%%%-------------------------------------------------------------------
%%% @author ivan
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. окт. 2019 15:41
%%%-------------------------------------------------------------------
-module(correct_handler).
-author("ivan").

-behaviour(cowboy_handler).

-include("test_web_server.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
  init/2,
  terminate/3
]).

-define(DEFAULT_PROP_MAP, #{
  from => 0,
  to => 99999999999999999999
}).

%%%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
%%% Cowboy_http_handler API
%%%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

init(Req, State) ->
  try
    case {cowboy_req:method(Req), cowboy_req:path(Req)} of
      {<<"GET">>, <<"/visited_domains">>} -> do(get, Req, State);
      {<<"POST">>, <<"/visited_links">>} -> do(post, Req, State);
      _ -> do(404, Req, State)
    end
  catch
    _:Error ->
      do(lists:flatten(io_lib:format("~p", [Error])), Req, State)
  end.

terminate(_Reason, _Req, _State) ->
  ok.

%%%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
%%% Internal Functions
%%%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

do(404, Req, State) ->
  JSON = make_json(404, ""),
  Req2 = cowboy_req:reply(404, #{}, JSON, Req),
  {ok, Req2, State};

do(post, Req0, State) ->
  {ok, Data, Req1} = cowboy_req:read_body(Req0),
  InJSON = jsone:decode(Data),
  TS = convert_time_stamp(),
  {ok, _} = eredis:q(State#handle_state.redis_pid, ["zadd", "visited_domains" | attach_timestamp(TS, maps:get(<<"links">>, InJSON))]),
  JSON = make_json(ok, ""),
  Req2 = cowboy_req:reply(200, #{}, JSON, Req1),
  {ok, Req2, State};

do(get, Req, State) ->
  QS = cowboy_req:qs(Req),
  ListOfProperty = binary:split(QS, <<"&">>),
  Props = generate_property_map(ListOfProperty, ?DEFAULT_PROP_MAP),
  {ok, Output} = eredis:q(State#handle_state.redis_pid, ["zrangebyscore", "visited_domains", maps:get(from, Props), maps:get(to, Props)]),
  JSON = make_json(visited_domains, lists:join(<<",\n">>, Output)),
  Req2 = cowboy_req:reply(200, #{}, JSON, Req),
  {ok, Req2, State};

do(Error, Req, State) ->
  JSON = make_json(erlang:list_to_binary(Error), ""),
  Req2 = cowboy_req:reply(500, #{}, JSON, Req),
  {ok, Req2, State}.
%%%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

generate_property_map([], Props) -> Props;
generate_property_map([PropertyBin | Tail], Acc) ->
  case binary:split(PropertyBin, <<"=">>) of
    [<<"from">>, Time] -> generate_property_map(Tail, Acc#{from => Time});
    [<<"to">>, Time] -> generate_property_map(Tail, Acc#{to => Time});
    _ -> generate_property_map(Tail, Acc)
  end.
%%%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

make_json(ok, _) -> <<"{\n  \"status\" : \"ok\"\n}">>;
make_json(404, _) -> <<"{\n  \"status\" : \"404\"\n}">>;
make_json(visited_domains, Output) ->
  Fun =
    fun (_, Acc, []) -> Acc;
        (F, Acc, [<<",\n">> | Tail]) -> F(F, <<Acc/binary, ",\n">>, Tail);
        (F, Acc, [B | Tail]) -> F(F, <<Acc/binary, "    \"", B/binary, "\"">>, Tail)
    end,
  DomainsList = Fun(Fun, <<>>, Output),
  <<"{\n  \"domains\" : [\n", DomainsList/binary, "\n  ],\n  \"status\" : \"ok\"\n}">>;
make_json(Error, _) ->
  <<"{\n  \"status\" : \"", Error/binary, "\"\n}">>.
%%%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

convert_time_stamp() ->
  {A, B, _} = erlang:timestamp(),
  A1 = erlang:integer_to_binary(A),
  B1 = erlang:integer_to_binary(B),
  <<A1/binary, B1/binary>>.
%%%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

attach_timestamp(_, []) -> [];
attach_timestamp(TS, [Link | Tail]) ->
  [TS, Link | attach_timestamp(TS, Tail)].
%%%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
%%% Tests
%%%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

make_json_1_test() ->
  Output = make_json(visited_domains, [<<",\n">>, <<"someone">>]),
  Expected = <<"{\n  \"domains\" : [\n,\n    \"someone\"\n  ],\n  \"status\" : \"ok\"\n}">>,
  ?assertEqual(Expected, Output).
make_json_2_test() ->
  Output = make_json(<<"someone">>, ""),
  Expected = <<"{\n  \"status\" : \"someone\"\n}">>,
  ?assertEqual(Expected, Output).

generate_property_map_1_test() ->
  Output = generate_property_map([<<"trash=12345">>, <<"trash">>, <<"from54321">>, <<"to54321">>], ?DEFAULT_PROP_MAP),
  ?assertEqual(?DEFAULT_PROP_MAP, Output).
generate_property_map_2_test() ->
  Output = generate_property_map([<<"from=12345">>, <<"to=12345">>], ?DEFAULT_PROP_MAP),
  Expected = #{from => <<"12345">>, to => <<"12345">>},
  ?assertEqual(Expected, Output).

convert_time_stamp_1_test() ->
  Output = convert_time_stamp(),
  ?assertEqual(true, erlang:is_binary(Output)).
convert_time_stamp_2_test() ->
  {A, B, _} = erlang:timestamp(),
  Expected = erlang:integer_to_list(A) ++ erlang:integer_to_list(B),
  Output = erlang:binary_to_list(convert_time_stamp()),
  ?assertEqual(Expected, Output).
%%%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
%%% End
%%%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
