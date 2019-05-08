-module(db_client).
-define(HOST, "http://localhost:5984/").
-define(BALANCE_DB, "balance/").
-define(DEFAULT_HEADERS, [{"Accept", "application/json"}]).
-define(CONTENT_TYPE, "application/json").

%% API
-export([request/1]).

request(Body) ->
  BinBody = binarize(Body),
  {ok, {_Status, _Headers, Resp}} =
    httpc:request(put, {put_balance_url(), ?DEFAULT_HEADERS, ?CONTENT_TYPE, jsone:encode(BinBody)}, [], []),
  case jsone:decode(list_to_binary(Resp)) of
    #{<<"error">> := Error, <<"reason">> := Reason} ->
      {Error, Reason};
    #{<<"ok">> := true, <<"id">> := Id} ->
      {ok, Id}
  end.

put_balance_url() ->
  balance_url() ++ generate_key().

balance_url() ->
  ?HOST ++ ?BALANCE_DB.

generate_key() ->
  <<A:32, B:16, C:16, D:16, E:48>> = crypto:strong_rand_bytes(16),
  io_lib:format("~8.16.0b~4.16.0b4~3.16.0b~4.16.0b~12.16.0b",
    [A, B, C band 16#0fff, D band 16#3fff bor 16#8000, E]).

binarize(Data) ->
  binarize(Data, []).

binarize([], Acc) ->
  Acc;
binarize([{Key, Value}|OtherTuple], Acc) ->
  binarize(OtherTuple, [{to_binary(Key), to_binary(Value)}|Acc]).

to_binary(Data) when is_binary(Data)  -> Data;
to_binary(Data) when is_list(Data)    -> list_to_binary(Data);
to_binary(Data) when is_atom(Data)    -> atom_to_binary(Data, utf8);
to_binary(Data) when is_integer(Data) -> integer_to_binary(Data);
to_binary(Data) when is_float(Data)   -> float_to_binary(Data, [{decimals, 2}]).