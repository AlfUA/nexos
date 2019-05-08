-module(parser).

%% API exports
-export([parse_data/1]).
%%====================================================================
%% API functions
%%====================================================================
-spec parse_data(<<_:_*8>>) -> map() | invalid_data.
parse_data(<<Pan:16/binary, PrCode:6/binary, Amount:12/binary, DateTime:10/binary,
          SNum:6/binary, Time:6/binary, Date:4/binary, CDate:4/binary,
          AcqId:6/binary, _IgnoredVal:6/binary, RefNum:12/binary>> = Data) when is_binary(Data)->
  #{
    pan => Pan,
    prCode => PrCode,
    amount => get_amount(Amount),
    dateTime => DateTime,
    sNum => SNum,
    time => Time,
    date => Date,
    cDate => CDate,
    acqId => AcqId,
    refNum => RefNum
  };
parse_data(Data) when is_list(Data) ->
  parse_data(list_to_binary(Data));
parse_data(_) ->
  invalid_data.

%%====================================================================
%% Internal functions
%%====================================================================
get_amount(<<"0", Rest/binary>>) ->
  get_amount(Rest);
get_amount(Amount) ->
  get_fract_amount(Amount).

get_fract_amount(Amount) ->
  IntSize = byte_size(Amount) - 2,
  <<Int:IntSize/binary, Fraction:2/binary>> = Amount,
  <<Int/binary, ".", Fraction/binary>>.
