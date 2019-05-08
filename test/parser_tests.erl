-module(parser_tests).
-include_lib("eunit/include/eunit.hrl").

parser_test() ->
  ValidResult1 = #{
    acqId => <<"123456">>, amount => <<"100.00">>,
    cDate => <<"0804">>, date => <<"0804">>,
    dateTime => <<"0804030013">>,pan => <<"0000011319353459">>,
    prCode => <<"011000">>,refNum => <<"192165102801">>,
    sNum => <<"051028">>,time => <<"083013">>
  },
  ValidResult2 = #{
    acqId => <<"123456">>, amount => <<"200.00">>,
    cDate => <<"0804">>, date => <<"0804">>,
    dateTime => <<"0804030013">>, pan => <<"0000011319353459">>,
    prCode => <<"011000">>, refNum => <<"192165102801">>,
    sNum => <<"051028">>, time => <<"083013">>
  },
  ValidResult3 = #{
    acqId => <<"123456">>, amount => <<"400.00">>,
    cDate => <<"0804">>, date => <<"0804">>,
    dateTime => <<"0804030013">>, pan => <<"0000011319353459">>,
    prCode => <<"011000">>, refNum => <<"192165102801">>,
    sNum => <<"051028">>, time => <<"083013">>
  },
  ValidResult4 = #{
    acqId => <<"123456">>, amount => <<"600.10">>,
    cDate => <<"0804">>, date => <<"0804">>,
    dateTime => <<"0804030013">>, pan => <<"0000011319353459">>,
    prCode => <<"011000">>, refNum => <<"192165102801">>,
    sNum => <<"051028">>, time => <<"083013">>
  },

  %% processing binaries
  ?assertEqual(parser:parse_data(<<"0000011319353459011000000000010000080403001305102808301308040804123456123456192165102801">>), ValidResult1),
  ?assertEqual(parser:parse_data(<<"0000011319353459011000000000020000080403001305102808301308040804123456123456192165102801">>), ValidResult2),
  ?assertEqual(parser:parse_data(<<"0000011319353459011000000000040000080403001305102808301308040804123456123456192165102801">>), ValidResult3),
  ?assertEqual(parser:parse_data(<<"0000011319353459011000000000060010080403001305102808301308040804123456123456192165102801">>), ValidResult4),

  %% processing strings
  ?assertEqual(parser:parse_data("0000011319353459011000000000010000080403001305102808301308040804123456123456192165102801"), ValidResult1),
  ?assertEqual(parser:parse_data("0000011319353459011000000000020000080403001305102808301308040804123456123456192165102801"), ValidResult2),
  ?assertEqual(parser:parse_data("0000011319353459011000000000040000080403001305102808301308040804123456123456192165102801"), ValidResult3),
  ?assertEqual(parser:parse_data("0000011319353459011000000000060010080403001305102808301308040804123456123456192165102801"), ValidResult4),

  %% other data types are not allowed
  ?assertEqual(parser:parse_data(0000011319353459011000000000010000080403001305102808301308040804123456123456192165102801), invalid_data),
  ?assertEqual(parser:parse_data(0000011319353459011000000000020000080403001305102808301308040804123456123456192165102801), invalid_data),
  ?assertEqual(parser:parse_data(0000011319353459011000000000040000080403001305102808301308040804123456123456192165102801), invalid_data),
  ?assertEqual(parser:parse_data(0000011319353459011000000000060010080403001305102808301308040804123456123456192165102801), invalid_data),

  % data that doesn't match the spec is also not allowed
  ?assertEqual(parser:parse_data(<<"000001131935345901100000010000080403001305102808301308040804123456123456192165102801">>), invalid_data).
