-module(db_client_tests).

-include_lib("eunit/include/eunit.hrl").

db_client_test() ->
  Client1 = [{fname1, "Bob"}, {lname1, "Smith"}, {account1_balance, 1323.34}, {account1_id, 10000000000}],
  Client2 = [{fname2, "Larry"}, {lname2, "Roberts"}, {account2_balance, 1200.01}, {account2_id, 10000000002}],
  Client3 = [{fname3, "Beth"}, {lname3, "Jones"}, {account3_balance, 0.01}, {account3_id, 10000000008}],
  Client4 = [{fname4, "Tom"}, {lname4, "Fitz"}, {account4_balance, 1000.00}, {account4_id, 20000000000}],
  Client5 = [{fname5, "Sue"}, {lname5, "Johnson"}, {account5_balance, 100.55}, {account5_id, 30000000004}],

  inets:start(),

  ?assertMatch({ok, _Id1}, db_client:put(Client1)),
  ?assertMatch({ok, _Id2}, db_client:put(Client2)),
  ?assertMatch({ok, _Id3}, db_client:put(Client3)),
  ?assertMatch({ok, _Id4}, db_client:put(Client4)),
  ?assertMatch({ok, _Id5}, db_client:put(Client5)),

  inets:stop().