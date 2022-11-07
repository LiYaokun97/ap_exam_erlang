-module(test_erlst).
-include_lib("eunit/include/eunit.hrl").


-export([test_all/0, test_everything/0]).
-export([]). % Remember to export the other functions from Q2.2

% You are allowed to split your testing code in as many files as you
% think is appropriate, just remember that they should all start with
% 'test_'.
% But you MUST have a module (this file) called test_erlst.

test_all() ->
  test_launch(),
  test_open_account(),
  test_account_balance1(),
  test_make_offer1(),
  test_make_offer2(),
  test_make_offer3(),
  ok.

test_everything() ->
  test_all().

test_launch() ->
  {ok, A} = erlst:launch(),
  {ok, B} = erlst:launch(),
  erlst:open_account(A, {10, [{"a", 1}, {"b", 2}]}),
  erlst:open_account(B, {20, []}),
  erlst:open_account(A, {10, [{"a", 10}, {"b", 20}]}),
  io:format("test_lanunch1 ok ~n").

test_open_account() ->
  {ok, A} = erlst:launch(),
  {ok, B} = erlst:launch(),
  erlst:open_account(A, {10, [{"a", 1}, {"b", 2}]}),
  erlst:open_account(B, {20, []}),
  erlst:open_account(A, {10, [{"a", 10}, {"b", 2320}]}),
  erlst:open_account(A, {10, [{"a", 1023}, {"c", 230}]}),
  erlst:open_account(A, {10, [{"a", 140}, {"d", 230}]}),
  erlst:open_account(A, {10, [{"f", 130}, {"b", 2320}]}),
  io:format("test_open_account ok ~n").

test_account_balance1() ->
  {ok, A} = erlst:launch(),
  InputHolding1 = {100, [{"a", 1}, {"b", 2}]},
  InputHolding2 = {1012332, [{"a", 10}, {"b", 2320}]},
  Account1 = erlst:open_account(A, InputHolding1),
  Account2 = erlst:open_account(A, InputHolding2),
  Holding1 = erlst:account_balance(Account1),
  Holding2 = erlst:account_balance(Account2),
  ?assertMatch(Holding1, InputHolding1),
  ?assertMatch(Holding2, InputHolding2),
  io:format("test_account_balance1 ok ~n").


test_make_offer1() ->
  {ok, A} = erlst:launch(),
  InputHolding1 = {100, [{"a", 10}, {"b", 25}]},
  Account1 = erlst:open_account(A, InputHolding1),
  {ok, {OfferId, Server}} = erlst:make_offer(Account1, {"a", 12}),
  {ok, {OfferId2, Server2}} = erlst:make_offer(Account1, {"a", 18}),
  ?assertMatch(OfferId, 1),
  ?assertMatch(Server, A),
  ?assertMatch(OfferId2, 2),
  ?assertMatch(Server2, A),
  io:format("test_make_offer1 ok ~n").

test_make_offer2() ->
  {ok, A} = erlst:launch(),
  {ok, B} = erlst:launch(),
  InputHolding1 = {100, [{"a", 10}, {"b", 10}]},
  InputHolding2 = {200, [{"a", 2}, {"b", 20}]},
  Account1 = erlst:open_account(A, InputHolding1),
  Account2 = erlst:open_account(B, InputHolding2),
  {ok, {OfferId, Server}} = erlst:make_offer(Account1, {"a", 12}),
  {ok, {OfferId2, Server2}} = erlst:make_offer(Account1, {"a", 18}),
  ?assertMatch(OfferId, 1),
  ?assertMatch(Server, A),
  ?assertMatch(OfferId2, 2),
  ?assertMatch(Server2, A),

  {ok, {OfferId3, Server3}} = erlst:make_offer(Account2, {"a", 100}),
  {ok, {OfferId4, Server4}} = erlst:make_offer(Account2, {"a", 18}),
  ?assertMatch(OfferId3, 1),
  ?assertMatch(Server3, B),
  ?assertMatch(OfferId4, 2),
  ?assertMatch(Server4, B),

  io:format("test_make_offer2 ok ~n").


test_make_offer3() ->
  {ok, A} = erlst:launch(),
  {ok, B} = erlst:launch(),
  InputHolding1 = {100, [{"a", 1}, {"b", 10}]},
  InputHolding2 = {200, [{"a", 20}, {"b", 20}]},
  Account1 = erlst:open_account(A, InputHolding1),
  Account2 = erlst:open_account(B, InputHolding2),

  {ok, {OfferId, Server}} = erlst:make_offer(Account1, {"a", 12}),
  {Error, Reason} = erlst:make_offer(Account1, {"a", 18}),
  ?assertMatch(OfferId, 1),
  ?assertMatch(Server, A),
  ?assertMatch(Error, error),
  ?assertMatch(Reason, not_enough_stock_to_offer),

  {ok, {OfferId3, Server3}} = erlst:make_offer(Account2, {"a", 100}),
  {ok, {OfferId4, Server4}} = erlst:make_offer(Account2, {"a", 18}),
  ?assertMatch(OfferId3, 1),
  ?assertMatch(Server3, B),
  ?assertMatch(OfferId4, 2),
  ?assertMatch(Server4, B),

  io:format("test_make_offer3 ok ~n").


