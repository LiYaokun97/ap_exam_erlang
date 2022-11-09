-module(test_erlst).
-include_lib("eunit/include/eunit.hrl").


-export([test_all/0, test_everything/0,test_add_trader1/0, test_all_add_trader/0, test_add_trader2/0, test_add_trader3/0,
  test_add_trader4/0, test_remove_trader/0, test_shutdown/0, test_exception_strategy/0, test_rescind_offer/0
  ,test_rescind_offer2/0,test_rescind_offer3/0, test_shutdown2/0 ]).
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
  test_rescind_offer(),
  test_rescind_offer2(),
  test_all_add_trader(),
  test_shutdown(),
  test_shutdown2(),
  test_exception_strategy(),
  ok.

test_everything() ->
  test_all().

test_launch() ->
  {ok, A} = erlst:launch(),
  {ok, B} = erlst:launch(),
  erlst:open_account(A, {10, [{a, 1}, {b, 2}]}),
  erlst:open_account(B, {20, []}),
  erlst:open_account(A, {10, [{a, 10}, {b, 20}]}),
  io:format("test_lanunch1 ok ~n").

test_open_account() ->
  {ok, A} = erlst:launch(),
  {ok, B} = erlst:launch(),
  erlst:open_account(A, {10, [{a, 1}, {b, 2}]}),
  erlst:open_account(B, {20, []}),
  erlst:open_account(A, {10, [{a, 10}, {b, 2320}]}),
  erlst:open_account(A, {10, [{a, 1023}, {"c", 230}]}),
  erlst:open_account(A, {10, [{a, 140}, {"d", 230}]}),
  erlst:open_account(A, {10, [{"f", 130}, {b, 2320}]}),
  io:format("test_open_account ok ~n").

test_account_balance1() ->
  {ok, A} = erlst:launch(),
  InputHolding1 = {100, [{a, 1}, {b, 2}]},
  InputHolding2 = {1012332, [{a, 10}, {b, 2320}]},
  Account1 = erlst:open_account(A, InputHolding1),
  Account2 = erlst:open_account(A, InputHolding2),
  Holding1 = erlst:account_balance(Account1),
  Holding2 = erlst:account_balance(Account2),
  ?assertMatch(Holding1, InputHolding1),
  ?assertMatch(Holding2, InputHolding2),
  io:format("test_account_balance1 ok ~n").


test_make_offer1() ->
  {ok, A} = erlst:launch(),
  InputHolding1 = {100, [{a, 10}, {b, 25}]},
  Account1 = erlst:open_account(A, InputHolding1),
  {ok, {OfferId, Server}} = erlst:make_offer(Account1, {a, 12}),
  {ok, {OfferId2, Server2}} = erlst:make_offer(Account1, {a, 18}),
  ?assertMatch(OfferId, 1),
  ?assertMatch(Server, A),
  ?assertMatch(OfferId2, 2),
  ?assertMatch(Server2, A),
  io:format("test_make_offer1 ok ~n").

test_make_offer2() ->
  {ok, A} = erlst:launch(),
  {ok, B} = erlst:launch(),
  InputHolding1 = {100, [{a, 10}, {b, 10}]},
  InputHolding2 = {200, [{a, 2}, {b, 20}]},
  Account1 = erlst:open_account(A, InputHolding1),
  Account2 = erlst:open_account(B, InputHolding2),
  {ok, {OfferId, Server}} = erlst:make_offer(Account1, {a, 12}),
  {ok, {OfferId2, Server2}} = erlst:make_offer(Account1, {a, 18}),
  ?assertMatch(OfferId, 1),
  ?assertMatch(Server, A),
  ?assertMatch(OfferId2, 2),
  ?assertMatch(Server2, A),

  {ok, {OfferId3, Server3}} = erlst:make_offer(Account2, {a, 100}),
  {ok, {OfferId4, Server4}} = erlst:make_offer(Account2, {a, 18}),
  ?assertMatch(OfferId3, 1),
  ?assertMatch(Server3, B),
  ?assertMatch(OfferId4, 2),
  ?assertMatch(Server4, B),

  io:format("test_make_offer2 ok ~n").


test_make_offer3() ->
  {ok, A} = erlst:launch(),
  {ok, B} = erlst:launch(),
  InputHolding1 = {100, [{a, 1}, {b, 10}]},
  InputHolding2 = {200, [{a, 20}, {b, 20}]},
  Account1 = erlst:open_account(A, InputHolding1),
  Account2 = erlst:open_account(B, InputHolding2),

  {ok, {OfferId, _}} = erlst:make_offer(Account1, {a, 12}),
  Result = erlst:make_offer(Account1, {a, 18}),
  io:format("Result : ~p ~n", [Result]),
  ?assertMatch(OfferId, 1),

  {ok, {OfferId3, Server3}} = erlst:make_offer(Account2, {a, 100}),
  {ok, {OfferId4, Server4}} = erlst:make_offer(Account2, {a, 18}),
  ?assertMatch(OfferId3, 1),
  ?assertMatch(Server3, B),
  ?assertMatch(OfferId4, 2),
  ?assertMatch(Server4, B),

  io:format("test_make_offer3 ok ~n").

test_rescind_offer() ->
  {ok, A} = erlst:launch(),
  InputHolding1 = {100, [{a, 10}, {b, 10}]},
  Account1 = erlst:open_account(A, InputHolding1),
  {ok, {OfferId, Server}} = erlst:make_offer(Account1, {a, 12}),
  {ok, {OfferId2, Server2}} = erlst:make_offer(Account1, {a, 18}),
  ?assertMatch(OfferId, 1),
  ?assertMatch(Server, A),
  ?assertMatch(OfferId2, 2),
  ?assertMatch(Server2, A),
  erlst:rescind_offer(Account1, {OfferId, Server}),
  io:format("test_rescind_offer ok ~n").

test_rescind_offer2() ->
  {ok, A} = erlst:launch(),
  {ok, B} = erlst:launch(),
  InputHolding1 = {100, [{a, 10}, {b, 10}]},
  InputHolding2 = {200, [{a, 2}, {b, 20}]},
  Account1 = erlst:open_account(A, InputHolding1),
  Account2 = erlst:open_account(B, InputHolding2),
  {ok, {OfferId, Server}} = erlst:make_offer(Account1, {a, 12}),
  {ok, {OfferId2, Server2}} = erlst:make_offer(Account1, {a, 18}),
  ?assertMatch(OfferId, 1),
  ?assertMatch(Server, A),
  ?assertMatch(OfferId2, 2),
  ?assertMatch(Server2, A),
  erlst:rescind_offer(Account1, {OfferId, Server}),

  OfferMap = maps:new(),
  OfferMap1 = maps:put({2, A}, {{1,A},{a,18}}, OfferMap),
  RightState = maps:to_list(OfferMap1),
  State = erlst:observe_state(A),
  ?assertMatch(State, RightState),

  {ok, {OfferId3, Server3}} = erlst:make_offer(Account2, {a, 100}),
  {ok, {OfferId4, Server4}} = erlst:make_offer(Account2, {a, 18}),
  ?assertMatch(OfferId3, 1),
  ?assertMatch(Server3, B),
  ?assertMatch(OfferId4, 2),
  ?assertMatch(Server4, B),
  io:format("test_rescind_offer2 ok ~n").

test_rescind_offer3() ->
  {ok, A} = erlst:launch(),
  InputHolding1 = {100, [{a, 10}]},
  Account1 = erlst:open_account(A, InputHolding1),
  {ok, OfferId1} = erlst:make_offer(Account1, {a, 1}),
  erlst:make_offer(Account1, {a, 1}),
  erlst:add_trader(Account1, fun({_StockName, Price}) -> timer:sleep(1000), if Price < 2 -> accept; true -> reject end end),
  erlst:rescind_offer(Account1, OfferId1),
  io:format("[test process] test_rescind_offer3 ok ~n").


test_all_add_trader() ->
  test_add_trader1(),
  test_add_trader2(),
  test_add_trader4(),
  test_add_trader3().

%% seller and buyer are the same account
test_add_trader1() ->
  {ok, A} = erlst:launch(),
  InputHolding1 = {100, [{a, 10}, {b, 25}]},
  Account1 = erlst:open_account(A, InputHolding1),
  {ok, {OfferId, Server}} = erlst:make_offer(Account1, {a, 12}),
  {ok, {OfferId2, Server2}} = erlst:make_offer(Account1, {a, 18}),
  ?assertMatch(OfferId, 1),
  ?assertMatch(Server, A),
  ?assertMatch(OfferId2, 2),
  ?assertMatch(Server2, A),
  erlst:add_trader(Account1, fun(_Offer) -> accept end),
  io:format("[test process] test_add_trader1 ok ~n").

%% In my design, the buyer and seller can be same account, so the behaviour of this
%% test is not deterministic, but we can make sure the final sum of money account1 and
%% account2 have is 100, and the sum of amount of stock a they have is 110.
test_add_trader2() ->
  {ok, A} = erlst:launch(),
  InputHolding1 = {100, [{a, 10}]},
  InputHolding2 = {0, [{a, 100}]},
  Account1 = erlst:open_account(A, InputHolding1),
  Account2 = erlst:open_account(A, InputHolding2),
  erlst:make_offer(Account1, {a, 3}),
  erlst:make_offer(Account1, {a, 2}),
  erlst:make_offer(Account2, {a, 1}),
  erlst:make_offer(Account2, {a ,1}),
  erlst:make_offer(Account2, {a ,1}),
  erlst:make_offer(Account2, {a ,1}),
  erlst:make_offer(Account2, {a ,1}),
  erlst:add_trader(Account1, fun({_StockName, Price}) -> if Price < 2 -> accept; true -> reject end end),
  erlst:add_trader(Account2, fun(_Offer) -> accept end),
  io:format("[test process] test_add_trader2 ok ~n").

strategy1({StockName, _Price}) ->
  if
    (StockName==b) -> accept;
    true -> reject
  end.

test_add_trader3() ->
  {ok, A} = erlst:launch(),
  InputHolding1 = {100, [{a, 10}]},
  InputHolding2 = {0, [{b, 100}]},
  Account1 = erlst:open_account(A, InputHolding1),
  Account2 = erlst:open_account(A, InputHolding2),
  erlst:make_offer(Account1, {a, 2}),
  erlst:add_trader(Account1, fun({StockName, Price}) -> strategy1({StockName, Price}) end),
  erlst:add_trader(Account2, fun(_Offer) -> accept end),
  erlst:make_offer(Account2, {b ,10}),
  io:format("[test process] test_add_trader3 ok ~n").

test_add_trader4() ->
  {ok, A} = erlst:launch(),
  InputHolding1 = {12, [{a, 1},{b, 1}, {c, 2}]},
  InputHolding2 = {0, [{b, 1}]},
  Account1 = erlst:open_account(A, InputHolding1),
  Account2 = erlst:open_account(A, InputHolding2),
  erlst:make_offer(Account1, {a, 2}),
  erlst:add_trader(Account1, fun({StockName, Price}) -> strategy1({StockName, Price}) end),
  erlst:add_trader(Account2, fun(_Offer) -> accept end),
  erlst:make_offer(Account2, {b ,10}),
  erlst:make_offer(Account1, {c, 6}),
  io:format("[test process] test_add_trader3 ok ~n").

strategy_sleep_5000ms({_StockName, _Price}) ->
  timer:sleep(5000),
  accept.

test_remove_trader() ->
  {ok, A} = erlst:launch(),
  InputHolding1 = {12, [{a, 1},{b, 1}, {c, 2}]},
  Account1 = erlst:open_account(A, InputHolding1),
  erlst:make_offer(Account1, {a, 2}),
  TraderId = erlst:add_trader(Account1, fun({StockName, Price}) -> strategy_sleep_5000ms({StockName, Price}) end),
  erlst:make_offer(Account1, {c, 6}),
  erlst:remove_trader(Account1, TraderId),
  io:format("[test process] test_remove_trader ok ~n").

test_shutdown() ->
  {ok, A} = erlst:launch(),
  InputHolding1 = {12, [{a, 1},{b, 1}, {c, 2}]},
  Account1 = erlst:open_account(A, InputHolding1),
  erlst:make_offer(Account1, {a, 2}),
  erlst:add_trader(Account1, fun({StockName, Price}) -> strategy_sleep_5000ms({StockName, Price}) end),
  erlst:make_offer(Account1, {c, 6}),
  ExecutedNum = erlst:shutdown(A),
  ?assertMatch(ExecutedNum , 0),
  io:format("[test process] test_shutdown ok ~n").


test_shutdown2() ->
  {ok, A} = erlst:launch(),
  InputHolding1 = {12, [{a, 1},{b, 1}, {c, 2}]},
  Account1 = erlst:open_account(A, InputHolding1),
  erlst:make_offer(Account1, {a, 2}),
  erlst:add_trader(Account1, fun({StockName, Price}) -> strategy_sleep_5000ms({StockName, Price}) end),
  erlst:make_offer(Account1, {c, 6}),
  timer:sleep(12000),
  ExecutedNum = erlst:shutdown(A),
  ?assertMatch(ExecutedNum , 2),
  io:format("[test process] test_shutdown2 ok ~n").


strategy_exception({_StockName, _Price}) ->
  io:format("[test process] strategy_exception ~n"),
  X = 100/0,
  X.

test_exception_strategy() ->
  {ok, A} = erlst:launch(),
  InputHolding1 = {12, [{a, 1},{b, 1}, {c, 2}]},
  Account1 = erlst:open_account(A, InputHolding1),
  erlst:make_offer(Account1, {a, 2}),
  erlst:add_trader(Account1, fun({StockName, Price}) -> strategy_exception({StockName, Price}) end),
  erlst:make_offer(Account1, {c, 6}),
  io:format("[test process] test_exception_strategy ok ~n").

