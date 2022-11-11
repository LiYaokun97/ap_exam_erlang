-module(test_erlst).
%%-include_lib("eunit/include/eunit.hrl").
-include_lib("eqc/include/eqc.hrl").

-export([test_all/0, test_everything/0,generate_stock_name/0 ]).
-export([prop_value_preservation/0,generate_stock_exchange/0,
  test_strategy/0,generate_strategy/0, prop_total_trades/0,
  prop_value_preservation_collect_traderlist/0,
  prop_value_preservation_collect_offerlist/0]). % Remember to export the other functions from Q2.2


% You are allowed to split your testing code in as many files as you
% think is appropriate, just remember that they should all start with
% 'test_'.
% But you MUST have a module (this file) called test_erlst.

test_all() ->
%%   all this functions starting with 'test_' are for testing basic function in Q2.1
  eqc:quickcheck(prop_value_preservation()),
  eqc:quickcheck(prop_total_trades()),
  test_eunit_erlst:test_all(),
  ok.

test_everything() ->
  test_all().

%% TraderList [{InitialHolding, AccountId, TraderId}]
%% offerList [{Holding, AccountId, {{StockName, Price} ,OfferId}}]
prop_value_preservation() ->
  ?FORALL(
    {S, TraderList, OfferList, RemoveTrader, RemoveOffer},
    generate_offer_and_trader2(),
    begin
      TraderHolingMap = calculate_initial_holding_in_traderList(TraderList, maps:new()),
      AllHoldingMap = calculate_initial_holding_in_offerList(OfferList, TraderHolingMap),
      %% we have time-consuming strategy function, so we need to wait at least 1000.
      remove_trader_from_server(RemoveTrader, TraderList),
%%      remove_offer_from_server(RemoveOffer, OfferList),
      timer:sleep(100),
      remove_all_trders(TraderList),
      timer:sleep(100),
      AccountIdList = collect_all_accountId(TraderList, OfferList),
      HoldingList = get_holding_list(AccountIdList),
      NewAllHoldingMap = calculate_initial_holdinglist(HoldingList, maps:new()),
      io:format("[test process] NewAllHoldingMap ~p~n",[NewAllHoldingMap]),
      io:format("[test process] AllHoldingMap ~p~n",[AllHoldingMap]),
      erlst:shutdown(S),
      io:format("----------------------------------------------------------~n"),
      NewAllHoldingMap == AllHoldingMap
    end
  ).

remove_trader_from_server(B, TraderList) ->
  if B ->
    case TraderList of
      [X|_XS] ->
        {_, AccountId, TraderId} = X,
        io:format("[test process] remove trader ~n"),
        erlst:remove_trader(AccountId, TraderId);
      [] -> nothing
    end;
    true -> nothing
  end.


remove_offer_from_server(B, OfferList) ->
  if B ->
    case OfferList of
    [X|_XS] ->
      {_, AccountId, {_, OfferId}} = X,
      io:format("[test process] remove offer ~n"),
      erlst:rescind_offer(AccountId, OfferId);
    [] -> nothing
    end;
    true -> nothing
  end.

prop_value_preservation_collect_traderlist() ->
  ?FORALL(
    {_S, TraderList, _OfferList,_,_},
    generate_offer_and_trader2(),
    collect(TraderList, true)
  ).

prop_value_preservation_collect_offerlist() ->
  ?FORALL(
    {_S, _TraderList, OfferList,_,_},
    generate_offer_and_trader2(),
    collect(OfferList, true)
  ).

prop_total_trades() ->
  ?FORALL(
    {S, TraderList, OfferList, RemoveTrader, RemoveOffer},
    generate_offer_and_trader2(),
    begin
      MakeOfferNum = length(OfferList),
      remove_trader_from_server(RemoveTrader, TraderList),
%%      remove_offer_from_server(RemoveOffer, OfferList),
      timer:sleep(100),
      FinalTradeNum = erlst:shutdown(S),
      MakeOfferNum >= FinalTradeNum
    end
  ).

%% the name of the stock can only be [a,b,c,d,e]
generate_stock_name() ->
  eqc_gen:elements([a,b,c,d,e]).

generate_pos_int() ->
  ?LET(N, eqc_gen:nat(), case N of
                           0 -> 1;
                           X -> X
                         end ).

%% [{StockName, Amount}]
generate_stock_list() ->
  eqc_gen:list({generate_stock_name(), generate_pos_int()}).

transfer_holding(StockList, HoldingMap) ->
  case StockList of
    [X|XS] ->
      {StockName, Amount} = X,
      CurrentAmount = maps:get(StockName, HoldingMap, 0),
      NewMap = maps:put(StockName, Amount + CurrentAmount, HoldingMap),
      transfer_holding(XS, NewMap);
    [] -> HoldingMap
  end.

%% holding:{CurMoney, [{StockName, Amount}]}
generate_holdings() ->
  ?LET({Money ,StockList}, {generate_pos_int() , generate_stock_list()},
    {Money, maps:to_list(transfer_holding(StockList, maps:new()))}).

generate_stock_exchange() ->
  ?LET({_, S}, ?LAZY(erlst:launch()), S).

%% return: {Holding, Account}
generate_account(S) ->
  ?LET(Holding, generate_holdings(),  {Holding, erlst:open_account(S, Holding)}).

%% should limit the frequency of negative number.
%% most cases should be positive.
generate_stock_price() ->
  eqc_gen:frequency([{1, eqc_gen:int()}, {9, eqc_gen:nat()}]).

%%return {{StockName, Price} ,OfferId}
generate_offer(Acct) ->
  ?LET({StockName, Price}, {generate_stock_name(), generate_stock_price()},
    case erlst:make_offer(Acct, {StockName, Price}) of
      {ok, OfferId} -> {{StockName, Price} ,OfferId};
      {error, Reason} -> Reason
    end ).

%% return {Holding, AccountId, {{StockName, Price} ,OfferId}}
generate_offer_with_server(S) ->
  ?LET({Holding, AccountId} , generate_account(S), {Holding, AccountId, generate_offer(AccountId)}).

%% return [{Holding, AccountId, {{StockName, Price} ,OfferId}}]
generate_offer_list_with_server(S) ->
  eqc_gen:list(generate_offer_with_server(S)).


reliable_strategy() ->
  eqc_gen:elements([
    {call,test_erlst,mkstrategy,[buy_everything]},
    {call,test_erlst,mkstrategy,[{buy_price_less_than, 20}]},
    {call,test_erlst,mkstrategy,[{buy_only, a}]},
    {call,test_erlst,mkstrategy,[{buy_only, b}]},
    {call,test_erlst,mkstrategy,[{buy_only, c}]},
    {call,test_erlst,mkstrategy,[{buy_only, d}]},
    {call,test_erlst,mkstrategy,[{buy_only, e}]},
    {call,test_erlst,mkstrategy,[{both, {buy_price_less_than, 8}, {buy_only, a}}]},
    {call,test_erlst,mkstrategy,[long_time_strategy_accept_everything]},
    {call,test_erlst,mkstrategy,[long_time_strategy_reject_everything]}
  ]).

test_strategy() ->
  ?LAZY(eqc_symbolic:eval(reliable_strategy())).

generate_strategy() ->
  ?LET(Func, eqc_symbolic:eval(test_strategy()), Func).

mkstrategy_helper() ->
  ?LET(Symb, reliable_strategy(), case Symb of
                                    {_,_,_, [Lists]} -> mkstrategy(Lists);
                                    _ -> fun(_Offer) -> accept end
                                  end ).

mkstrategy(Opr) ->
  io:format("[test process] Opr ~p~n",[Opr]),
  case Opr of
    buy_everything ->
      Func = fun(_Offer) -> accept end,
      io:format("[test process] Func ~p~n",[Func]),
      Func;
    {buy_price_less_than, Price} ->
      fun({_Stock, Price2}) ->
        if
          Price2 < Price -> accept;
          true-> reject
        end
      end;
    {buy_only, StockName} ->
      fun({Stock, _Price2}) ->
        case Stock of
          StockName -> accept;
          _ -> reject
        end
      end;
    {both, Para1, Para2} ->
      Func1 = mkstrategy(Para1),
      Func2 = mkstrategy(Para2),
      fun(Offer) ->
        case Func1(Offer) of
          accept -> case Func2(Offer) of
                      accept -> accept;
                      reject -> reject
                    end;
          reject -> reject
        end
      end;
    long_time_strategy_accept_everything ->
      fun(_Offer) ->
        timer:sleep(10),
        accept
      end;
    long_time_strategy_reject_everything ->
      fun(_Offer) ->
        timer:sleep(10),
        reject
      end
  end.

generate_trader(Acct) ->
  ?LET(Strategy, mkstrategy_helper(),
    ?LAZY(erlst:add_trader(Acct, Strategy))).

%% return {InitialHolding,AccountId, TraderId}
generate_trader_with_server(S) ->
  ?LET({Holding, AccountId}, generate_account(S), {Holding, AccountId, generate_trader(AccountId)}).

%% return [{InitialHolding,AccountId, TraderId}]
generate_trader_list_with_server(S) ->
  eqc_gen:list(generate_trader_with_server(S)).


generate_offer_and_trader2() ->
  ?LET(S, generate_stock_exchange(), {S, generate_trader_list_with_server(S), generate_offer_list_with_server(S), eqc_gen:bool(),  eqc_gen:bool()}).

remove_all_trders(TraderList) ->
  lists:foldl(
    fun(X, _Acc) ->
      {_, AccountId, TraderId} = X,
      erlst:remove_trader(AccountId, TraderId),
      []
    end,
    [],
    TraderList).

get_holding_list(AccountIdList) ->
  lists:foldl(
    fun(X, Acc) ->
      Holdings = erlst:account_balance(X),
      Acc ++ [Holdings]
    end,
    [],
    AccountIdList).


collect_all_accountId(TraderList, OfferList) ->
  AccountList1 = lists:foldl(
    fun(X, Acc) ->
      {_, AccountId, _} = X,
      Acc ++ [AccountId]
    end,
    [],
    TraderList),
  AccountList2 = lists:foldl(
    fun(X, Acc) ->
      {_, AccountId, _} = X,
      Acc ++ [AccountId]
    end,
    [],
    OfferList),
  AccountList1 ++ AccountList2.

calculate_initial_holding_in_offerList(OfferList, HoldingMap) ->
  case OfferList of
    [X|XS] ->
      {InitialHolding, _, _} = X,
      NewMap = calculate_initial_holding(InitialHolding, HoldingMap),
      calculate_initial_holding_in_offerList(XS, NewMap);
    [] -> HoldingMap
  end.

%% TraderList : [{InitialHolding, AccountId, TraderId}]
%% OfferList : [{InitialHolding, AccountId, {{StockName, Price} ,OfferId}}]
calculate_initial_holding_in_traderList(TraderList, HoldingMap) ->
  case TraderList of
    [X|XS] ->
      {InitialHolding, _, _} = X,
      io:format("[test process] InitialHolding ~p~n",[InitialHolding]),
      NewMap = calculate_initial_holding(InitialHolding, HoldingMap),
      calculate_initial_holding_in_traderList(XS, NewMap);
    [] -> HoldingMap
  end.

calculate_initial_holding(Holding, HoldingMap) ->
    {CurMoney, StockList} = Holding,
    NewMap = calculate_initial_stock_list(StockList, HoldingMap),
    CurAllMoney = maps:get("Money", NewMap, 0),
    maps:put("Money", CurAllMoney + CurMoney, NewMap).


calculate_initial_holdinglist(HoldingList, HoldingMap) ->
  case HoldingList of
    [X|XS] ->
      {CurMoney, StockList} = X,
      NewMap = calculate_initial_stock_list(StockList, HoldingMap),
      CurAllMoney = maps:get("Money", NewMap, 0),
      NewMap2 = maps:put("Money", CurAllMoney + CurMoney, NewMap),
      calculate_initial_holdinglist(XS, NewMap2);
    [] ->
      HoldingMap
  end.

calculate_initial_stock_list(StockList, HoldingMap) ->
  case StockList of
    [X|XS] ->
      {StockName, Amount} = X,
      OldAmount = maps:get(StockName, HoldingMap, 0),
      NewMap = maps:put(StockName, OldAmount + Amount, HoldingMap),
      calculate_initial_stock_list(XS, NewMap);
    []-> HoldingMap
  end.



