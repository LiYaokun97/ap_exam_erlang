-module(erlst).
-behaviour(gen_server).

% You are allowed to split your Erlang code in as many files as you
% find appropriate.
% However, you MUST have a module (this file) called erlst.

% Export at least the API:
-export([launch/0,
  shutdown/1,
  open_account/2,
  account_balance/1,
  make_offer/2,
  rescind_offer/2,
  add_trader/2,
  remove_trader/2,
  observe_state/1,
  init/1,
  handle_call/3,
  accept_offer/3,
  handle_cast/2]).

% You may have other exports as well
-export([]).

-type stock_exchange() :: term().
-type account_id() :: term().
-type offer_id() :: term().
-type trader_id() :: term().
-type stock() :: atom().
-type isk() :: non_neg_integer().
-type stock_amount() :: pos_integer().
-type holdings() :: {isk(), [{stock(), stock_amount()}]}.
-type offer() :: {stock(), isk()}.
-type decision() :: accept | reject.
-type trader_strategy() :: fun((offer()) -> decision()).


%% account_map : key 为account_id(), value为holdings()
-record(account_data, {current_account_num::integer(), account_map}).
%% offers_map : key 为offer_id(), value为 {account_id(), offer()}
-record(offer_data, {current_offer_num::integer(), offers_map} ).
%% trader_map : key trader_id(), value trader_pid
-record(trader_data, {current_trader_num::integer(), traders_map}).
-record(se_server_data, {accounts, offers, traders}).


-spec launch() -> {ok, stock_exchange()} | {error, term()}.
launch() ->
    gen_server:start_link(?MODULE, [] ,[]).

%% for debug only, return State in gen_server for test use.
observe_state(S) ->
  gen_server:call(S, {observe_state_offers}).

-spec shutdown(S :: stock_exchange()) -> non_neg_integer().
shutdown(_) ->
    not_implemented.

-spec open_account(S :: stock_exchange(), holdings()) -> account_id().
open_account(S, Holdings) ->
    gen_server:call(S, {open_account, Holdings}).

-spec account_balance(Acct :: account_id()) -> holdings().
account_balance(Acct) ->
  {_, ServerPid} = Acct,
  %% todo : if ServerPid doesn't exist, crash; So should add try-catch here.
  gen_server:call(ServerPid, {account_balance, Acct}).

-spec make_offer(Acct :: account_id(), Terms :: offer()) -> {ok, offer_id()} | {error, term()}.
make_offer(Acct, Offer) ->
  {_, ServerPid} = Acct,
  gen_server:call(ServerPid, {make_offer, Acct, Offer}).

-spec rescind_offer(Acct :: account_id(), Offer :: offer_id()) -> ok.
rescind_offer(Acct, OfferId) ->
  {_, ServerPid} = Acct,
  gen_server:cast(ServerPid, {rescind_offer, Acct, OfferId}).

-spec add_trader(Acct :: account_id(), Strategy :: trader_strategy()) -> trader_id().
add_trader(AccountId, Strategy) ->
  {_, Server} = AccountId,
  gen_server:call(Server, {add_trader, Strategy, AccountId}).

-spec remove_trader(Acct :: account_id(), Trader :: trader_id()) -> ok.
remove_trader(_, _) ->
    not_implemented.

accept_offer(S, OfferElem, AccountId) ->
  gen_server:call(S, {accept_offer, OfferElem, AccountId}).

init(_) ->
  State = #se_server_data{
    accounts=#account_data{current_account_num = 0, account_map = maps:new()},
    offers=#offer_data{ current_offer_num=0, offers_map = maps:new()},
    traders = #trader_data{current_trader_num = 0, traders_map = maps:new()}
  },
  {ok,  State}.

check_accepted_offer_valid(State, OfferId, Offer, BuyerAccountId, SellerAccountId) ->
  case check_offer_exist(State, OfferId) of
    false -> offer_do_not_exist;
    true ->
      case check_seller_has_stock(State, SellerAccountId, Offer) of
        false -> seller_do_not_have_stock;
        true ->
          case check_buyer_has_enough_money(State, Offer, BuyerAccountId) of
            false -> buyer_do_not_have_enough_money;
            true -> ok
          end
      end
  end.

check_buyer_has_enough_money(State, Offer, BuyerAccountId) ->
  {_,{ _, Price}} = Offer,
  case get_account_by_id(State, BuyerAccountId) of
    account_do_not_exist -> false;
    {CurMoney, _} ->
      if
        CurMoney >= Price -> true;
        true -> false
      end
  end.

check_offer_exist(State, OfferId) ->
  case get_offer_by_id(State, OfferId) of
    offer_do_not_exist -> false;
    _ -> true
  end .

check_seller_has_stock(State, SellerAccountId, Offer) ->
  {StockName, _} = Offer,
  case get_account_by_id(State, SellerAccountId) of
    account_do_not_exist -> false;
    {_, StockList} ->
      case lists:keyfind(StockName, 1, StockList) of
        false -> false;
        {_, Amount} ->
          if
            Amount > 0 -> true;
            true -> false
          end
      end
  end.

execute_trade(State, SellerAccountId, BuyerAccountId, Offer, OfferId) ->
  NewState = delete_offer_by_id(State, OfferId),
  {StockName, Price} = Offer,
  NewState2 = update_seller_holding(NewState, SellerAccountId, StockName, Price),
  update_buyer_holding(NewState2, BuyerAccountId, StockName, Price).

%% for open_account request
handle_call({observe_state_offers}, _From, State) ->
  OfferList = maps:to_list((State#se_server_data.offers)#offer_data.offers_map),
  {reply, OfferList, State};

handle_call({accept_offer, OfferElem, AccountId}, _From, State) ->
  {OfferId, {SellerAccountId, Offer}} = OfferElem,
  {BuyerAccountId, _} = AccountId,
  Result = case check_accepted_offer_valid(State, OfferId, Offer, BuyerAccountId, SellerAccountId) of
    ok ->
      execute_trade(State, SellerAccountId, BuyerAccountId, Offer, OfferId),
      delete_trader_offer;
    offer_do_not_exist -> delete_trader_offer;
    _ ->  keep_trader_offer
  end,
  {reply, Result, State};

%% for open_account request
handle_call({open_account , Holdings}, _From, State) ->
  {AccountId, NewState} = add_account(State, self(), Holdings),
  {reply, AccountId, NewState};

%% for account_balance request
handle_call({account_balance , AccountId}, _From, State) ->
%%  io:format("accountId in handle_call : ~p ~n", [AccountId]),
  Holdings = get_account_by_id(State, AccountId),
  %% todo: what if no such account?
   {reply, Holdings, State};

%% for make_offer request
handle_call({make_offer , Acct, Offer}, _From, State) ->
  Holdings = get_account_by_id(State , Acct),
  case check_valid_offer(Offer, Holdings) of
    true ->
      {OfferId, NewState} = add_offer(self(), State, Acct, Offer),
      {reply, {ok, OfferId}, NewState};
    false ->
      {reply, {error, not_enough_stock_to_offer}, State}
  end;

handle_call({add_trader, Strategy, AccountId}, _From, State) ->
  TraderPid = trader:start_trader(Strategy, self(), get_offers_map(State) , AccountId),
  TraderId = get_current_trader_num(State) + 1,
  NewState = set_current_trader_num(State, TraderId),
  TraderValue = {TraderPid, Strategy, AccountId},
  TradeMap = get_traders_map(NewState),
  NewTradeMap = maps:update(TraderId, TraderValue, TradeMap),
  NewState2 = set_traders_map(NewState, NewTradeMap),
  {reply, {TraderId}, NewState2}.

%% for rescind_offer request
handle_cast({rescind_offer, _Acct, OfferId}, State) ->
  NewState = delete_offer(State, OfferId),
  {noreply,NewState}.

%% account utils functions
add_account(State, Server, Holdings) ->
  Id = get_current_account_num(State) + 1,
  NewAccountMap = maps:put({Id, Server}, Holdings , get_accounts_map(State)),
  NewState1 = set_accounts_map(State, NewAccountMap),
  NewState2 = set_current_account_num(NewState1, Id),
  {{Id, Server}, NewState2}.

%% offer utils function
add_offer(Server, State, Acct, Offer) ->
  Offers = State#se_server_data.offers,
  Id = Offers#offer_data.current_offer_num + 1,
  NewOfferMap = maps:put({Id, Server}, {Acct, Offer} , Offers#offer_data.offers_map),
  NewOffers = Offers#offer_data{current_offer_num = Id, offers_map = NewOfferMap},
  NewState = State#se_server_data{offers = NewOffers},
  {{Id, Server}, NewState}.

delete_offer(State, OfferId) ->
  OffersMap = (State#se_server_data.offers)#offer_data.offers_map,
  NewOffersMap = maps:remove(OfferId, OffersMap),
  State#se_server_data{offers =(State#se_server_data.offers)#offer_data{offers_map = NewOffersMap}}.

check_own_stock(StockName, Holding) ->
  { _, StockList} = Holding,
  case lists:keyfind(StockName , 1, StockList) of
    false -> false;
    {_, Amount} ->
      if
        Amount > 0 -> true;
        true -> false
      end
  end.

check_valid_offer(Offer, Holdings) ->
  {StockName1, _} = Offer,
  check_own_stock(StockName1, Holdings).

%% api for get data quickly from State
get_accounts_data(State) ->
  State#se_server_data.accounts.

get_offers_data(State) ->
  State#se_server_data.offers.

get_traders_data(State)->
  State#se_server_data.traders.

get_accounts_map(State) ->
  (get_accounts_data(State))#account_data.account_map.

set_accounts_map(State, NewAccountMap) ->
  NewAccountData = (get_accounts_data(State))#account_data{account_map = NewAccountMap},
  State#se_server_data{accounts = NewAccountData}.

get_offers_map(State) ->
  (get_offers_data(State))#offer_data.offers_map.

%%set_offers_map(State, NewOffersMap) ->
%%  NewOffersData = (get_offers_data(State))#offer_data{offers_map = NewOffersMap},
%%  State#se_server_data{offers = NewOffersData}.

get_traders_map(State) ->
  (get_traders_data(State))#trader_data.traders_map.

set_traders_map(State, NewTradersMap) ->
  NewTradersData = (get_traders_data(State))#trader_data{ traders_map = NewTradersMap},
  State#se_server_data{traders = NewTradersData}.

get_current_trader_num(State) ->
  (get_traders_data(State))#trader_data.current_trader_num.

set_current_trader_num(State, Num) ->
  NewTradersData = (get_traders_data(State))#trader_data{ current_trader_num = Num},
  State#se_server_data{traders = NewTradersData}.

get_current_account_num(State) ->
  (get_accounts_data(State))#account_data.current_account_num.

set_current_account_num(State, Num) ->
  NewAccountsData = (get_accounts_data(State))#account_data{ current_account_num = Num},
  State#se_server_data{accounts = NewAccountsData}.

%%get_current_offer_num(State) ->
%%  (get_offers_data(State))#offer_data.current_offer_num.

get_offer_by_id(State, OfferId) ->
  OfferMap = get_offers_map(State),
  maps:get(OfferId, OfferMap, offer_do_not_exist).

get_account_by_id(State, AccountId) ->
  AccountMap = get_accounts_map(State),
  maps:get(AccountId, AccountMap, account_do_not_exist).

delete_offer_by_id(State, OfferId) ->
  OfferMap = get_offers_map(State),
  NewOffersMap = maps:remove(OfferId, OfferMap),
  NewOffersData = (get_offers_data(State))#offer_data{offers_map = NewOffersMap},
  State#se_server_data{offers = NewOffersData}.

set_account_by_id(State, SellerAccountId, AccountValue) ->
  AccountMap = get_accounts_map(State),
  NewAccountMap = maps:update(SellerAccountId ,AccountValue, AccountMap),
  set_accounts_map(State, NewAccountMap).

update_seller_holding(State, SellerAccountId, StockName, Price) ->
  Holding = get_account_by_id(State, SellerAccountId),
  {CurMoney, StockList} = Holding,
  {_, Amount} = lists:keyfind(StockName, 1, StockList),
  NewStockList = lists:delete( {StockName, Amount} ,StockList) ++ [{StockName, Amount - 1}],
  NewHolding = {CurMoney + Price, NewStockList},
  set_account_by_id(State, SellerAccountId, NewHolding).

update_buyer_holding(State, BuyerAccountId, StockName, Price) ->
  Holding = get_account_by_id(State, BuyerAccountId),
  {CurMoney, StockList} = Holding,
  {_, Amount} = lists:keyfind(StockName, 1, StockList),
  NewStockList = lists:delete({StockName, Amount} ,StockList) ++ [{StockName, Amount + 1}],
  NewHolding = {CurMoney - Price, NewStockList},
  set_account_by_id(State, BuyerAccountId, NewHolding).
