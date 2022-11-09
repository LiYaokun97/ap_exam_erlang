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
  accept_offer/4,
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


%% account_map : key account_id(), value holdings()
-record(account_data, {current_account_num::integer(), account_map}).
%% offers_map : key offer_id(), value {account_id(), offer()}
-record(offer_data, {current_offer_num::integer(), offers_map} ).
%% trader_map : key trader_id(), value {trader_pid, Strategy, AccountId }
-record(trader_data, {current_trader_num::integer(), traders_map}).
%% supervisor {SupId, }
-record(se_server_data, {accounts, offers, traders, supervisor, executed_trades_num}).


-spec launch() -> {ok, stock_exchange()} | {error, term()}.
launch() ->
    gen_server:start_link(?MODULE, [] ,[]).

%% for debug only, return State in gen_server for test use.
observe_state(S) ->
  gen_server:call(S, {observe_state_offers}).

-spec shutdown(S :: stock_exchange()) -> non_neg_integer().
shutdown(S) ->
  ExecutedNumber = gen_server:call(S, {shutdown}),
  gen_server:stop(S),
  ExecutedNumber.

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
remove_trader(AccountId, TraderId) ->
  {_, Server} = AccountId,
  gen_server:cast(Server, {remove_trader, TraderId}).

accept_offer(S, OfferElem, AccountId, TraderId) ->
  gen_server:call(S, {accept_offer, OfferElem, AccountId, TraderId}).

init(_) ->
  State = #se_server_data{
    accounts=#account_data{current_account_num = 0, account_map = maps:new()},
    offers=#offer_data{ current_offer_num=0, offers_map = maps:new()},
    traders = #trader_data{current_trader_num = 0, traders_map = maps:new()},
    supervisor = sup:start_link(),
    executed_trades_num = 0
  },
  {ok,  State}.

check_accepted_offer_valid(State, OfferId, Offer, BuyerAccountId, SellerAccountId, TraderId) ->
  case check_offer_exist(State, OfferId) of
    false -> offer_do_not_exist;
    true ->
      case check_seller_has_stock(State, SellerAccountId, Offer) of
        false -> seller_do_not_have_stock;
        true ->
          case check_buyer_has_enough_money(State, Offer, BuyerAccountId) of
            false -> buyer_do_not_have_enough_money;
            true ->
              case check_valid_trader(State, TraderId) of
                false -> offer_do_not_exist;
                true -> ok
              end
          end
      end
  end.

check_buyer_has_enough_money(State, Offer, BuyerAccountId) ->
  { _, Price} = Offer,
  case get_account_by_id(State, BuyerAccountId) of
    account_do_not_exist ->
      io:format("[Server process] State :~n~p~nBuyerAccountId:~p~n", [State, BuyerAccountId]),
      io:format("[server process] check_buyer_has_enough_money account_do_not_exist ~n"),
      false;
    {CurMoney, _} ->
      if
        CurMoney >= Price -> true;
        true -> false
      end
  end.

check_valid_trader(State, TraderId) ->
  case get_trader_by_id(State, TraderId) of
    trader_do_not_exist ->
      io:format("[server process] trader do not exist ~n"),
      false;
    _ ->
      io:format("[server process] trader exist ~n"),
      true
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
  NewState3 = update_executed_trades_num(NewState2),
  update_buyer_holding(NewState3, BuyerAccountId, StockName, Price).

%% for open_account request
handle_call({observe_state_offers}, _From, State) ->
  OfferList = maps:to_list((State#se_server_data.offers)#offer_data.offers_map),
  {reply, OfferList, State};

handle_call({accept_offer, OfferElem, AccountId, TraderId}, _From, State) ->
  io:format("[server process] server get accept_offer request ~n"),
  {OfferId, {SellerAccountId, Offer}} = OfferElem,
  BuyerAccountId = AccountId,
  case check_accepted_offer_valid(State, OfferId, Offer, BuyerAccountId, SellerAccountId, TraderId) of
    ok ->
      io:format("[server process] before make deal: ~n~p ~n", [State]),
      NewState = execute_trade(State, SellerAccountId, BuyerAccountId, Offer, OfferId),
      io:format("[server process] after make deal: ~n~p ~n", [NewState]),
      {reply, delete_trader_offer, NewState};
    offer_do_not_exist -> {reply, delete_trader_offer, State};
    Other ->
      io:format("[server process] check_accepted_offer_valid result: ~p ~n", [Other]),
      {reply, keep_trader_offer, State}
  end;

%% for open_account request
handle_call({open_account , Holdings}, _From, State) ->
  {AccountId, NewState} = add_account(State, self(), Holdings),
  {reply, AccountId, NewState};

%% for open_account request
handle_call({shutdown}, _From, State) ->
  shutdown_all_traders(State),
  NewState1 = set_accounts_map(State, maps:new()),
  NewState2 = set_traders_map(NewState1, maps:new()),
  ExecutedTradesNum = get_executed_trades_num(NewState2),
  {reply, ExecutedTradesNum, NewState2};

%% for account_balance request
handle_call({account_balance , AccountId}, _From, State) ->
%%  io:format("accountId in handle_call : ~p ~n", [AccountId]),
  Holdings = get_account_by_id(State, AccountId),
  %% todo: what if no such account?
   {reply, Holdings, State};

%% for make_offer request
handle_call({make_offer , Acct, Offer}, _From, State) ->
  case check_valid_offer(Offer) of
    true ->
      {OfferId, NewState} = add_offer(self(), State, Acct, Offer),
      notify_all_trader_new_offer(NewState, OfferId, Offer, Acct),
      {reply, {ok, OfferId}, NewState};
    false ->
      {reply, {error, invalid_offer}, State}
  end;

handle_call({update_trader_map_only, NewState}, _From, _State) ->
  io:format("[server process] update_trader_map_only ~n~p~n",[NewState]),
  {reply, {ok}, NewState};

handle_call({add_trader, Strategy, AccountId}, _From, State) ->
  TraderId = get_current_trader_num(State) + 1,
%%  {_, TraderPid} = trader:start_trader(Strategy, self(), get_offers_map(State) , AccountId, TraderId),
  Sup = get_supervisor(State),
  ChildSpec = [Strategy, self(), get_offers_map(State) , AccountId, TraderId],
  Result = supervisor:start_child(Sup, ChildSpec),
  io:format("[server process] start_child result: ~p~n",[Result]),
  {_, TraderPid} = Result,
  NewState = set_current_trader_num(State, TraderId),
  TraderValue = {TraderPid, Strategy, AccountId},
  TraderMap = get_traders_map(NewState),
  NewTradeMap = maps:put(TraderId, TraderValue, TraderMap),
  NewState2 = set_traders_map(NewState, NewTradeMap),
  io:format("[server process] add trader successfully ~n"),
  {reply, TraderId, NewState2}.

%% for open_account request
handle_cast({remove_trader, TraderId}, State) ->
  case get_trader_by_id(State, TraderId) of
    trader_do_not_exist ->
      io:format("[server process] remove_trader but trader_do_not_exist ~n"),
      {noreply, State};
    {TraderPid, _, _} ->
      NewState = delete_trader_by_id(State, TraderId),
      supervisor:terminate_child(get_supervisor(State), TraderPid),
      io:format("[server process] remove trader state ~p~n",[NewState]),
      {noreply, NewState}
  end;

%% for rescind_offer request
handle_cast({rescind_offer, _Acct, OfferId}, State) ->
%%   todo when rescind offer, should notify child process to stop running strategy on this offer.
  NewState = delete_offer(State, OfferId),
  {noreply,NewState}.


shutdown_trader_list(Sup, TradersList) ->
  case TradersList of
    [X|XS] ->
      {_, {TraderPid, _, _}} = X,
      supervisor:terminate_child(Sup, TraderPid),
      shutdown_trader_list(Sup, XS);
    [] ->
      nothing
  end.

shutdown_all_traders(State) ->
  Sup = get_supervisor(State),
  TradesMap = get_traders_map(State),
  TradersList = maps:to_list(TradesMap),
  shutdown_trader_list(Sup, TradersList).

%%key offer_id(), value {account_id(), offer()}
%%{trader_pid, Strategy, AccountId }
notify_all_trader_new_offer(State, TraderList, OfferId, Offer, AccountId)->
  case TraderList of
    [X | XS] ->
      {_, TraderValue} = X,
      {TraderPid , _, _} = TraderValue,
      OfferElem = {OfferId, {AccountId, Offer}},
      trader:new_offer(TraderPid, OfferElem),
      notify_all_trader_new_offer(State, XS, OfferId, Offer, AccountId);
    [] ->
      nothing
  end.

notify_all_trader_new_offer(State, OfferId, Offer, AccountId) ->
  TradersMap = get_traders_map(State),
  TradersList = maps:to_list(TradersMap),
  notify_all_trader_new_offer(State, TradersList, OfferId, Offer, AccountId).

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


check_valid_offer(Offer) ->
  {StockName, Price} = Offer,
  case is_atom(StockName) of
    true ->
      case is_integer(Price) of
        true ->
          case Price >= 0 of
            true -> true;
            false -> false
          end;
        false -> false
      end;
    false -> false
  end.

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

get_trader_by_id(State, TraderId) ->
  TraderMap = get_traders_map(State),
  io:format("[server process] get_trader_by_id trader id : ~p ~n",[TraderId]),
  io:format("[server process] get_trader_by_id :~n~p ~n",[TraderMap]),
  maps:get(TraderId, TraderMap, trader_do_not_exist).

delete_offer_by_id(State, OfferId) ->
  OfferMap = get_offers_map(State),
  NewOffersMap = maps:remove(OfferId, OfferMap),
  NewOffersData = (get_offers_data(State))#offer_data{offers_map = NewOffersMap},
  State#se_server_data{offers = NewOffersData}.

delete_trader_by_id(State, TraderId) ->
  TraderMap = get_traders_map(State),
  NewTraderMap = maps:remove(TraderId, TraderMap),
  NewTraderData = (get_traders_data(State))#trader_data{traders_map = NewTraderMap},
  State#se_server_data{traders = NewTraderData}.

get_supervisor(State) ->
  State#se_server_data.supervisor.

set_account_by_id(State, SellerAccountId, AccountValue) ->
  AccountMap = get_accounts_map(State),
  NewAccountMap = maps:update(SellerAccountId ,AccountValue, AccountMap),
  set_accounts_map(State, NewAccountMap).

get_executed_trades_num(State)->
  State#se_server_data.executed_trades_num.

update_executed_trades_num(State) ->
  NewNum = get_executed_trades_num(State) + 1,
  State#se_server_data{executed_trades_num = NewNum}.

update_seller_holding(State, SellerAccountId, StockName, Price) ->
  Holding = get_account_by_id(State, SellerAccountId),
  {CurMoney, StockList} = Holding,
  {_, Amount} = lists:keyfind(StockName, 1, StockList),
  NewStockList = case Amount - 1 of
    0 -> lists:delete({StockName, Amount} ,StockList);
    _ -> lists:delete({StockName, Amount} ,StockList) ++ [{StockName, Amount - 1}]
  end,
  NewHolding = {CurMoney + Price, NewStockList},
  set_account_by_id(State, SellerAccountId, NewHolding).

update_buyer_holding(State, BuyerAccountId, StockName, Price) ->
  Holding = get_account_by_id(State, BuyerAccountId),
  {CurMoney, StockList} = Holding,
  case lists:keyfind(StockName, 1, StockList) of
    false ->
      NewStockList = StockList ++ [{StockName, 1}],
      NewHolding = {CurMoney - Price, NewStockList},
      set_account_by_id(State, BuyerAccountId, NewHolding);
    {_, Amount} ->
      NewStockList = lists:delete({StockName, Amount} ,StockList) ++ [{StockName, Amount + 1}],
      NewHolding = {CurMoney - Price, NewStockList},
      set_account_by_id(State, BuyerAccountId, NewHolding)
  end.