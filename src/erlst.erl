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
%% offers_map : key 为offer_id(), value为 {account_id(), offer()}
-record(account_data, {current_account_num::integer(), account_map}).
-record(offer_data, {current_offer_num::integer(), offers_map} ).
-record(se_server_data, {accounts, offers}).


-spec launch() -> {ok, stock_exchange()} | {error, term()}.
launch() ->
    gen_server:start_link(?MODULE, [] ,[]).

-spec shutdown(S :: stock_exchange()) -> non_neg_integer().
shutdown(_) ->
    not_implemented.

-spec open_account(S :: stock_exchange(), holdings()) -> account_id().
open_account(S, Holdings) ->
    gen_server:call(S, {open_account, Holdings}).

-spec account_balance(Acct :: account_id()) -> holdings().
account_balance(Acct) ->
  {_, ServerPid} = Acct,
  io:format("accountId in account_balance : ~p ", [ServerPid]),
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
add_trader(_, _) ->
    not_implemented.

-spec remove_trader(Acct :: account_id(), Trader :: trader_id()) -> ok.
remove_trader(_, _) ->
    not_implemented.

%% for debug only, return State in gen_server for test use.
observe_state(S) ->
  gen_server:call(S, {observe_state}).

init(_) ->
  State = #se_server_data{
    accounts=#account_data{current_account_num = 0, account_map = maps:new()},
    offers=#offer_data{ current_offer_num=0, offers_map = maps:new()}
  },
  {ok,  State}.

%% for open_account request
handle_call({observe_state}, _From, State) ->
  {reply, State, State};

%% for open_account request
handle_call({open_account , Holdings}, _From, State) ->
  {AccountId, NewAccounts} = add_account(self(), State#se_server_data.accounts, Holdings),
  NewState = State#se_server_data{accounts = NewAccounts},
  {reply, AccountId, NewState};

%% for account_balance request
handle_call({account_balance , AccountId}, _From, State) ->
%%  io:format("accountId in handle_call : ~p ~n", [AccountId]),
  Holdings = get_account_data(State#se_server_data.accounts, AccountId),
  %% todo: what if no such account?
   {reply, Holdings, State};

%% for make_offer request
handle_call({make_offer , Acct, Offer}, _From, State) ->
  Holdings = get_account_data(State#se_server_data.accounts , Acct),
  case check_valid_offer(Offer, Holdings) of
    true ->
      {OfferId, NewState} = add_offer(self(), State, Acct, Offer),
%%      io:format("NewState :  ~p ~n", [NewState]),
      {StockName, _} = Offer,
      NewState2 = decrease_account_stock_num(NewState, Acct, StockName),
%%      io:format("NewState2 :  ~p ~n", [NewState2]),
      {reply, {ok, OfferId}, NewState2};
    false ->
      {reply, {error, not_enough_stock_to_offer}, State}
  end.

%% for rescind_offer request
handle_cast({rescind_offer, Acct, OfferId}, State) ->
  io:format("State before rescind_offer :  ~p ~n", [State]),
  {_, Offer} = get_offer_data( State#se_server_data.offers, OfferId),
  NewState = delete_offer(State, OfferId),
  {StockName, _} = Offer,
  NewState2 = increase_account_stock_num(NewState, Acct, StockName),
  io:format("State after rescind_offer :  ~p ~n", [NewState2]),
  {noreply,NewState2}.

%% account utils functions
add_account(Server, Accounts, Holdings) ->
  Id = Accounts#account_data.current_account_num + 1,
  NewAccountMap = maps:put({Id, Server}, Holdings , Accounts#account_data.account_map),
  NewAccounts = Accounts#account_data{current_account_num = Id, account_map = NewAccountMap},
  {{Id, Server}, NewAccounts}.

get_account_data(AccountsData ,AccountId) ->
  maps:get(AccountId, AccountsData#account_data.account_map).

decrease_account_stock_num(State, AccountId, StockName) ->
  AccountMap = (State#se_server_data.accounts)#account_data.account_map,
  AccountValue = maps:get(AccountId ,AccountMap),
  {CurrentMoney, StockList} = AccountValue,
  case lists:keyfind(StockName, 1, StockList) of
    { _ , Amount} ->
      NewStockInfo = {StockName , Amount - 1},
      NewStockList = lists:delete({StockName, Amount}, StockList) ++ [NewStockInfo],
      NewAccountMap = maps:update(AccountId, {CurrentMoney, NewStockList}, AccountMap),
      NewAccountsData = (State#se_server_data.accounts)#account_data{account_map = NewAccountMap},
      State#se_server_data{accounts = NewAccountsData}
  end.

increase_account_stock_num(State, AccountId, StockName) ->
  AccountMap = (State#se_server_data.accounts)#account_data.account_map,
  AccountValue = maps:get(AccountId ,AccountMap),
  {CurrentMoney, StockList} = AccountValue,
  case lists:keyfind(StockName, 1, StockList) of
    { _ , Amount} ->
      NewStockInfo = {StockName , Amount + 1},
      NewStockList = lists:delete({StockName, Amount}, StockList) ++ [NewStockInfo],
      NewAccountMap = maps:update(AccountId, {CurrentMoney, NewStockList}, AccountMap),
      NewAccountsData = (State#se_server_data.accounts)#account_data{account_map = NewAccountMap},
      State#se_server_data{accounts = NewAccountsData}
  end.

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

get_offer_data(OffersData ,OfferId) ->
  maps:get(OfferId , OffersData#offer_data.offers_map).

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
