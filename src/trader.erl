%%%-------------------------------------------------------------------
%%% @author liyk1
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. 11月 2022 22:30
%%%-------------------------------------------------------------------
-module(trader).
-author("liyk1").
-behavior(gen_server).

%% API
-export([init/1, handle_call/3, handle_cast/2]).
-export([start_trader/5, new_offer/2]).


start_trader(Strategy, Sup, CurOffers, AccountId, TraderId) ->
  {ok, Pid} = gen_server:start_link(?MODULE, {Sup, CurOffers, Strategy, AccountId, TraderId}, []),
  try_accept_all_offers(Pid),
  {ok, Pid}.

%% only pass AccountId to trader, the trader doesn't care the holdings in the account
init({Sup, CurOffers, Strategy, AccountId, TraderId}) ->
  MyMap = maps:new(),
  MyMap1 = maps:put("Sup", Sup, MyMap),
  MyMap2 = maps:put("CurOffers", CurOffers, MyMap1),
  MyMap3 = maps:put("Strategy", Strategy, MyMap2),
  MyMap4 = maps:put("AccountId", AccountId, MyMap3),
  MyMap5 = maps:put("TraderId", TraderId, MyMap4),
  {ok,  MyMap5}.

try_accept_all_offers(Trader) ->
  gen_server:cast(Trader, {try_accept_all_offers}).

new_offer(Trader, OfferElem) ->
  gen_server:cast(Trader, {new_offer, OfferElem}).

handle_call(_Request, _From, _State) ->
  erlang:error(not_implemented).

handle_cast({try_accept_all_offers}, State) ->
  Strategy = get_strategy_from_state(State),
  CurOffers = get_cur_offers_from_state(State),
  CurOffersList = maps:to_list(CurOffers),
  NewState = implement_strategy_to_offers(State, CurOffersList, Strategy),
  {noreply, NewState};

handle_cast({new_offer, OfferElem}, State) ->
  io:format("[trader process ~p] new offer from server: ~p ~n", [self(), OfferElem]),
  Strategy = get_strategy_from_state(State),
  CurOffersMap = get_cur_offers_from_state(State),
  {Key, Value} = OfferElem,
  NewCurOffersMap = maps:put(Key , Value, CurOffersMap),
  NewState = set_cur_offers(State, NewCurOffersMap),
  CurOffersList = maps:to_list(NewCurOffersMap),
  NewState2 = implement_strategy_to_offers(NewState, CurOffersList, Strategy),
  {noreply, NewState2}.

accept_offer(State , OfferElem, OfferId) ->
  Sup = get_sup_from_state(State),
  AccountId = get_accounts_id_state(State),
  TraderId = get_trader_id(State),
  case erlst:accept_offer(Sup, OfferElem, AccountId, TraderId) of
    keep_trader_offer ->
      io:format("[trader process ~p] get response from server: keep_trader_offer ~n", [self()]),
      State;
    delete_trader_offer->
      io:format("[trader process ~p] get response from server: delete_trader_offer ~n", [self()]),
      delete_offer_by_id(State, OfferId)
  end.

%% account_map : key 为account_id(), value为holdings()
%% offers_map : key 为offer_id(), value为 {account_id(), offer()}
implement_strategy_to_offers(State, CurOffersList, Strategy) ->
  case CurOffersList of
    [X |XS] ->
      {Key, Value} = X,
      {_, Offer} = Value,
      Result = try Strategy(Offer) of
        accept ->
          accept;
        reject ->
          reject
      catch
        _:_ ->
          reject
      end,
      case Result of
        accept ->
          io:format("[trader process ~p] accept one offer: ~p ~n",[self(), Offer]),
          NewState = accept_offer(State, X, Key),
          implement_strategy_to_offers(NewState, XS, Strategy);
        reject ->
          io:format("[trader process ~p] reject one offer:~p ~n",[self(), Offer]),
          NewState = delete_offer_by_id(State, Key),
          implement_strategy_to_offers(NewState, XS, Strategy)
      end;
    [] ->
      io:format("[trader process ~p] implement_strategy_to_offers done ~n",[self()]),
      State
  end.

delete_offer_by_id(State, OfferId) ->
  CurOffersMap = get_cur_offers_from_state(State),
  NewCurOffersMap = maps:remove(OfferId, CurOffersMap),
  set_cur_offers(State, NewCurOffersMap).

%% State util function
get_sup_from_state(State) ->
  maps:get("Sup", State).

get_cur_offers_from_state(State) ->
  maps:get("CurOffers", State).

set_cur_offers(State, CurOffersMap) ->
  maps:update("CurOffers", CurOffersMap, State).

get_strategy_from_state(State) ->
  maps:get("Strategy", State).

get_accounts_id_state(State) ->
  maps:get("AccountId", State).

get_trader_id(State) ->
  maps:get("TraderId", State).
