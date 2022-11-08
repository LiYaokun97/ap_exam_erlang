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
-export([start_trader/4, new_offer/2]).


start_trader(Strategy , Sup, CurOffers, AccountId) ->
  {ok, Pid} = gen_server:start_link(?MODULE, {Sup, CurOffers, Strategy, AccountId}, []),
  try_accept_all_offers(Pid),
  Pid.

%% only pass AccountId to trader, the trader doesn't care the holdings in the account
init({Sup, CurOffers, Strategy, AccountId}) ->
  MyMap = maps:new(),
  MyMap1 = maps:put("Sup", Sup, MyMap),
  MyMap2 = maps:put("CurOffers", CurOffers, MyMap1),
  MyMap3 = maps:put("Strategy", Strategy, MyMap2),
  MyMap4 = maps:put("AccountId", AccountId, MyMap3),
  {ok,  MyMap4}.

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
  Strategy = get_strategy_from_state(State),
  CurOffersMap = get_cur_offers_from_state(State),
  {Key, Value} = OfferElem,
  NewCurOffersMap = maps:update(Key , Value, CurOffersMap),
  NewState = set_cur_offers(State, NewCurOffersMap),
  CurOffersList = maps:to_list(NewCurOffersMap),
  NewState2 = implement_strategy_to_offers(NewState, CurOffersList, Strategy),
  {noreply, NewState2}.

accept_offer(State , OfferElem, OfferId) ->
  Sup = get_sup_from_state(State),
  AccountId = get_accounts_id_state(State),
  case erlst:accept_offer(Sup, OfferElem, AccountId) of
    keep_trader_offer ->
      io:format("[trader process] get response from server~n"),
      State;
    delete_trader_offer->
      io:format("[trader process] get response from server delete offer ~n"),
      delete_offer_by_id(State, OfferId)
  end.

%% account_map : key 为account_id(), value为holdings()
%% offers_map : key 为offer_id(), value为 {account_id(), offer()}
implement_strategy_to_offers(State, CurOffersList, Strategy) ->
  case CurOffersList of
    [X |XS] ->
      {Key, Value} = X,
      {_, Offer} = Value,
      case Strategy(Offer) of
        accept ->
          io:format("[trader process] accept offer~n"),
          NewState = accept_offer(State, X, Key),
          implement_strategy_to_offers(NewState, XS, Strategy);
        reject ->
          io:format("[trader process] reject offer~n"),
          NewState = delete_offer_by_id(State, Key),
          implement_strategy_to_offers(NewState, XS, Strategy)
      end;
    [] ->
      io:format("[trader process] implement_strategy_to_offers done ~n"),
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


