-module(erlst).

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
         remove_trader/2
        ]).

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

-spec launch() -> {ok, stock_exchange()} | {error, term()}.
launch() ->
    not_implemented.

-spec shutdown(S :: stock_exchange()) -> non_neg_integer().
shutdown(_) ->
    not_implemented.

-spec open_account(S :: stock_exchange(), holdings()) -> account_id().
open_account(_, _) ->
    not_implemented.

-spec account_balance(Acct :: account_id()) -> holdings().
account_balance(_) ->
    not_implemented.

-spec make_offer(Acct :: account_id(), Terms :: offer()) -> {ok, offer_id()} | {error, term()}.
make_offer(_, _) ->
    not_implemented.

-spec rescind_offer(Acct :: account_id(), Offer :: offer_id()) -> ok.
rescind_offer(_, _) ->
    not_implemented.

-spec add_trader(Acct :: account_id(), Strategy :: trader_strategy()) -> trader_id().
add_trader(_, _) ->
    not_implemented.

-spec remove_trader(Acct :: account_id(), Trader :: trader_id()) -> ok.
remove_trader(_, _) ->
    not_implemented.
