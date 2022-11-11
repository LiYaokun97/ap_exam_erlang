-module(sup).

-export([start_link/0]).

-behaviour(supervisor).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  {ok, Sup} = supervisor:start_link( ?MODULE, []),
  Sup.

%% @private
init(_Args) ->
  SupFlags = #{strategy => simple_one_for_one, intensity => 1, period => 5},
  ChildSpecs = [#{id => trader,
    start => {trader, start_trader, []},
    restart => permanent,
    shutdown => brutal_kill,
    type => worker,
    modules => [trader]}],

  {ok, {SupFlags, ChildSpecs}}.
