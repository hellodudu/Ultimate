-module(db_session_sup).
-behaviour(supervisor).

-include("../../include/session.hrl").
-export([start_link/0,init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    DBChild = {db_session, {db_session, start, []}, permanent, 5000, worker, [db_session]},
    {ok, {{one_for_one, 10, 30}, [DBChild]}}.
