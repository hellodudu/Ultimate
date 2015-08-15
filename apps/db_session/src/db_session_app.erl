-module(db_session_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    crypto:start(),
    emysql:add_pool(my_pool, 1, "root", "123qwe", "localhost", 3306, "db_game", utf8),
    db_session_sup:start_link().

stop(State) ->
    lager:info( "dbsession stopped at node ~p", [erlang:node()]),
    State.


