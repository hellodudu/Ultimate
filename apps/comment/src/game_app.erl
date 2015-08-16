-module(game_app).
-behaviour(application).
-include("../../include/connector.hrl").
-include("../../include/role.hrl").
-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    %% 初始化连接器
    {ok,[[PortString]]} = init:get_argument(port),
    Port = erlang:list_to_integer(PortString),
    pass = case ranch:start_listener(game, 
                                   2000, 
                                   ranch_tcp, 
                                   [{port, Port},
                                    {active, once}, 
                                    {packet, 0},
                                    {reuseaddr, true},
                                    {keepalive, true},
                                    {max_connections, 256}], 
                                   connector, 
                                   []) of
        {ok,_ } -> 
            lager:info("ranch start_listener success"),
            pass;
        RanchError -> 
            lager:error("Cannot start ranch with Error: ~p",RanchError),
            error
    end,

    Ret = case game_sup:start_link() of
        {ok, Pid} ->
            lager:info("game_sup start ok"),
            {ok, Pid};
        Error ->
            lager:info("game_sup start error=~p", [Error]),
            {error, Error}
    end,

    lager:info("game start on node ~p, listening on port ~p", [erlang:node(),Port]),
    register(game_connector, spawn(fun() -> update_connect() end)),
    Ret.

stop(_State) ->
    ok.

%% 更新连接
update_connect() ->
    receive
        {init_role, List} ->
            lager:info("db_session connect success!"),
            role_mgr ! {init_db_role, List}
    after 3000 ->
        lager:info("db_session did not connected yet!"),
        update_connect()
    end.
