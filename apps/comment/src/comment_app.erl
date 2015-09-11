-module(comment_app).
-behaviour(application).
-include("../../include/connector.hrl").
-include("../../include/role.hrl").
-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    %% 初始化连接器
    {ok,[[PortString]]} = init:get_argument(port),
    Port = erlang:list_to_integer(PortString),
    pass = case ranch:start_listener(comment, 
                                   2000, 
                                   ranch_tcp, 
                                   [{port, Port}],
                                   connector, 
                                   []) of
        {ok,_ } -> 
            lager:info("ranch start_listener success"),
            pass;
        RanchError -> 
            lager:error("Cannot start ranch with Error: ~p",RanchError),
            error
    end,

    Ret = case comment_sup:start_link() of
        {ok, Pid} ->
            lager:info("game_sup start ok"),
            {ok, Pid};
        Error ->
            lager:info("game_sup start error=~p", [Error]),
            {error, Error}
    end,

    lager:info("game start on node ~p, listening on port ~p", [erlang:node(),Port]),
    Ret.

stop(_State) ->
    ok.

