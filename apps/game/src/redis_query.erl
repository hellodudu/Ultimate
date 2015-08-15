-module(redis_query).
-define(REDIS_POOL, redis_pool).
-define(TIMEOUT, 5000).

-export([
         get/1,
         set/2,
         expire/2,
         delete/1,
         hmget/2,
         hmset/1
        ]).

%% query
q(Command) ->
    q(Command, ?TIMEOUT).

q(Command, Timeout) ->
    poolboy:transaction(?REDIS_POOL, fun(Worker) ->
                                             gen_server:call(Worker, {q, Command}, Timeout)
                                     end).

%% query pipeline
qp(Command) ->
    qp(Command, ?TIMEOUT).

qp(Command, Timeout) ->
    poolboy:transaction(?REDIS_POOL, fun(Worker) ->
                                             gen_server:call(Worker, {qp, Command}, Timeout)
                                     end).

get(Key) -> 
    q(["GET", Key]).

set(Key, Value) -> 
    q(["SET", Key, Value]).

expire(Key, Time) -> 
    q(["EXPIRE", Key, Time]).

delete(Key) -> 
    q(["DEL", Key]).

hmget(Key, Fields) ->
    Ret = q(["HMGET", Key | Fields]),
    lager:info("hmget result = ~p~n", [Ret]).

hmset(KeyValuePairs) ->
    Ret = q(["HMSet" | KeyValuePairs]),
    lager:info("hmset result = ~p~n", [Ret]).
