%% neo key value store.

-module(neo_kvs).

-export([start_link/2]).
-export([start_link/3]).

-export([q/2]).

%% keys commands
-export([del/2,
         del/3,
         mdel/2,
		 exists/2,
		 expire/3,
		 expireat/3,
		 keys/2,
		 move/3,
		 persist/2,
		 rename/3,
		 renamenx/3,
		 ttl/2,
		 type/2
		 ]).

%% strings commands
-export([append/3,
		 decr/2,
		 decrby/3,
		 get/2,
		 get/3,
		 getint/2,
		 getint/3,
		 getbit/3,
		 getrange/4,
		 getset/3,
		 getset/4,
		 incr/2,
		 incrby/3,
		 mget/2,
		 mset/2,
		 msetnx/2,
		 set/3,
		 setbit/4,
		 setex/4,
		 setnx/3,
		 setrange/4,
		 strlen/2
		]).

%% hashes commands
-export([hdel/3,
         hdel/4,
		 hmdel/3,
		 hexists/3,
		 hget/3,
		 hget/4,
		 hgetint/3,
		 hgetint/4,
		 hgetall/2,
		 hincr/3,
		 hincrby/4,
		 hkeys/2,
		 hlen/2,
		 hmget/3,
		 hmset/3,
		 hmpset/3,
		 hset/4,
		 hsetnx/4,
		 hvals/2
		 ]).

%% lists commands
-export([lindex/3,
		 linsertbefore/4,
		 linsertafter/4,
		 llen/2,
		 lpop/2,
		 lpop/3,
		 blpop/2,
		 blpop/3,
		 lpush/3,
		 lmpush/3,
		 lpushx/3,
		 lrange/4,
		 lrem/4,
		 lset/4,
		 ltrim/4,
		 rpop/2,
		 rpop/3,
		 brpop/2,
		 brpop/3,
		 rpoplpush/3,
		 rpush/3,
         rmpush/3,
		 rpushx/3
		 ]).

%% sets commands
-export([sadd/3,
		 smadd/3,
		 scard/2,
		 sdiff/2,
		 sdiffstore/3,
		 sinter/2,
		 sinterstore/3,
		 sismember/3,
		 smembers/2,
		 smove/4,
		 spop/2,
		 srandmember/2,
		 srandmember/3,
		 srem/3,
		 smrem/3,
		 sunion/2,
		 sunionstore/3,
		 sortget/3
		]).

-export([zadd/4,
         mzadd/3,
		 zrevrange/4,
         zrangebyscore/2,
         zrangebyscore/3,
         zrangebyscore/4
		]).

%% transaction commands
-export([multi/1,
         exec/1,
         transaction/3
        ]).

%% pipeline
-export([qp/2]).

start_link(Host, Port) ->
    eredis:start_link(Host, Port).

%% wrapper eredis start_link, register redis pid with name
start_link(Name, Host, Port) ->
    {ok, C} = eredis:start_link(Host, Port),
    register(Name, C),
	{ok, C}. %% cooperate with supervisor, we return {ok, Pid}.

q(C, Command) ->
    Conn = neo_pooler:pick_member(C),
    eredis:q(Conn, Command).

qp(C, Pipeline) ->
    Conn = neo_pooler:pick_member(C),
    eredis:qp(Conn, Pipeline).

%% keys commands

del(C, Key) ->
	q(C, ["DEL", Key]).
del(C, Key, Value) ->
      {ok, R} = q(C, [<<"EVAL">>,
                      <<"if redis.call(\"GET\",KEYS[1]) == KEYS[2] then return redis.call(\"DEL\",KEYS[1]) else return nil end">>, <<"2">>, Key, Value]),
      case R of
          undefined -> false;
          <<"1">>   -> true
      end.

mdel(C, Keys) ->
    q(C, lists:append(["DEL"], Keys)).

exists(C, Key) ->
	case q(C, ["EXISTS", Key]) of
		{ok, <<"0">>} -> false;
		{ok, <<"1">>} -> true;
		Error -> throw(Error)
	end.

expire(C, Key, Seconds) ->
	q(C, ["EXPIRE", Key, Seconds]).

expireat(C, Key, Timestamp) ->
	q(C, ["EXPIREAT", Key, Timestamp]).

keys(C, Patten) ->
	{ok, R} = q(C, ["KEYS", Patten]),
	R.

move(C, Key, Db) ->
	q(C, ["MOVE", Key, Db]).

persist(C, Key) ->
	q(C, ["PERSIST", Key]).

rename(C, Key, NewKey) ->
	q(C, ["RENAME", Key, NewKey]).

renamenx(C, Key, NewKey) ->
	q(C, ["RENAMENX", Key, NewKey]).

ttl(C, Key) ->
	{ok, B} = q(C, ["TTL", Key]),
	neo_util:to_int(B).

type(C, Key) ->
	{ok, B} = q(C, ["TYPE", Key]),
	binary_to_atom(B, utf8).

%% strings command
append(C, Key, Val) ->
	q(C, ["APPEND", Key, Val]).

decr(C, Key) ->
	{ok, B} = q(C, ["DECR", Key]),
	neo_util:to_int(B).

decrby(C, Key, Decrement) ->
	{ok, B} = q(C, ["DECRBY", Key, Decrement]),
	neo_util:to_int(B).

get(C, Key) ->
	get(C, Key, <<>>).

get(C, Key, Default) ->
	case q(C, ["GET", Key]) of
		{ok, undefined} ->
			Default;
		{ok, Val} ->
			Val;
		Error -> throw(Error)
	end.

getset(C, Key, Val) ->
	getset(C, Key, Val, <<>>).

getset(C, Key, Val, Default) ->
	case q(C, ["GETSET", Key, Val]) of
		{ok, undefined} ->
			Default;
		{ok, Val2} ->
			Val2;
		Error -> throw(Error)
	end.

getint(C, Key) ->
	getint(C, Key, -1).

getint(C, Key, Default) ->
	{ok, R} = q(C, ["GET", Key]),
	neo_util:to_int(R, Default).
	
getbit(C, Key, Offset) ->
	{ok, R} = q(C, ["GETBIT", Key, Offset]),
	R.

getrange(C, Key, Start, End) ->
	{ok, R} = q(C, ["GETRANGE", Key, Start, End]),
	R.

incr(C, Key) ->
	{ok, B} = q(C, ["INCR", Key]),
	neo_util:to_int(B).

incrby(C, Key, Increment) ->
	{ok, B} = q(C, ["INCRBY", Key, Increment]),
	neo_util:to_int(B).

mget(C, Keys) ->
	{ok, R} = q(C, ["MGET" | Keys]),
	R.

mset(C, KeyVals) ->
	q(C, ["MSET" | KeyVals]).

msetnx(C, KeyVals) ->
	q(C, ["MSETNX" | KeyVals]).

set(C, Key, Val) ->
	q(C, ["SET", Key, Val]).

setbit(C, Key, Offset, Val) ->
	q(C, ["SETBIT", Key, Offset, Val]).

setex(C, Key, Seconds, Val) ->
	q(C, ["SETEX", Key, Seconds, Val]).

setnx(C, Key, Val) ->
	q(C, ["SETNX", Key, Val]).

setrange(C, Key, Offset, Val) ->
	q(C, ["SETRANGE", Key, Offset, Val]).

strlen(C, Key) ->
	{ok, B} = q(C, ["STRLEN", Key]),
	neo_util:to_int(B).

%% hashes command
hdel(C, Key, Field) ->
	q(C, ["HDEL", Key, Field]).
hdel(C, Key, Field, Value) ->
      {ok, R} = q(C, [<<"EVAL">>,
                      <<"if redis.call(\"HGET\",KEYS[1],KEYS[2]) == KEYS[3] then return redis.call(\"HDEL\",KEYS[1],KEYS[2]) else return nil end">>,
                      <<"3">>, Key, Field, Value]),
      case R of
          undefined -> false;
          <<"1">>   -> true
      end.

hmdel(C, Key, Fields) ->
	q(C, ["HDEL", Key|Fields]).

hexists(C, Key, Field) ->
	case q(C, ["HEXISTS", Key, Field]) of
		{ok, <<"0">>} -> false;
		{ok, <<"1">>} -> true;
		Error -> throw(Error)
	end.

hget(C, Key, Field) ->
	hget(C, Key, Field, <<>>).

hget(C, Key, Field, Default) ->
	case q(C, ["HGET", Key, Field]) of
		{ok, undefined} -> Default;
		{ok, R} -> R;
		Error -> throw(Error)
	end.
	
hgetint(C, Key, Field) ->
	hgetint(C, Key, Field, -1).

hgetint(C, Key, Field, Default) ->
	{ok, R} = q(C, ["HGET", Key, Field]),
	neo_util:to_int(R, Default).

hgetall(C, Key) ->
	{ok, R} = q(C, ["HGETALL", Key]),
	neo_util:to_proplist(R).

hincr(C, Key, Field) ->
	hincrby(C, Key, Field, 1).

hincrby(C, Key, Field, Increment) ->
	q(C, ["HINCRBY", Key, Field, Increment]).

hkeys(C, Key) ->
	{ok, L} = q(C, ["HKEYS", Key]),
	L.

hlen(C, Key) ->
	{ok, B} = q(C, ["HLEN", Key]),
	neo_util:to_int(B).

hmget(C, Key, Fields) ->
	{ok, L} = q(C, ["HMGET", Key | Fields]),
	L.

hmpset(C, Key, FieldVals) ->
	Vals2 = lists:foldl(fun({Field, Val}, Acc) ->
							[Field, Val | Acc]
					    end, [], FieldVals),
	hmset(C, Key, Vals2).

hmset(C, Key, Vals) ->
	q(C, lists:append(["HMSET", Key], Vals)).

hset(C, Key, Field, Val) ->
	q(C, ["HSET", Key, Field, Val]).

hsetnx(C, Key, Field, Val) ->
	q(C, ["HSETNX", Key, Field, Val]).

hvals(C, Key) ->
	{ok, L} = q(C, ["HVALS", Key]),
	L.

%% lists commands

lindex(C, Key, Index) ->
	{ok, R} = q(C, ["LINDEX", Key, Index]),
	R.

linsertbefore(C, Key, Pivot, Val) ->
	q(C, ["LINSERT", Key, "BEFORE", Pivot, Val]).

linsertafter(C, Key, Pivot, Val) ->
	q(C, ["LINSERT", Key, "AFTER", Pivot, Val]).

llen(C, Key) ->
	{ok, R} = q(C, ["LLEN", Key]),
	list_to_integer(binary_to_list(R)).

lpop(C, Key) ->
	lpop(C, Key, undefined).

lpop(C, Key, Default) ->
	case q(C, ["LPOP", Key]) of
		{ok, undefined} ->
			Default;
		{ok, Val} ->
			Val;
		Error -> throw(Error)
	end.

blpop(C, Key) ->
	blpop(C, Key, 0).

blpop(C, Key, Timeout) ->
	case q(C, ["BLPOP", Key, Timeout]) of
        {ok, [Key, Value]} ->
            [Key, Value];
		Error -> Error
	end.

lpush(C, Key, Val) ->
	q(C, ["LPUSH", Key, Val]).

lmpush(C, Key, Vals) ->
	q(C, ["LPUSH", Key | Vals]).

lpushx(C, Key, Val) ->
	q(C, ["LPUSHX", Key, Val]).

lrange(C, Key, Start, Stop) ->
	{ok, R} = q(C, ["LRANGE", Key, Start, Stop]),
	R.

lrem(C, Key, Count, Val) ->
	q(C, ["LREM", Key, Count, Val]).

lset(C, Key, Index, Val) ->
	q(C, ["LSET", Key, Index, Val]).

ltrim(C, Key, Start, Stop) ->
	q(C, ["LTRIM", Key, Start, Stop]).

rpop(C, Key) ->
	rpop(C, Key, undefined).

rpop(C, Key, Default) ->
	case q(C, ["RPOP", Key]) of
		{ok, undefined} ->
			Default;
		{ok, Val} ->
			Val;
		Error -> throw(Error)
	end.

brpop(C, Key) ->
	brpop(C, Key, 0).

brpop(C, Key, Timeout) ->
	case q(C, ["BRPOP", Key, Timeout]) of
        {ok, [Key, Value]} ->
            [Key, Value];
		Error -> Error
	end.

rpoplpush(C, Src, Dest) ->
	q(C, ["RPOPLPUSH", Src, Dest]).

rpush(C, Key, Val) ->
	q(C, ["RPUSH", Key, Val]).

rmpush(C, Key, Vals) ->
	q(C, ["RPUSH", Key | Vals]).

rpushx(C, Key, Val) ->
	q(C, ["RPUSHX", Key, Val]).

%% set commands

sadd(C, Key, Val) ->
	q(C, ["SADD", Key, Val]).

smadd(_C, _Key, []) ->
    {ok, -1};
smadd(C, Key, Vals) ->
	q(C, ["SADD", Key | Vals]).

scard(C, Key) ->
	{ok, B} = q(C, ["SCARD", Key]),
	neo_util:to_int(B).

sdiff(C, Keys) ->
	{ok, L} = q(C, ["SDIFF" | Keys]),
	L.

sdiffstore(C, Dest, Keys) ->
	{ok, B} = q(C, ["SDIFFSTORE", Dest | Keys]),
	neo_util:to_int(B).

sinter(C, Keys) ->
	{ok, L} = q(C, ["SINTER" | Keys]),
	L.

sinterstore(C, Dest, Keys) ->
	{ok, B} = q(C, ["SINTERSTORE", Dest | Keys]),
	neo_util:to_int(B).

sismember(C, Key, Val) ->
	case q(C, ["SISMEMBER", Key, Val]) of
		{ok, <<"1">>} -> true;
		{ok, <<"0">>} -> false
	end.

smembers(C, Key) ->
	{ok, L} = q(C, ["SMEMBERS", Key]),
	L.

smove(C, Src, Dest, Member) ->
	case q(C, ["SMOVE", Src, Dest, Member]) of
		{ok, <<"0">>} -> {error, not_member};
		{ok, <<"1">>} -> ok
	end.

spop(C, Key) ->
	{ok, R} = q(C, ["SPOP", Key]),
	R.

srandmember(C, Key) ->
	{ok, R} = q(C, ["SRANDMEMBER", Key]),
	R.

srandmember(C, Key, Count) ->
	{ok, R} = q(C, ["SRANDMEMBER", Key, Count]),
	R.

srem(C, Key, Val) ->
	q(C, ["SREM", Key, Val]).

smrem(C, Key, Vals) ->
	q(C, ["SREM", Key|Vals]).

sunion(_C, []) ->
    [];
sunion(C, Keys) ->
	{ok, L} = q(C, ["SUNION" | Keys]),
	L.

sunionstore(C, Dest, Keys) ->
	{ok, B} = q(C, ["SUNIONSTORE", Dest | Keys]),
	neo_util:to_int(B).

sortget(C, Source, Keys) ->
	Leng = length(Keys),
	GetList = lists:append([["GET", X] || X <- Keys]),
	KeyList = ["SORT", Source] ++ GetList,
	{ok, B} = q(C, KeyList),
	neo_util:list_slice(B, Leng).

multi(C) ->
    q(C, ["MULTI"]).

exec(C) ->
    q(C, ["EXEC"]).

transaction(C, Fun, Args) ->
    neo_pooler:transaction(C,
        fun(Conn) ->
            {ok, <<"OK">>} = eredis:q(Conn, ["MULTI"]),
            erlang:apply(Fun, [Conn | Args]),
            {ok, Result} = eredis:q(Conn, ["EXEC"]),
            Result
        end).

zadd(C, Key, Score, Val) ->
	q(C, ["ZADD", Key, Score, Val]).

mzadd(C, Key, SVs) when is_list(SVs) ->
	q(C, ["ZADD", Key] ++ SVs).

zrevrange(C, Key, Start, Stop) ->
	q(C, ["ZREVRANGE", Key, Start, Stop]).

zrangebyscore(C, Key) ->
    zrangebyscore(C, Key, "-inf", "+inf").

zrangebyscore(C, Key, Min) ->
    zrangebyscore(C, Key, Min, "+inf").

zrangebyscore(C, Key, Min, Max) ->
    q(C, ["ZRANGEBYSCORE", Key, Min, Max, "WITHSCORES"]).
