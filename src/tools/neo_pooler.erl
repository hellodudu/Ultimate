-module(neo_pooler).

-export([pick_member/1,
         checkout/1,
         checkin/2,
         transaction/2,
         transaction/3]).

-define(MAX_RETRIES, 3).

pick_member(PoolName) ->
    pooler:pick_member(PoolName).

checkout(PoolName) ->
    pooler:take_member(PoolName).

checkin(PoolName, Connection) ->
    pooler:return_member(PoolName, Connection).

transaction(PoolName, Fun) ->
    transaction(PoolName, Fun, ?MAX_RETRIES).

transaction(_PoolName, _Fun, 0) ->
    {error, no_connections};
transaction(PoolName, Fun, Retries) ->
    case checkout(PoolName) of
        error_no_members ->
            {error, no_connections};
        Conn when is_pid(Conn) ->
            case Fun(Conn) of
                {error, closed} ->
                    transaction(PoolName, Fun, Retries - 1);
                Result ->
                    % checkin(PoolName, Conn),
                    Result
            end
    end.
