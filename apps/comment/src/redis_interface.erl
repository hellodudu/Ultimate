-module(redis_interface).

-include("role.hrl").

-export([
         load_role/1,
         save_role/2,
         hmget/1
        ]).

%% 读取角色
load_role(RoleID) ->
    RetRoleRecord = #role{},
    {ok, RoleID1} = redis_query:get(io_lib:format("role:~B:role_id", [RoleID])),
    {ok, AccountID} = redis_query:get(io_lib:format("role:~B:account_id", [RoleID])),
    {ok, Name} = redis_query:get(io_lib:format("role:~B:name", [RoleID])),
    {ok, Sex} = redis_query:get(io_lib:format("role:~B:sex", [RoleID])),
    {ok, Level} = redis_query:get(io_lib:format("role:~B:level", [RoleID])),
    {ok, Diamond} = redis_query:get(io_lib:format("role:~B:diamond", [RoleID])),
    RetRoleRecord1 = RetRoleRecord#role{role_id = binary_to_list(RoleID1), account_id = binary_to_list(AccountID), name = binary_to_list(Name), 
                                        sex = binary_to_list(Sex), level = binary_to_list(Level), diamond = binary_to_list(Diamond)},

    lager:info("redis load_role result = ~p~n", [RetRoleRecord1]),
    RetRoleRecord1.

save_role(RoleID, RoleRec) ->
    redis_query:set(io_lib:format("role:~B:role_id", [RoleID]), RoleRec#role.role_id),
    redis_query:set(io_lib:format("role:~B:account_id", [RoleID]), RoleRec#role.account_id),
    redis_query:set(io_lib:format("role:~B:name", [RoleID]), RoleRec#role.name),
    redis_query:set(io_lib:format("role:~B:sex", [RoleID]), RoleRec#role.sex),
    redis_query:set(io_lib:format("role:~B:level", [RoleID]), RoleRec#role.level),
    redis_query:set(io_lib:format("role:~B:diamond", [RoleID]), RoleRec#role.diamond),
    true.

hmget(RoleID) ->
    Result = redis_query:hmget(io_lib:format("role:~B:role_id", [RoleID]), ["name", "age", "birthday"]),
    lager:info("redis_interface hmget result = ~p~n", [Result]).

hmset(RoleRecord) ->
    KeyValuePairs = [io_lib:format("role:~B:name", [RoleRecord#role.role_id]), RoleRecord#role.name, io_lib:format("role:~B:level", [RoleRecord#role.level])],
    lager:info("redis_interface KeyValuePairs = ~p~n", [KeyValuePairs]),
    Result = redis_query:hmset(KeyValuePairs),
    lager:info("redis_interface hmset result = ~p~n", [Result]).
