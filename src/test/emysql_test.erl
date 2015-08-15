-module(emysql_test).

-export([add/0]).
-export([insert/0, select/0]).
-record(hello_record, {role_id, account_id, name, sex, level, diamond}).

add() ->
    crypto:start(),
    application:start(emysql),
    emysql:add_pool(my_pool, [{size, 1}, {user, "root"}, {password, "123qwe"}, {database, "db_game"}, {encoding, utf8}]).

insert() ->
    emysql:execute(my_pool, <<"INSERT INTO role SET role_id = 1001, name = 'dudu'">>),
    emysql:execute(my_pool, <<"INSERT INTO role SET role_id = 2002, name = 'dada'">>).

select() ->
    Result = emysql:execute(my_pool, <<"SELECT * FROM role">>),
    io:format("select result=~p~n", [Result]),
    Recs = emysql_util:as_record(
		Result, hello_record, record_info(fields, hello_record)),
    io:format("undo record=~p~n", [Recs]),


    ets:new(ets_role, [set, protected, named_table, {keypos, #hello_record.role_id}]),
    [ets:insert(ets_role, Record) || Record <- Recs],

    Role2002 = ets:lookup(ets_role, 2002),

    Role1001 = ets:lookup(ets_role, 1001),
    CurRec = lists:last(Role1001),

    io:format("begin insert record=~p~n", [CurRec]),
    NewRec = CurRec#hello_record{level = 23, diamond = 233},
    ets:delete(ets_role, NewRec#hello_record.role_id),
    ets:insert(ets_role, NewRec),
    LevelUpRole1001 = ets:lookup(ets_role, 1001),
    io:format("after insert record=~p~n", [lists:last(LevelUpRole1001)]),

    [H|_T] = Role2002,
    Query = io_lib:format("update role set account_id=~B, name=\"~s\", sex=~B, level=~B, diamond=~B WHERE role_id=~B", 
        [H#hello_record.account_id, H#hello_record.name, H#hello_record.sex, H#hello_record.level, H#hello_record.diamond+100, H#hello_record.role_id]),
    io:format("Statement = ~p~n", [Query]),
    emysql:execute(my_pool, Query).
