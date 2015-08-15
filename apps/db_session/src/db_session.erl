-module(db_session).
-include("account_pb.hrl").
-include("role.hrl").
-include("role_pb.hrl").
-include("session.hrl").
-include("db.hrl").

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/0, stop/0]).
-export([get_max_rolenum/0]).

init([]) -> {ok, #session{} }.

start() ->
    %% 加载数据库到ets表中
    Role_Result = emysql:execute(?DBPOOL, <<"select * from role;">>),
    Role_Recs = emysql_util:as_record(Role_Result, role, record_info(fields, role)),

    %% 开启并发读写优化
    ets:new(ets_role, [set, public, named_table, {write_concurrency, true}, {read_concurrency, true}, {keypos, #role.role_id}]),
    [ets:insert(ets_role, RoleRec) || RoleRec <- Role_Recs],

    %% 玩家总数
    Role_Nums = length(Role_Recs),
    put(role_num, Role_Nums),
    lager:info("role<~p> load complete!~n", [Role_Nums]),

    %% 创建节点连接进程
    lager:info("try connecting connector"),
    spawn(fun() -> update_connection() end),

    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

get_max_rolenum() ->
    case get(role_num) of
        undefined -> 
            0;
        Num ->
            Num
    end.

handle_cast(stop, State) ->
    {stop, normal, State};


handle_cast(_Request, State) ->
    {noreply, State}.

handle_call(_Request, _From, Session) ->
    Reply = ok,
    {reply, Reply, Session}.

handle_info(timeout, State) ->
    {noreply, State};

%% 创建新角色
handle_info({create_role, StoreRoleRec}, State) ->
    %% 执行sql语句
    Query = io_lib:format("insert into role set role_id=~B, account_id=~B, name=\"~s\", sex=~B, level=~B, diamond=~B ", 
        [StoreRoleRec#role.role_id, StoreRoleRec#role.account_id, StoreRoleRec#role.name, StoreRoleRec#role.sex, StoreRoleRec#role.level, StoreRoleRec#role.diamond]),
    lager:info("query<~s> execute successful", [Query]),
    emysql:execute(?DBPOOL, Query),
    {noreply, State};

%% 保存角色 todo
handle_info({save_role, RoleId}, State) ->
    case ets:lookup(ets_role, RoleId) of
        [] ->
            lager:info("role<~p> doesn't exist!", [RoleId]),
            ignore;
        List when length(List) == 1 ->
            PreRec = lists:last(List),
            NewRec = PreRec#role{diamond = 233},
            ets:update_element(ets_role, 1, {#role.diamond, NewRec#role.diamond}),

            %% 执行sql语句
            Query = io_lib:format("update role set account_id=~B, name=\"~s\", sex=~B, level=~B, diamond=~B where role_id=~B", 
                [NewRec#role.account_id, NewRec#role.name, NewRec#role.sex, NewRec#role.level, NewRec#role.diamond, NewRec#role.role_id]),
            lager:info("query = ~p", [Query]),
            emysql:execute(?DBPOOL, Query)
    end,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, Session) ->
    #session{account_id=AccountId} = Session,
    case AccountId of
        undefined -> pass;
        AccountId when erlang:is_integer(AccountId) ->
            global:unregister_name(AccountId)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% 探测节点
update_connection() ->
    receive
    after 3000 ->
        Nodes = nodes(),
        case lists:member('game@127.0.0.1', Nodes) of
            false ->
                case net_adm:ping('game@127.0.0.1') of
                    pong ->
                        [lager:info("connect node<~p> success", [Node]) || Node <- nodes()],
                        [send_info(Node) || Node <- nodes()];
                    _ ->
                        ignore
                end;
            true ->
                ignore
        end
    end,
    update_connection().

%% 发送初始化数据
send_info(Node) ->
    case Node of
        Connector when Connector == 'game@127.0.0.1' ->
            {game_connector, 'game@127.0.0.1'} ! {init_role, ets:tab2list(ets_role)};
        _Other ->
            pass
    end.

