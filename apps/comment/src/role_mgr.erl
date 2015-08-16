-module(role_mgr).
-include("role.hrl").
-include("session.hrl").
-include("error_code.hrl").
-include("db.hrl").
-include("role_pb.hrl").
-include("account_pb.hrl").

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_role/1, stop_role/1]).
-export([restart_role/1, start_role/2]).
-export([stop/0]).
-export([handle/1]).
-export([init/0]).
-export([get_role_record_by_account/1]).


init([]) -> {ok, self()}.

init() ->
    ets:new(ets_online_role_list, [set, public, named_table, {write_concurrency, true}, {read_concurrency, true}]),
    {ok, Redis} = eredis:start_link(),
    put(redis, Redis),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Info, State) ->
    {noreply, State}.

%% 初始化数据库玩家列表
handle_info({init_db_role, RoleList}, State) ->
    ets:new(ets_role, [set, public, named_table, {write_concurrency, true}, {read_concurrency, true}, {keypos, #role.role_id}]),
    [ets:insert(ets_role, Role) || Role <- RoleList],
    {noreply, State};

%% 写redis
handle_info({save_role, RoleRec}, State) ->
    KeyStr = io_lib:format("role:~B:name", [RoleRec#role.role_id]),
    Redis = get(redis),
    eredis:q(Redis, ["SET", KeyStr, "redis"]),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% 玩家进入游戏世界 开启role进程
start_role(RoleRecord) when erlang:is_record(RoleRecord, role) ->
    #role{role_id=RoleId} = RoleRecord,
    case global:whereis_name(RoleId) of
        Pid when erlang:is_pid(Pid) -> 
            erlang:send(Pid, reconnected),
            ok;
        undefined ->
            start_role(new, RoleRecord)
    end.

start_role(new, RoleRecord) when erlang:is_record(RoleRecord, role)->
    RoleId = RoleRecord#role.role_id,
    RoleChildSpec = {RoleId,
                     {role, start_link, [RoleRecord]},
                     transient,
                     2000,
                     worker,
                     [role]
                    },
    StartChildResult = supervisor:start_child(role_sup, RoleChildSpec),
    case StartChildResult of
        {ok, ChildPid} when erlang:is_pid(ChildPid) -> 
            ok;
        {ok, ChildPid,_ChildInfo} when erlang:is_pid(ChildPid) -> 
            ok;
        {error, {already_started, ChildPid}} when erlang:is_pid(ChildPid) -> 
            erlang:send(ChildPid, reconnected),
            ok;
        {error, already_present} -> 
            supervisor:delete_child(role_sup, RoleId),
            start_role(RoleRecord)
    end.

%% 结束玩家进程
stop_role(RoleRecord) when erlang:is_record(RoleRecord, role) ->
    #role{role_id=RoleId} = RoleRecord,
    stop_role(RoleId);
stop_role(RoleId) ->
    supervisor:terminate_child(role_sup, RoleId),
    supervisor:delete_child(role_sup, RoleId).

restart_role(RoleId) ->
    supervisor:restart_child(role_sup, RoleId).

%% 未登录玩家处理
handle({CsAccountLogin, Session}) when erlang:is_record(CsAccountLogin, cs_account_login) ->
    #session{session_state = ?CONNECTED} = Session,
    #cs_account_login{account_id = AccountId} = CsAccountLogin, %% in production environment, we shall validate or 
                                                              %% fetch account profile from 
                                                              %% another account server using a string token.
                                                              %% for simplicity, just assume that the account id is ok here.
    NewSession = Session#session{account_id=AccountId,
                                 session_state=?LOGGED_IN},
    connector:send_to_role(Session, #sc_account_login{result=?SUCCESS}),
    {ok, NewSession};

handle({CsAccountCreateRole, Session}) when erlang:is_record(CsAccountCreateRole, cs_account_create_role) ->
    #session{account_id = AccountId, session_state=?LOGGED_IN} = Session,
    OnlineRoleList = ets:tab2list(ets_online_role_list),

    ScAccountCreateRole =
        case (erlang:length(OnlineRoleList) < ?ROLE_MAX_NUM_CREATED) of
            true ->
                #cs_account_create_role{account_id = AccountId, 
                                        name = RoleName,
                                        sex = RoleSex} = CsAccountCreateRole,
                RoleRecord = #role{role_id=undefined,
                                   account_id=AccountId,
                                   name=RoleName,
                                   sex=RoleSex,
                                   level=1,
                                   diamond=20},

                %% id简单自增1
                RoleId = length(ets:tab2list(ets_role)),
                case ets:member(ets_role, RoleId) of
                    false ->
                        %% 更新ets数据
                        StoreRoleRec = RoleRecord#role{role_id = RoleId},
                        ets:insert(ets_role, StoreRoleRec),

                        %% 写redis
                        redis_interface:save_role(RoleId, StoreRoleRec),
                        redis_interface:hmget(RoleId),

                        %% 发送到mysql
                        {db_session, 'db_session@127.0.0.1'} ! {create_role, StoreRoleRec},

                        %% 更新在线玩家列表
                        NewOnlineRoleList = [RoleId|OnlineRoleList],
                        put(online_role_list, NewOnlineRoleList),
                        #sc_account_create_role{result=?SUCCESS, role_id_list=NewOnlineRoleList};
                    true ->
                        lager:info("role_id<~p> exist = ~p", [RoleId]),
                        #sc_account_create_role{result=?ROLE_EXISTED}
                end;
            false ->
                lager:info("length limit online role list size = ~p", [erlang:length(OnlineRoleList)]),
                #sc_account_create_role{result=?ROLE_MAX_NUM_CREATED}
        end,
    connector:send_to_role(Session, ScAccountCreateRole),
    {ok, Session};

handle({CsAccountGetRoleIdList, Session}) when erlang:is_record(CsAccountGetRoleIdList,cs_account_get_role_id_list) ->
    #session{session_state=?LOGGED_IN, account_id=AccountID} = Session,
    RoleList = ets:match_object(ets_role, #role{account_id = AccountID, _='_'}),
    ScAccountGetRoleIdList = #sc_account_get_role_id_list{result = ?SUCCESS, role_id_list = RoleList},
    connector:send_to_role(Session, ScAccountGetRoleIdList),
    ok;

handle({CsAccountGetRole, Session}) when erlang:is_record(CsAccountGetRole, cs_account_get_role) ->
    #cs_account_get_role{role_id=RoleId} = CsAccountGetRole,
    #session{account_id=AccountID, session_state=?LOGGED_IN} = Session,
    RoleIdList = ets:match_object(ets_role, #role{role_id = RoleId, account_id = AccountID, _='_'}),
    ScAccountGetRole = 
        case length(RoleIdList) of
            0 ->
                #sc_account_get_role{result=?ROLE_NOT_YOURS};
            _Length ->
                case get_role_record(RoleId) of
                    RoleRecord when erlang:is_record(RoleRecord, role) ->
                        #role{name=Name,sex=Sex,level=Level} = RoleRecord,
                        RoleInfo = #role_info{role_id=RoleId, name=Name, sex=Sex, level=Level},
                        #sc_account_get_role{result=?SUCCESS, role=RoleInfo};
                    _ ->
                        #sc_account_get_role{result=?ROLE_NOT_EXISTED}
                end
        end,
    connector:send_to_role(Session, ScAccountGetRole),
    ok;

handle({CsAccountEnterGame, Session}) when erlang:is_record(CsAccountEnterGame,cs_account_enter_game) ->
    #cs_account_enter_game{role_id=RoleId} = CsAccountEnterGame,
    #session{account_id=AccountID, session_state=?LOGGED_IN} = Session,
    RoleIdList = ets:match_object(ets_role, #role{role_id = RoleId, account_id = AccountID, _='_'}),
    ScAccountEnterGame = 
        case length(RoleIdList) of
            0 ->
                #sc_account_enter_game{result = ?ROLE_NOT_YOURS};
            _Length ->
                case get_role_record(RoleId) of
                    RoleRecord when erlang:is_record(RoleRecord,role)->
                        SessionRoleRecord = RoleRecord#role{session=Session},
                        case role_mgr:start_role(SessionRoleRecord) of
                            ok -> #sc_account_enter_game{result=?SUCCESS};
                            _ -> #sc_account_enter_game{result=?SERVER_INTERNAL_ERROR}
                        end;
                    _ ->
                        #sc_account_enter_game{result=?ROLE_NOT_EXISTED}
                end
        end,
    Result = ScAccountEnterGame#sc_account_enter_game.result,
    NewSession = 
        case Result of
            ?SUCCESS -> Session#session{role_id=RoleId};
            _ -> Session
        end,

    ets:update_element(ets_role, RoleId, {#role.session, NewSession}),
    connector:send_to_role(Session, ScAccountEnterGame),
    {ok, NewSession}.

get_role_record(RoleId) ->
    case ets:lookup(ets_role, RoleId) of
        [] ->
            lager:info("role doesn't exist!"),
            not_existed;
        List when length(List) == 1 ->
            lists:last(List)
    end.

get_role_record_by_account(AccountID) ->
    ets:match_object(ets_role, #role{role_id = '_', account_id = AccountID, _='_'}).

