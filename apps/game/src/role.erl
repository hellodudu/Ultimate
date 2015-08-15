-module(role).
-behaviour(gen_server).

-include("session.hrl").
-include("role.hrl").
-include("chat_pb.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

start_link(RoleRecord) when erlang:is_record(RoleRecord, role)->
    RoleId = RoleRecord#role.role_id,
    gen_server:start_link({global, RoleId}, ?MODULE, RoleRecord , []).

init(RoleRecord) when erlang:is_record(RoleRecord, role) ->
    #role{role_id=RoleId, session=Session} = RoleRecord,
    #session{connector_pid=ConnectorPid} = Session,
    erlang:monitor(process, ConnectorPid),
    NewSession = Session#session{role_id=RoleId},
    NewRoleRecord = RoleRecord#role{session=NewSession},
    {ok, NewRoleRecord}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({Request, Session}, RoleRecord) when erlang:is_record(Request, cs_chat_deliver_to) ->
    try
        case chat:handle({Request, RoleRecord}, Session) of
            {ok,NewRole} -> {noreply, NewRole};
            _ -> {noreply, RoleRecord}
        end
    catch
        Error:Reason ->
            lager:info("role can not handle message<~p>, error<~p>, reason<~p>", [Request, Error, Reason]),
            {noreply, RoleRecord}
    end;

handle_info(reconnected, RoleRecord) ->
    TimerRef = RoleRecord#role.reconnectionTimer,
    erlang:cancel_timer(TimerRef),
    {noreply, RoleRecord#role{reconnectionTimer=undefined}};

handle_info(connector_down, RoleRecord) ->
    prepare_to_stop_role(RoleRecord),
    {stop, normal, RoleRecord};

handle_info(connection_lost, RoleRecord) ->
    prepare_to_stop_role(RoleRecord),
    {stop, normal, RoleRecord};

handle_info({'DOWN',_MonitorRef,_Type, _Object, _Info},RoleRecord) ->
    TimerRef = erlang:send_after(timer:seconds(30), erlang:self(), connector_down),
    {noreply,RoleRecord#role{reconnectionTimer=TimerRef}};

handle_info(stop, RoleRecord) ->
    {stop, normal, RoleRecord};

handle_info(_Info, RoleRecord) ->
    {noreply, RoleRecord}.

terminate(normal,RoleRecord) ->
    %% recycle session resource here.
    #role{role_id=RoleId} = RoleRecord,
    case RoleId of
        undefined -> pass;
        _Else -> 
            supervisor:delete_child(role_sup, RoleId)
    end,
    ok;
terminate(_Reason,_Role) ->
    ok.

code_change(_OldVsn,State,_Extra) ->
    {ok, State}.


prepare_to_stop_role(_Role) -> ok.
