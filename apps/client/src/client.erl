%% simulate a client to connect to server.
-module(client).

-include("account_pb.hrl").
-include("connector.hrl").
-include("error_code.hrl").
-include("role_pb.hrl").
-include("role.hrl").
-include("session.hrl").
-include("chat_pb.hrl").

-behavoir(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/2]).
-export([start/2, stop/0, do_recv/1]).
-export([start/0]).

% 发送操作消息到服务器
-export([
        login/1,
        create_role/1,
        get_role_list/0,
        get_role/1,
        enter_game/1,
        chat/2
    ]).

start() ->
    start(get(ip), get(port)).

start(Ip, Port) ->
    put(ip, Ip),
    put(port, Port),
    gen_server:start({local, ?MODULE}, ?MODULE, {Ip,Port}, []).

start_link(Ip, Port) ->
    gen_server:start_link({local, ?MODULE},?MODULE,{Ip,Port},[]).

send(MessageRecord) ->
    erlang:send( ?MODULE, {to_server,MessageRecord}).

%% 操作
login(AccountID) ->
    case get(account_id) of
        undefined ->
            Login = #cs_account_login{account_id=AccountID},
            put(account_id, AccountID),
            send(Login);
        _ ->
            lager:info("you have logged in already")
    end.

create_role(Name) ->
    case get(account_id) of
        undefined ->
            lager:info("please login first");
        AccountID ->
            CreateRole = #cs_account_create_role{account_id=AccountID, name=Name, sex=1},
            send(CreateRole)
    end.

get_role_list() ->
    case get(account_id) of
        undefined ->
            lager:info("please login first");
        _ ->
            RoleList = #cs_account_get_role_id_list{},
            send(RoleList)
    end.

get_role(RoleID) ->
    case get(account_id) of
        undefined ->
            lager:info("please login first");
        _ ->
            GetRole = #cs_account_get_role{role_id = RoleID},
            send(GetRole)
    end.

enter_game(RoleID) ->
    case get(account_id) of
        undefined ->
            lager:info("please login first");
        _ ->
            EnterGame = #cs_account_enter_game{role_id = RoleID},
            send(EnterGame)
    end.

chat(ToAccountID, Message) ->
    case get(account_id) of
        undefined ->
            lager:info("please login first");
        _ ->
            Chat = #cs_chat_deliver_to{recipient_account_id = ToAccountID, message = Message},
            send(Chat)
    end.

stop() ->
    erlang:send(?MODULE, stop).

init({Ip, Port}) ->
    SocketOptions = [binary, {packet, 0}, {active, false}, {keepalive, true}],
    case gen_tcp:connect(Ip, Port, SocketOptions) of
        {ok, Socket} ->
            io:format("connect success!~n"),
            %spawn(fun() -> do_recv(Socket) end),
            {ok, Socket};
        Other ->
            io:format("connect failed with reason:~p~n", [Other]),
            {error, Other}
    end.

handle_call(get_socket, _From, Socket) ->
    {reply,Socket,Socket};
handle_call(_,_From,Socket)->
    {reply, ok, Socket}.
handle_cast(_Req,Socket) ->
    {noreply, Socket}.

handle_info({to_server, Record}, Socket) ->
    BinaryData = erlang:term_to_binary(Record),
    gen_tcp:send(Socket, BinaryData),
    erlang:send(erlang:self(), recv),
    {noreply, Socket};
handle_info({respond, {_, Code}}, Socket) ->
    case Code of
        0 ->
            lager:info("execute successful!");
        N ->
            lager:info("execute error, code = ~p", [N])
    end,
    {noreply, Socket};
handle_info({respond, {_, Code, _}}, Socket) ->
    case Code of
        0 ->
            lager:info("execute successful!");
        N ->
            lager:info("execute error, code = ~p", [N])
    end,
    {noreply, Socket};
handle_info({tcp,_Socket,Data}, Socket) ->
    %{Module, Response} = lib_codec:decode(Data),
    RecordData = erlang:binary_to_list(Data),
    lager:info("Received response: ~p", [RecordData]),
    {noreply, Socket};
handle_info({tcp_closed,_Socket}, _Socket) ->
    lager:debug("Client disconnected"),
    {stop,normal,socket_disconnected};
handle_info({tcp_error, Socket, Reason}, Socket) ->
    lager:debug("connection disconnected with error reason: ~p",[Reason]),
    {noreply,Socket};
handle_info(stop,Socket) ->
    ok = gen_tcp:close(Socket),
    {stop, normal,ok};

% 处理收到的服务器消息
handle_info(recv, Socket) ->
    case gen_tcp:recv(Socket, 0, timer:seconds(5)) of
        {ok, Packet1} -> 
            <<Rest/binary>> = Packet1,
            Record = binary_to_term(Rest),
            lager:info("recv Respond = ~p", [Record]),
            self() ! {respond, Record},
            {noreply, Socket};
        {error, Reason} ->
            lager:error( "Client receiving error with reason:~p", [Reason]),
            {stop,normal,timeout}
    end;
handle_info(_Unhandled, Socket) ->
    lager:error( "Unhandled Info: ~p",[_Unhandled]),
    {noreply,Socket}.

terminate(_Reason,_Socket) ->
    ok.

code_change(_Vsn, Socket, _Extra) ->
    {ok, Socket}.

do_recv(Socket) ->
    lager:info("into do_recv"),
    receive
        {tcp, Socket, Data} ->
            Val = binary_to_term(Data),
            lager:info("recv server respond = ~p", [Val]),
            do_recv(Socket);
        {tcp_closed, Socket} ->
            lager:info("server socket closed!");
        Other ->
            lager:info("other = ~p", [Other])
    end.
