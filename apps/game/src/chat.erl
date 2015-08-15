-module(chat).

-include("chat_pb.hrl").
-include("role.hrl").
-include("session.hrl").
-include("error_code.hrl").

-export([handle/2]).


handle({CsChatDeliverTo, _RoleRecord}, SenderSession) when erlang:is_record(CsChatDeliverTo, cs_chat_deliver_to) ->
    #cs_chat_deliver_to{recipient_account_id=ReceipentAccountId, message=Message} = CsChatDeliverTo,
    ValidatedMessage = validate_message(Message),

    %% 筛选出在线角色
    RecList = role_mgr:get_role_record_by_account(ReceipentAccountId),
    ToSessionList = [Rec#role.session || Rec <- RecList],
    [connector:send_to_role(Session, #sc_chat_one_message{sender_account_id=ReceipentAccountId, message=ValidatedMessage}) || Session <- ToSessionList],

    %% 返回结果给发送者
    connector:send_to_role(SenderSession, #sc_chat_deliver_to{result=?SUCCESS});
handle(_, _) ->
    lager:info("chat handle other").
  
validate_message(Message) -> 
    Message.


