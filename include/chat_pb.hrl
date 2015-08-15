-ifndef(CS_CHAT_DELIVER_TO_PB_H).
-define(CS_CHAT_DELIVER_TO_PB_H, true).
-record(cs_chat_deliver_to, {
    recipient_account_id = erlang:error({required, recipient_account_id}),
    message
}).
-endif.

-ifndef(SC_CHAT_DELIVER_TO_PB_H).
-define(SC_CHAT_DELIVER_TO_PB_H, true).
-record(sc_chat_deliver_to, {
    result = erlang:error({required, result})
}).
-endif.

-ifndef(SC_CHAT_ONE_MESSAGE_PB_H).
-define(SC_CHAT_ONE_MESSAGE_PB_H, true).
-record(sc_chat_one_message, {
    sender_account_id = erlang:error({required, sender_account_id}),
    message
}).
-endif.

