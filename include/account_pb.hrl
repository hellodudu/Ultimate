-ifndef(CS_ROLE_GET_INFO_PB_H).
-define(CS_ROLE_GET_INFO_PB_H, true).
-record(cs_role_get_info, {
    account_id = erlang:error({required, account_id})
}).
-endif.

-ifndef(ROLE_INFO_PB_H).
-define(ROLE_INFO_PB_H, true).
-record(role_info, {
    role_id,
    name,
    sex,
    level
}).
-endif.

-ifndef(SC_ROLE_GET_INFO_PB_H).
-define(SC_ROLE_GET_INFO_PB_H, true).
-record(sc_role_get_info, {
    result = erlang:error({required, result}),
    role
}).
-endif.

-ifndef(CS_ROLE_CREATE_ROLE_PB_H).
-define(CS_ROLE_CREATE_ROLE_PB_H, true).
-record(cs_role_create_role, {
    name = erlang:error({required, name}),
    sex = erlang:error({required, sex})
}).
-endif.

-ifndef(SC_ROLE_CREATE_ROLE_PB_H).
-define(SC_ROLE_CREATE_ROLE_PB_H, true).
-record(sc_role_create_role, {
    result = erlang:error({required, result})
}).
-endif.

-ifndef(CS_ACCOUNT_LOGIN_PB_H).
-define(CS_ACCOUNT_LOGIN_PB_H, true).
-record(cs_account_login, {
    account_id = erlang:error({required, account_id})
}).
-endif.

-ifndef(SC_ACCOUNT_LOGIN_PB_H).
-define(SC_ACCOUNT_LOGIN_PB_H, true).
-record(sc_account_login, {
    result = erlang:error({required, result})
}).
-endif.

-ifndef(CS_ACCOUNT_CREATE_ROLE_PB_H).
-define(CS_ACCOUNT_CREATE_ROLE_PB_H, true).
-record(cs_account_create_role, {
    account_id = erlang:error({required, account_id}),
    name = erlang:error({required, name}),
    sex = erlang:error({required, sex})
}).
-endif.

-ifndef(SC_ACCOUNT_CREATE_ROLE_PB_H).
-define(SC_ACCOUNT_CREATE_ROLE_PB_H, true).
-record(sc_account_create_role, {
    result = erlang:error({required, result}),
    role_id_list = []
}).
-endif.

-ifndef(CS_ACCOUNT_GET_ROLE_ID_LIST_PB_H).
-define(CS_ACCOUNT_GET_ROLE_ID_LIST_PB_H, true).
-record(cs_account_get_role_id_list, {
    
}).
-endif.

-ifndef(SC_ACCOUNT_GET_ROLE_ID_LIST_PB_H).
-define(SC_ACCOUNT_GET_ROLE_ID_LIST_PB_H, true).
-record(sc_account_get_role_id_list, {
    result = erlang:error({required, result}),
    role_id_list = []
}).
-endif.

-ifndef(CS_ACCOUNT_GET_ROLE_PB_H).
-define(CS_ACCOUNT_GET_ROLE_PB_H, true).
-record(cs_account_get_role, {
    role_id = erlang:error({required, role_id})
}).
-endif.

-ifndef(SC_ACCOUNT_GET_ROLE_PB_H).
-define(SC_ACCOUNT_GET_ROLE_PB_H, true).
-record(sc_account_get_role, {
    result = erlang:error({required, result}),
    role
}).
-endif.

-ifndef(CS_ACCOUNT_ENTER_GAME_PB_H).
-define(CS_ACCOUNT_ENTER_GAME_PB_H, true).
-record(cs_account_enter_game, {
    role_id = erlang:error({required, role_id})
}).
-endif.

-ifndef(SC_ACCOUNT_ENTER_GAME_PB_H).
-define(SC_ACCOUNT_ENTER_GAME_PB_H, true).
-record(sc_account_enter_game, {
    result = erlang:error({required, result})
}).
-endif.

