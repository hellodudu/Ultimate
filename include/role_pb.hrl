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

