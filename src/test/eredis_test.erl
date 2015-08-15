-module(eredis_test).
-export([test/0]).

test() ->
    {ok, C} = eredis:start_link(),
    io:format("set foo result = ~p~n", [eredis:q(C, ["SET", "foo", "bar"])]),
    io:format("get foo result = ~p~n", [eredis:q(C, ["GET", "foo"])]).
    %{ok, <<"OK">>} = eredis:q(C, ["SET", "foo", "bar"]),
    %{ok, <<"bar">>} = eredis:q(C, ["GET", "foo"]).
