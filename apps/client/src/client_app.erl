-module(client_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    {ok, [[PortString]]} = init:get_argument(port),
    {ok, [[Host]]} = init:get_argument(host),
    Port = erlang:list_to_integer(PortString),
    client:start(Host, Port).

stop(State) ->
    lager:info( "client stopped at node ~p", [erlang:node()]),
    State.


