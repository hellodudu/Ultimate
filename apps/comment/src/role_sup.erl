-module(role_sup).
-behaviour(supervisor).
-export([init/1]).
-export([start_link/0]).

start_link() ->
    case supervisor:start_link({local, ?MODULE}, ?MODULE, []) of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

init([]) ->
    {ok,{{one_for_one,10,10}, []}}.
