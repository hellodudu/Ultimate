-module(game_sup).
-behaviour(supervisor).
-export([init/1]).

-export([start_link/0]).
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, Pools} = application:get_env(game, pools),
    ChildSpecs1 = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
                                PoolArgs = [{name, {local, Name}},
                                              {worker_module, redis_worker}
                                             ] ++ SizeArgs,
                                poolboy:child_spec(Name, PoolArgs, WorkerArgs)
                          end, Pools),
    RoleMgr = {role_mgr, {role_mgr, init, []}, permanent, 2000, worker, [role_mgr]},
    RoleSupervisor = {role_sup, {role_sup, start_link, []}, permanent, 2000, supervisor, [role_sup]},
    ChildSpecs2 = [RoleMgr | ChildSpecs1],
    ChildSpecs = [RoleSupervisor | ChildSpecs2],
    {ok, {{one_for_one, 10, 10}, ChildSpecs}}.
