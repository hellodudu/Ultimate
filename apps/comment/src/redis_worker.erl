-module(redis_worker).
-include("role.hrl").
-include("session.hrl").
-include("error_code.hrl").
-include("db.hrl").
-include("role_pb.hrl").
-include("account_pb.hrl").

-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([get_script/2, save_script/3]).
-export([get_key/2]).

-record(state, {conn}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(_Args) -> 
    process_flag(trap_exit, true),
    %Hostname = proplists:get_value(hostname, Args),
    %Database = proplists:get_value(database, Args),
    %Username = proplists:get_value(username, Args),
    %Password = proplists:get_value(password, Args),

    {ok, Redis} = eredis:start_link(),
    {ok, #state{conn = Redis}}.

handle_call({q, Command}, _From, State) ->
    Response = eredis:q(State#state.conn, Command),
    {reply, Response, State};

handle_call({qp, Pipeline}, _From, State) ->
    Response = eredis:qp(State#state.conn, Pipeline),
    {reply, Response, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Info, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    eredis:stop(State#state.conn),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

get_script(Api, Method) ->
    gen_server:call(?MODULE, {get_script, Api, Method}).

save_script(Api, Method, Script) ->
    gen_server:call(?MODULE, {save_script, Api, Method, Script}).

get_key(Api, Method) ->
    generate_key([Api, Method]).

generate_key(KeysList) ->
    lists:foldl(fun(Key, Acc) -> Acc ++ ":" ++ Key end, "example", KeysList).
