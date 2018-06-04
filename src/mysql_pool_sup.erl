-module(mysql_pool_sup).
-behaviour(supervisor).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

-export([start_link/0, add_pool/3, child_spec/3]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Pools = cfg:get(mysql, []),
    ChildSpecs = lists:map(fun({PoolName, {PoolArgs, MysqlArgs}}) ->
        child_spec(PoolName, PoolArgs, MysqlArgs)
    end, Pools),
	{ok, {{one_for_one, 10, 10}, ChildSpecs}}.

%% @doc Adds a pool to the started mysql_poolboy application.
add_pool(PoolName, PoolArgs, MysqlArgs) ->
    %% We want strategy fifo as default instead of lifo.
    PoolSpec = child_spec(PoolName, PoolArgs, MysqlArgs),
    supervisor:start_child(?MODULE, PoolSpec).

%% @doc Creates a supvervisor:child_spec. When the need to
%% supervise the pools in another way.
child_spec(PoolName, PoolArgs, MysqlArgs) ->
    PoolArgs1 = case proplists:is_defined(strategy, PoolArgs) of
        true  ->
            [{name, {local, PoolName}}, {worker_module, mysql} | PoolArgs];
        false ->
            %% Use fifo by default. MySQL closes unused connections after a certain time.
            %% Fifo causes all connections to be regularily used which prevents them from
            %% being closed.,
            [{strategy, fifo}, {name, {local, PoolName}}, {worker_module, mysql} | PoolArgs]
    end,
    poolboy:child_spec(PoolName, PoolArgs1, MysqlArgs).    