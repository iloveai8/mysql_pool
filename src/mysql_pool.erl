-module(mysql_pool).
-export([start/0, stop/0]).
-export([
    checkin/2, 
    checkout/1,
    execute/3, 
    execute/4,
    query/2, 
    query/3, 
    query/4,
    transaction/2, 
    transaction/3, 
    transaction/4,
    with/2
]).

start()->
	case application:start(mysql_pool) of
		ok -> ok;
		{error, {already_started, mysql_pool}} -> ok
	end.    

stop() ->
    application:stop(mysql_pool),
    erlang:halt(). 

%% @doc Returns a mysql connection to the given pool.
checkin(PoolName, Connection) ->
    poolboy:checkin(PoolName, Connection).

%% @doc Checks out a mysql connection from a given pool.
checkout(PoolName) ->
    poolboy:checkout(PoolName).

%% @doc Execute a mysql prepared statement with given params.
execute(PoolName, StatementRef, Params) ->
    poolboy:transaction(PoolName, fun(MysqlConn) ->
        mysql:execute(MysqlConn, StatementRef, Params)
    end).

%% @doc Execute a mysql prepared statement with given params and timeout
execute(PoolName, StatementRef, Params, Timeout) ->
    poolboy:transaction(PoolName, fun(MysqlConn) ->
        mysql:execute(MysqlConn, StatementRef, Params, Timeout)
    end).

%% @doc Executes a query to a mysql connection in a given pool.
query(PoolName, Query) ->
    poolboy:transaction(PoolName, fun(MysqlConn) ->
        mysql:query(MysqlConn, Query)
    end).

%% @doc Executes a query to a mysql connection in a given pool with either
%% list of query parameters or a timeout value.
query(PoolName, Query, ParamsOrTimeout) ->
    poolboy:transaction(PoolName, fun(MysqlConn) ->
        mysql:query(MysqlConn, Query, ParamsOrTimeout)
    end).

%% @doc Executes a query to a mysql connection in a given pool with both
%% a list of query parameters and a timeout value.
query(PoolName, Query, Params, Timeout) ->
    poolboy:transaction(PoolName, fun(MysqlConn) ->
        mysql:query(MysqlConn, Query, Params, Timeout)
    end).

%% @doc Wrapper to poolboy:transaction/2. Since it is not a mysql transaction.
%% Example instead of:
%% Conn = mysql_poolboy:checkout(mypool),
%% try
%%     mysql:query(Conn, "SELECT...")
%%  after
%%     mysql_poolboy:checkin(mypool, Conn)
%%  end.
%%
%% mysql_poolboy:with(mypool, fun (Conn) -> mysql:query(Conn, "SELECT...") end).
with(PoolName, Fun) when is_function(Fun, 1) ->
    poolboy:transaction(PoolName, Fun).

%% @doc Executes a mysql transaction fun. The fun needs to take one argument
%% which is the mysql connection.
transaction(PoolName, TransactionFun) when is_function(TransactionFun, 1) ->
    poolboy:transaction(PoolName, fun(MysqlConn) ->
        mysql:transaction(MysqlConn, TransactionFun, [MysqlConn], infinity)
    end).

%% @doc Executes a transaction fun. Args list needs be the same length as
%% TransactionFun arity - 1.
transaction(PoolName, TransactionFun, Args)
    when is_function(TransactionFun, length(Args) + 1) ->
    poolboy:transaction(PoolName, fun(MysqlConn) ->
        mysql:transaction(MysqlConn, TransactionFun, [MysqlConn | Args],
                          infinity)
    end).

%% @doc Same as transaction/3 but with the number of retries the mysql
%% transaction should try to execute.
transaction(PoolName, TransactionFun, Args, Retries)
    when is_function(TransactionFun, length(Args) + 1) ->
    poolboy:transaction(PoolName, fun(MysqlConn) ->
        mysql:transaction(MysqlConn, TransactionFun, [MysqlConn | Args],
                          Retries)
    end).    