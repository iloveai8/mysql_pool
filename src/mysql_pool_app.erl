-module(mysql_pool_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
	start_deps_apps(),
    mysql_pool_sup:start_link().

stop(_State) ->
    ok.

start_deps_apps()->
   application:start(base).