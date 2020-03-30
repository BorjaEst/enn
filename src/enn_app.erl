%%%-------------------------------------------------------------------
%% @doc enn public API
%% @end
%%%-------------------------------------------------------------------
-module(enn_app).
-author("borja").
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, StartArgs) ->
    Tables = enn:attributes_table(),
    edb:create_tables(Tables),
    edb:start(Tables),
    ok = create_project_directories(),
    enn_sup:start_link(StartArgs).

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================



create_project_directories() ->
	file:make_dir("logs").