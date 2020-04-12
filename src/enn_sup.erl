%%%-------------------------------------------------------------------
%% @doc enn top level supervisor.
%%
%% TODO: Use a non named_table for the nn_pool
%% @end
%%%-------------------------------------------------------------------
-module(enn_sup).
-behaviour(supervisor).

-include_lib("network.hrl").

%% API
-export([start_link/1, start_nn/1, terminate_nn/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-define(SPECS_DATALOG, #{
    id       => datalog,
    start    => {datalog, start_link, []},
    restart  => permanent,
    shutdown => 500,
    modules  => [gen_server]
}).
-define(NN_SUP_ID(Cortex_Id), {element(1, Cortex_Id), nn_sup}).
-define(SPECS_NN_SUP(Cortex_Id), #{
    id       => ?NN_SUP_ID(Cortex_Id),
    start    => {nn_sup, start_link, [Cortex_Id]},
    restart  => temporary,
    type     => supervisor,
    modules  => [supervisor]
}).

%%====================================================================
%% API functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts the supervisor
%% @end
%%--------------------------------------------------------------------
% TODO: To make description and specs
start_link(StartArgs) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, StartArgs).

%%--------------------------------------------------------------------
%% @doc Starts the neural network supervisor
%% @end
%%--------------------------------------------------------------------
% TODO: To make description and specs
start_nn(Cortex_Id) ->
    true = ets:insert(?NN_POOL, #network{id = Cortex_Id}),
    {ok, NN_Pid} = supervisor:start_child(?SERVER, 
                                          ?SPECS_NN_SUP(Cortex_Id)),
    {ok, _}      = nn_sup:start_cortex(NN_Pid, Cortex_Id),
    ok. 

%%--------------------------------------------------------------------
%% @doc Stops the neural network supervisor
%% @end
%%--------------------------------------------------------------------
% TODO: To make description and specs
terminate_nn(Cortex_Id) ->
    case supervisor:terminate_child(?SERVER, ?NN_SUP_ID(Cortex_Id)) of
        ok   -> true = ets:delete(?NN_POOL, Cortex_Id), ok;
        Fail -> Fail
    end.


%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    SupFlags = #{strategy  => one_for_all,
                 intensity => 10,
                 period    => 36},
    ChildSpecs = [
        ?SPECS_DATALOG
    ],
    start_nn_pool(),
    {ok, {SupFlags, ChildSpecs}}.

    
%%====================================================================
%% Internal functions
%%====================================================================

start_nn_pool() ->  
    ets:new(?NN_POOL, ?NN_POOL_OPTIONS).


