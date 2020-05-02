%%%-------------------------------------------------------------------
%% @doc enn top level supervisor.
%%
%% TODO: Use a non named_table for the nn_pool
%% @end
%%%-------------------------------------------------------------------
-module(enn_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_nn/1, terminate_nn/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-define(SPECS_NN_SUP(Network_id), #{
    id       => nn_sup:id(Network_id),
    start    => {nn_sup, start_link, [Network_id]},
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
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%--------------------------------------------------------------------
%% @doc Starts the neural network supervisor
%% @end
%%--------------------------------------------------------------------
% TODO: To make description and specs
start_nn(Network_id) ->
    true = enn_pool:register(Network_id),
    Specs = ?SPECS_NN_SUP(Network_id),
    {ok, P} = supervisor:start_child(?SERVER, Specs),
    {ok, _} = nn_sup:start_cortex(P, Network_id),
    ok. 

%%--------------------------------------------------------------------
%% @doc Stops the neural network supervisor
%% @end
%%--------------------------------------------------------------------
% TODO: To make description and specs
terminate_nn(Network_id) ->
    true = enn_pool:unregister(Network_id),
    supervisor:terminate_child(?SERVER, nn_sup:id(Network_id)).


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
    ChildSpecs = [],
    enn_pool:start(),
    {ok, {SupFlags, ChildSpecs}}.

    
%%====================================================================
%% Internal functions
%%====================================================================

