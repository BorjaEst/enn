%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
-module(nn_sup).
-behaviour(supervisor).

%% API
-export([id/1, start_link/1, start_cortex/2, start_neuron/2]).
-export_type([id/0]).

%% Supervisor callbacks
-export([init/1]).

-type id() :: {Ref :: reference(), nn_sup}.

-define(SPECS_CORTEX(Network_Id), #{
    id       => cortex:id(Network_Id),
    start    => {cortex, start_link, [Network_Id]},
    restart  => permanent,
    shutdown => 1000,
    modules  => [gen_statem]
}).
-define(SPECS_NEURON(Neuron_Id), #{
    id       => Neuron_Id,
    start    => {neuron, start_link, [Neuron_Id]},
    restart  => permanent,
    shutdown => 500,
    modules  => [neuron]
 }).

-define(START_CHILD(Supervisor, ChildSpec), 
        supervisor:start_child(Supervisor, ChildSpec)).


%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns the supervisor id of from a network id. 
%% @end
%%--------------------------------------------------------------------
-spec id(Network_Id :: netwrok:id()) -> Supervisor :: id().
id(Network_Id) -> {element(1, Network_Id), nn_sup}.

%%--------------------------------------------------------------------
%% @doc Starts the supervisor
%% @end
%%--------------------------------------------------------------------
% TODO: To make description and specs
start_link(Network_Id) ->
    supervisor:start_link(?MODULE, [Network_Id]).

%%--------------------------------------------------------------------
%% @doc Starts the neural network cortex
%% @end
%%--------------------------------------------------------------------
% TODO: To make description and specs
start_cortex(Supervisor, Network_Id) ->
    ?START_CHILD(Supervisor, ?SPECS_CORTEX(Network_Id)).

%%--------------------------------------------------------------------
%% @doc Starts the neural network cortex
%% @end
%%--------------------------------------------------------------------
% TODO: To make description and specs
start_neuron(Supervisor, Neuron_Id) ->
    ?START_CHILD(Supervisor, ?SPECS_NEURON(Neuron_Id)).


%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([Network_Id]) ->
    SupFlags = #{strategy  => one_for_all, %% All down if one down
                 intensity => 0,   %% Restart is not allowed
                 period    => 10}, %% Any as intensity = 0
    ChildSpecs = [],
    enn_pool:register_as_supervisor(Network_Id),
    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

