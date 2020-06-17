%%%-------------------------------------------------------------------
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

-define(SPECS_CORTEX(Network), #{
    id       => cortex:id(Network),
    start    => {cortex, start_link, [Network]},
    restart  => permanent,
    shutdown => 1000,
    modules  => [gen_statem]
}).
-define(SPECS_NEURON(Neuron_id), #{
    id       => Neuron_id,
    start    => {neuron, start_link, [Neuron_id]},
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
-spec id(Network :: netwrok:id()) -> Supervisor :: id().
id(Network) -> {element(1, Network), nn_sup}.

%%--------------------------------------------------------------------
%% @doc Starts the supervisor
%% @end
%%--------------------------------------------------------------------
% TODO: To make description and specs
start_link(Network) ->
    supervisor:start_link(?MODULE, [Network]).

%%--------------------------------------------------------------------
%% @doc Starts the neural network cortex
%% @end
%%--------------------------------------------------------------------
% TODO: To make description and specs
start_cortex(Supervisor, Network) ->
    ?START_CHILD(Supervisor, ?SPECS_CORTEX(Network)).

%%--------------------------------------------------------------------
%% @doc Starts the neural network cortex
%% @end
%%--------------------------------------------------------------------
% TODO: To make description and specs
start_neuron(Supervisor, Neuron_id) ->
    ?START_CHILD(Supervisor, ?SPECS_NEURON(Neuron_id)).


%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([Network]) ->
    SupFlags = #{strategy  => one_for_all, %% All down if one down
                 intensity => 0,   %% Restart is not allowed
                 period    => 10}, %% Any as intensity = 0
    ChildSpecs = [],
    enn_pool:register_as_supervisor(Network),
    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

