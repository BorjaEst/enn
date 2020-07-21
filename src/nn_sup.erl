%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
-module(nn_sup).
-behaviour(supervisor).

%% API
-export([id/1, start_link/1, start_neuron/2]).
-export_type([id/0]).

%% Supervisor callbacks
-export([init/1]).

-type id() :: {nn_sup, Ref :: reference()}.

-define(SPECS_NEURON(Neuron_id), #{
    id       => Neuron_id,
    start    => {neuron, start_link, [Neuron_id]},
    restart  => transient,
    shutdown => 500,
    modules  => [neuron]
 }).


%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns the supervisor id of from a network id. 
%% @end
%%--------------------------------------------------------------------
-spec id(Network :: netwrok:id()) -> Supervisor :: id().
id(Network) -> {nn_sup, element(2, Network)}.

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
start_neuron(Supervisor, Neuron_id) ->
    supervisor:start_child(Supervisor, ?SPECS_NEURON(Neuron_id)).


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
    true = enn_pool:register_as_nn_supervisor(Network),
    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

