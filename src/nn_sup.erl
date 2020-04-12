%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
-module(nn_sup).
-behaviour(supervisor).

-include_lib("network.hrl").


%% API
-export([start_link/1, start_cortex/2, start_neuron/2]).

%% Supervisor callbacks
-export([init/1]).

-define(START_CHILD(Supervisor, ChildSpec), 
        supervisor:start_child(Supervisor, ChildSpec)).

-define(SPECS_CORTEX(Cortex_Id), #{
    id       => Cortex_Id,
    start    => {cortex, start_link, [Cortex_Id]},
    restart  => permanent,
    shutdown => 1000,
    modules  => [gen_statem]
}).
-define(SPECS_NEURON(Neuron_Id, NN), #{
    id       => Neuron_Id,
    start    => {neuron, start_link, [Neuron_Id, NN#network.id]},
    restart  => permanent,
    shutdown => 500,
    modules  => [neuron]
 }).

-define(ETS_TABLE_SPECS, [{read_concurrency, true}, public]).


%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the supervisor
%% @end
%%--------------------------------------------------------------------
% TODO: To make description and specs
start_link(Cortex_Id) ->
    supervisor:start_link(?MODULE, [Cortex_Id]).

%%--------------------------------------------------------------------
%% @doc Starts the neural network cortex
%% @end
%%--------------------------------------------------------------------
% TODO: To make description and specs
start_cortex(Supervisor, Cortex_Id) ->
    ?START_CHILD(Supervisor, ?SPECS_CORTEX(Cortex_Id)).

%%--------------------------------------------------------------------
%% @doc Starts the neural network cortex
%% @end
%%--------------------------------------------------------------------
% TODO: To make description and specs
start_neuron(Id, NN) ->
    {ok, Pid} = ?START_CHILD(NN#network.supervisor, ?SPECS_NEURON(Id, NN)),
    true = ets:insert(NN#network.pid_pool, [{Id, Pid}, {Pid, Id}]),
    Pid.


%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([Cortex_Id]) ->
    SupFlags = #{strategy  => one_for_all, %% All down if one down
                 intensity => 0,   %% Restart is not allowed
                 period    => 10}, %% Any as intensity = 0
    ChildSpecs = [],
    register_in_pool(Cortex_Id),
    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

register_in_pool(Cortex_Id) -> 
    % The nn key in the nn_pool is the cortex id 
    true = ets:update_element(?NN_POOL, Cortex_Id, [
        {#network.supervisor, self()},
        {#network.pid_pool,     ets:new(unnamed, ?ETS_TABLE_SPECS)}
    ]).

