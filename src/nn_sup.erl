%%%-------------------------------------------------------------------
%%% @author borja
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Sep 2018 10:38
%%%-------------------------------------------------------------------
-module(nn_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_cortex/2, start_neuron/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SPECS_CORTEX(Cortex_Id), #{
    id       => Cortex_Id,
    start    => {cortex, start_link, [Cortex_Id]},
    restart  => permanent,
    shutdown => 1000,
    modules  => [gen_statem]}).


-define(SPECS_NEURON(Neuron_Id), #{
    id       => Neuron_Id,
    start    => {neuron, start_link, [Neuron_Id]},
    restart  => permanent,
    shutdown => 500,
    modules  => [neuron]}).


%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
% TODO: To make description and specs
start_link() ->
    supervisor:start_link(?MODULE, []).

%%--------------------------------------------------------------------
%% @doc
%% Starts the neural network cortex
%%
%% @end
%%--------------------------------------------------------------------
% TODO: To make description and specs
start_cortex(Supervisor, Cortex_Id) ->
    supervisor:start_child(Supervisor, ?SPECS_CORTEX(Cortex_Id)).

%%--------------------------------------------------------------------
%% @doc
%% Starts the neural network cortex
%%
%% @end
%%--------------------------------------------------------------------
% TODO: To make description and specs
start_neuron(Supervisor, Neuron_Id) ->
    supervisor:start_child(Supervisor, ?SPECS_NEURON(Neuron_Id)).


%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    SupFlags = #{strategy => one_for_all, %% If an element dies, all must shutdown
                 intensity => 0, %% Restart is not allowed
                 period => 10}, %% Any as intensity = 0
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
