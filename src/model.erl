%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(model).
-compile([export_all, nowarn_export_all]). %% TODO: To delete after build

-include_lib("eunit/include/eunit.hrl").

%% API
%%-export([]).
-export_type([connections/0]).

-type nature()      :: sequential | recurrent.
-type probability() :: float().
-type connections() :: #{Name::atom() => {nature(),probability()} 
                                        | nature()}.


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Compiles and stores a model in the DB returning its cortex_id.
%% Should run inside a mnesia transaction.
%% @end
%%--------------------------------------------------------------------
-spec compile(Model, Layers) -> Network when
    Model  :: #{Name::atom() => model:connections()},
    Layers :: #{Name::atom() => [enn:neuron()]},
    Network :: enn:network().
compile(Model, Layers) ->
    Network = todo, % Better to start a new network? from_map?
    nnet:edit(Network, 
        fun(NNet) -> 
            % Not correctly mapped (needs to use the compiled layers as well)
            maps:fold(fun connect/3, NNet, Model)
        end
    ).


%%====================================================================
%% Internal functions
%%====================================================================

% Connects the layers accorss the connections -----------------------
connect(L1, Connections, NNet) -> 
    Fun = fun(L2, Type, Acc) -> connect(L1, L2, Type, Acc) end,
    maps:fold(Fun, NNet, Connections).

connect(L1, L2, {sequential, Probability}, NNet) -> 
    nnet:seq_connect(links(L1,L2,Probability), NNet);
connect(L1, L2, {recurrent, Probability}, NNet) ->  
    nnet:rcc_connect(links(L1,L2,Probability), NNet);
connect(L1, L2, Nature, NNet) -> 
    connect(L1, L2, {Nature, 1.0}, NNet).

% Retruns the list of links between 2 layers ------------------------
links(Layer1, Layer2, 1.0) -> 
    [{Neuron1,Neuron2} || Neuron1 <- maps:values(Layer1), 
                          Neuron2 <- maps:values(Layer2)];
links(Layer1, Layer2, Probability) -> 
    [{Neuron1,Neuron2} || Neuron1 <- maps:values(Layer1), 
                          Neuron2 <- maps:values(Layer2),
                          Probability > rand:uniform()].


%%====================================================================
%% Eunit white box tests
%%====================================================================

% -------------------------------------------------------------------
% TESTS DESCRIPTIONS ------------------------------------------------

% -------------------------------------------------------------------
% SPECIFIC SETUP FUNCTIONS ------------------------------------------

% -------------------------------------------------------------------
% ACTUAL TESTS ------------------------------------------------------

% -------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS -----------------------------------------

