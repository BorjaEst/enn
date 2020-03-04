%%%-------------------------------------------------------------------
%%% @author borja
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Sep 2018 21:59
%%%-------------------------------------------------------------------

-define(NEW_CORTEX_ID(Name), {Name, cortex}).
-define(NEW_NEURON_ID(Layer), {{Layer, nnref:new()}, neuron}).

-export_type([element/0, cortex_id/0]).


%%% Neuronal networks are composed mostly by 2 elements: cortex and neruons
-type element() :: cortex | neuron.

%%% The cortex is a NN synchronizing element. It needs to know the PId of every neural network element, so that it will
%%% know when all the outputs have received their control inputs, and that it’s time for the inputs to again gather
%%% and fanout input data to the neurons in the input layer. At the same time, the Cortex element can also act as a
%%% supervisor of all the Neuron, Input, and Output elements in the NN system.
-type cortex_id() :: {Agent_Reference :: reference(), cortex}.
-record(cortex, {
	id :: cortex_id(),
	layers = #{} :: #{Layer :: float() => [neuron:neuron_id()]},
	outputs_ids = [] :: [nneuro:neuron_id()], % Output neurons, first network layer usually
	inputs_idps = [] :: [{nneuro:neuron_id() | cortex_id(), Weights :: float()}] % Input neurons, last network layer usually
}).
