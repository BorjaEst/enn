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

-define(PI, 3.141592653589793).
-define(DELTA_MULTIPLIER, ?PI * 2).

-export_type([cortex_id/0, neuron_id/0]).



%%% The cortex is a NN synchronizing element. It needs to know the PId of every neural network element, so that it will
%%% know when all the outputs have received their control inputs, and that it’s time for the inputs to again gather
%%% and fanout input data to the neurons in the input layer. At the same time, the Cortex element can also act as a
%%% supervisor of all the Neuron, Input, and Output elements in the NN system.
-type cortex_id() :: {Agent_Reference :: reference(), cortex}.
-record(cortex, {
	id :: cortex_id(),
	layers = #{} :: #{Layer :: float() => [neuron_id()]},
	outputs_ids = [] :: [neuron_id()], % Output neurons, first network layer usually
	inputs_idps = [] :: [{neuron_id() | cortex_id(), Weights :: float()}] % Input neurons, last network layer usually
}).

%%% The neuron is a signal processing element. It accepts signals, accumulates them into an ordered vector, then
%%% processes this input vector to produce an output, and finally passes the output to other elements it is connected
%%% to. The Neuron never interacts with the environment directly, and even when it does receive signals and produces
%%% output signals, it does not know whether these input signals are coming from inputs or neurons, or whether it is
%%% sending its output signals to other neurons or outputs. All the neuron does is have a list of input PIds from
%%% which it expects to receive signals, a list of output PIds to which the neuron sends its output, a weight list
%%% correlated with the input PIds, and an activation function it applies to the dot product of the input vector and its
%%% weight vector. The neuron waits until it receives all the input signals, processes those signals, and then passes
%%% the output forward.
-type neuron_id() :: {{LayerCoordinate :: float(), Unique_Id :: reference()}, neuron} | [].
-record(neuron, {
	id :: neuron_id(),
	af :: activation_function:activation_function(), % Activation function
	aggrf :: aggregation_function:activation_function(), % Name of the aggregation function
	bias = ?DELTA_MULTIPLIER * (rand:uniform() - 0.5) :: float(),
	inputs_idps = [] :: [{neuron_id() | cortex_id(), Weights :: [float()]}], % Inputs IdPs,
	outputs_ids = [] :: [neuron_id() | cortex_id()], % Outputs Ids,
	rcc_inputs_idps = [] :: [{neuron_id() | cortex_id(), Weights :: [float()]}],  % Recurrent inputs IdPs,
	rcc_outputs_ids = [] :: [neuron_id() | cortex_id()]  % Recurrent outputs Ids,
}).
