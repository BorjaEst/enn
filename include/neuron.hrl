%%%-------------------------------------------------------------------
%% @doc Neurons configuration.
%% @end
%%%------------------------------------------------------------------

-type neuron_id() :: {reference(), neuron}.
-record(neuron, {
    id = {make_ref(), neuron} :: neuron_id(),
    activation           :: activation:func(),
    aggregation          :: aggregation:func(),
    initializer          :: initializer:func(),
    bias = uninitialized :: float() | uninitialized
}).  
-type neuron() :: #neuron{}.

