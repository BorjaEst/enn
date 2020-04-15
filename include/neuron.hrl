%%%-------------------------------------------------------------------
%% @doc Neurons configuration.
%% @end
%%%------------------------------------------------------------------

-type neuron_id() :: {reference(), neuron}.
-type weight()    :: float() | uninitialized.
-record(neuron, {
    id = {make_ref(), neuron} :: neuron_id(),
    activation           ::  activation:func(),
    aggregation          :: aggregation:func(),
    initializer          :: initializer:func(),
    bias = uninitialized :: weight()
}).  
-type neuron() :: #neuron{}.

