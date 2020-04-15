%%%-------------------------------------------------------------------
%% @doc Neural networks definition.
%% @end
%%%------------------------------------------------------------------

-import_lib("neuron.hrl").
-import_lib("cortex.hrl").

-type network_id() :: {reference(), network}.
-type network_type()  :: 'sequential' | 'recurrent'.
-record(network, {
    id = {make_ref(), network} :: network_id(),
    cn = #{} :: #{neuron_id() => connections()},   
    type = recurrent :: network_type()
}).
-type network() :: #network{}.

-record(connections, {
    seq = {[],[]} :: {In :: [neuron_id()], Out :: [neuron_id()]},
    rcc = {[],[]} :: {In :: [neuron_id()], Out :: [neuron_id()]}
}).
-type connections() :: #connections{}.

-type link_id() :: {From :: neuron_id(), To :: neuron_id(), connection}.
-record(link, {
    id :: linkn_id(),
    w = uninitialized :: weight()
}).
-type link() :: #link{}.

