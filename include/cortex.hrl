%%%-------------------------------------------------------------------
%% @doc Cortex configuration.
%% @end
%%%------------------------------------------------------------------

-type cortex_id()       :: {reference(), cortex}.
-type serialized_grap() :: term().
-record(cortex, {
    id = {make_ref(), cortex} :: cortex_id(),
    inputs  :: integer(),
    outputs :: integer(),
    serialised_graph          :: serialized_grap()
}).  
-type cortex() :: #cortex{}.

