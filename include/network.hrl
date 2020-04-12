%%%-------------------------------------------------------------------
%% @doc Neural networks configuration.
%% @end
%%%------------------------------------------------------------------

-type networks_pool() :: ets:tid().
-type  neurons_pool() :: ets:tid().
-type network_graph() :: digraph:graph().
-define(NN_POOL, nn_pool).
-define(NN_POOL_OPTIONS, [
    named_table,        % As any process should be able to start a nn
    public,             % Every element registers itself (so public)
    set,                % The pool must be a set (no repeated values)
    {keypos, #network.id}    % The key of the record must be the id
]).

-record(network, {
    id         :: cortex_id(), % Network identifier
    supervisor :: pid(),       % Pid of the supervisor
    cortex     :: pid(),       % Pid of the cortex 
    graph      :: network_graph(), % Mounted graph with network
    pid_pool   :: neurons_pool()   % ETS table with neurons pid->id 
    % neurons     :: integer(),
    % links       :: integer(),
    % forward_cycles  = 0 :: integer(), % Number of FP performed cycles
    % backward_cycles = 0 :: integer(), % Number of BP performed cycles
    % last_bperr = []  :: [float()]  % Last back propagation errors
}).
-type network() :: #network{}.

