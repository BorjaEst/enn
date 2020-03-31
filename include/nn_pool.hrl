%%%-------------------------------------------------------------------
%% @doc Neural networks pool configuration.
%%
%% TODO: Use a non named_table for the nn_pool
%% @end
%%%------------------------------------------------------------------

-define(NN_POOL, nn_pool).
-define(NN_POOL_OPTIONS, [
    named_table,        % The name of the table must be global for now
    public,             % As any process should be able to start a nn

    set,                % The pool must be a set (no repeated values)
    {keypos, #nn.id}    % The key of the record must be the id
]).
-record(nn, {
    id          :: cortex:id(), % Neural network identtification
    supervisor  :: pid(),       % Pid of the supervisor
    cortex      :: pid(),       % Pid of the cortex 
    idpidT      :: ets:tid(),   % ETS table with neurons id<->pid 
    % forward_cycles  = 0 :: integer(), % Number of FP performed cycles
    % backward_cycles = 0 :: integer(), % Number of BP performed cycles
    dimensions = #{} :: #{Coordinade :: float() => Size :: integer()}
    % last_bperr = []  :: [float()]  % Last back propagation errors
}).

