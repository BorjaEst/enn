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
    id  :: cortex:id(), % Neural network identtification
    sup :: pid(),       % Pid of the supervisor
    cx  :: pid()        % Pid of the cortex 
}).

