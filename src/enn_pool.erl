%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(enn_pool).
-compile([export_all, nowarn_export_all]). %% TODO: To delete after buil

%% API
-export([]).
-export_type([enn_pool/0, enn/0, info/0]).

-type enn_pool() :: ets:tid().
-record(enn, {
    id :: enn:network(), % Network identifier
    cx_sup  :: pid(),    % Pid of the supervisor
    nn_sup  :: pid(),    % Pid of the supervisor
    cortex  :: pid(),    % Pid of the cortex 
    nn_pool :: nn_pool:pool() % Pool with neurons pid<->id
}).
-type enn()  :: #enn{}.
-type info() :: #{'cx_sup'  => pid(),
                  'nn_sup'  => pid(),
                  'cortex'  => pid(),
                  'nn_pool' => nn_pool:tid()}.

-define(ENN_POOL, enn_pool).
-define(TAB_CONFIGUTATION, [
    named_table,        % As any process should be able to start a nn
    public,             % Every element registers itself (so public)
    set,                % The pool must be a set (no repeated values)
    {keypos, #enn.id}   % The key of the record must be the id
]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the neural network pool ets table.
%% @end
%%--------------------------------------------------------------------
-spec start() -> ok.
start() -> 
    ?ENN_POOL = ets:new(?ENN_POOL, ?TAB_CONFIGUTATION),
    ok.

%%--------------------------------------------------------------------
%% @doc Registers a new enn in the enn pool.
%% @end
%%--------------------------------------------------------------------
-spec register(Network) -> boolean() when 
    Network :: enn:network().
register(Id) -> 
    ets:insert(?ENN_POOL, #enn{id = Id}).

%%--------------------------------------------------------------------
%% @doc Deletes a enn from the enn pool.
%% @end
%%--------------------------------------------------------------------
-spec unregister(Network) -> true when 
    Network :: enn:network().
unregister(Id) -> 
    ets:delete(?ENN_POOL, Id).

%%--------------------------------------------------------------------
%% @doc Returns true if registered as suppervisor, otherwise false.
%% @end
%%--------------------------------------------------------------------
-spec register_as_cx_supervisor(Network) -> boolean() when 
    Network :: enn:network().
register_as_cx_supervisor(Id) -> 
    ets:update_element(?ENN_POOL, Id, {#enn.cx_sup, self()}).

%%--------------------------------------------------------------------
%% @doc Returns true if registered as suppervisor, otherwise false.
%% @end
%%--------------------------------------------------------------------
-spec register_as_nn_supervisor(Network) -> boolean() when 
    Network :: enn:network().
register_as_nn_supervisor(Id) -> 
    ets:update_element(?ENN_POOL, Id, {#enn.nn_sup, self()}).

%%--------------------------------------------------------------------
%% @doc Returns true if registered as cortex, otherwise false.
%% @end
%%--------------------------------------------------------------------
-spec register_as_cortex(Network) -> boolean() when 
    Network :: enn:network().
register_as_cortex(Id) -> 
    ets:update_element(?ENN_POOL, Id, {#enn.cortex, self()}).

%%--------------------------------------------------------------------
%% @doc Returns true if NN_Pool registered, otherwise false.
%% @end
%%--------------------------------------------------------------------
-spec register_nn_pool(Network, NN_Pool) -> boolean() when 
    Network :: enn:network(),
    NN_Pool    :: nn_pool:tid().
register_nn_pool(Id, NN_Pool) -> 
    ets:update_element(?ENN_POOL, Id, {#enn.nn_pool, NN_Pool}).

%%--------------------------------------------------------------------
%% @doc Returns the neural network information.
%% @end
%%--------------------------------------------------------------------
-spec info(Network)  -> InfoMap when
      Network :: enn:network(),
      InfoMap    :: info().
info(Id) -> 
    case ets:lookup(?ENN_POOL, Id) of 
        []   -> error(badarg);
        [NN] -> #{cx_sup  => NN#enn.cx_sup,
                  nn_sup  => NN#enn.nn_sup,
                  cortex  => NN#enn.cortex,
                  nn_pool => NN#enn.nn_pool}
    end.


%%====================================================================
%% Internal functions
%%====================================================================


%%====================================================================
%% Eunit white box tests
%%====================================================================

% --------------------------------------------------------------------
% TESTS DESCRIPTIONS -------------------------------------------------

% --------------------------------------------------------------------
% SPECIFIC SETUP FUNCTIONS -------------------------------------------

% --------------------------------------------------------------------
% ACTUAL TESTS -------------------------------------------------------

% --------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS ------------------------------------------
