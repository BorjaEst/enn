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
-export_type([enn_pool/0, enn/0]).

-type enn_pool() :: ets:tid().
-record(enn, {
    id         :: network:id(), % Network identifier
    supervisor :: pid(),        % Pid of the supervisor
    cortex     :: pid(),        % Pid of the cortex 
    nn_pool    :: ets:tid()     % ETS table with neurons pid<->id
    % graph      :: graph(),      % Mounted graph with network
    % neurons     :: integer(),
    % links       :: integer(),
    % forward_cycles  = 0 :: integer(), % Number of FP performed cycles
    % backward_cycles = 0 :: integer(), % Number of BP performed cycles
    % last_bperr = []  :: [float()]  % Last back propagation errors
}).
-type enn() :: #enn{}.

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
-spec register(Network_Id) -> boolean() when 
    Network_Id :: network:id().
register(Id) -> 
    ets:insert(?ENN_POOL, #enn{id = Id}).

%%--------------------------------------------------------------------
%% @doc Deletes a enn from the enn pool.
%% @end
%%--------------------------------------------------------------------
-spec unregister(Network_Id) -> true when 
    Network_Id :: network:id().
unregister(Id) -> 
    ets:delete(?ENN_POOL, Id).

%%--------------------------------------------------------------------
%% @doc Returns true if registered as suppervisor, otherwise false.
%% @end
%%--------------------------------------------------------------------
-spec register_as_supervisor(Network_Id) -> boolean() when 
    Network_Id :: network:id().
register_as_supervisor(Id) -> 
    ets:update_element(?ENN_POOL, Id, {#enn.supervisor, self()}).

%%--------------------------------------------------------------------
%% @doc Returns true if registered as cortex, otherwise false.
%% @end
%%--------------------------------------------------------------------
-spec register_as_cortex(Network_Id) -> boolean() when 
    Network_Id :: network:id().
register_as_cortex(Id) -> 
    ets:update_element(?ENN_POOL, Id, {#enn.cortex, self()}).

%%--------------------------------------------------------------------
%% @doc Returns true if NN_Pool registered, otherwise false.
%% @end
%%--------------------------------------------------------------------
-spec register_nn_pool(Network_Id, NN_Pool) -> boolean() when 
    Network_Id :: network:id(),
    NN_Pool    :: nn_pool:tid().
register_nn_pool(Id, NN_Pool) -> 
    ets:update_element(?ENN_POOL, Id, {#enn.nn_pool, NN_Pool}).

%%--------------------------------------------------------------------
%% @doc Returns the neural network information.
%% @end
%%--------------------------------------------------------------------
-spec info(Network_Id)  -> InfoMap when
      Network_Id :: network:id(),
      InfoMap    :: #{'supervisor' => pid(),
                      'cortex'     => pid(),
                      'nn_pool'    => nn_pool:tid()}.
info(Id) -> 
    case ets:lookup(?ENN_POOL, Id) of 
        [] -> error(badarg);
        NN -> #{supervisor => NN#enn.supervisor,
                cortex     => NN#enn.cortex,
                nn_pool    => NN#enn.nn_pool}
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
