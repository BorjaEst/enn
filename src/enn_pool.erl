%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(enn_pool).
-compile([export_all, nowarn_export_all]). %% TODO: To delete after buil

-include_lib("network.hrl").

%% API
-export([]).
-export_type([]).

-record(enn, {
    id         :: network_id(), % Network identifier
    supervisor :: pid(),        % Pid of the supervisor
    cortex     :: pid(),        % Pid of the cortex 
    pid_pool   :: ets:tid()     % ETS table with neurons pid->id
    % graph      :: graph(),      % Mounted graph with network
    % neurons     :: integer(),
    % links       :: integer(),
    % forward_cycles  = 0 :: integer(), % Number of FP performed cycles
    % backward_cycles = 0 :: integer(), % Number of BP performed cycles
    % last_bperr = []  :: [float()]  % Last back propagation errors
}).

-define(ENN_POOL, enn_pool).
-define(NNTAB_CONFIGUTATION, [
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
    ?ENN_POOL = ets:new(?ENN_POOL, ?NNTAB_CONFIGUTATION),
    ok.


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
