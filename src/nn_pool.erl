%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(nn_pool).
-compile([export_all, nowarn_export_all]). %% TODO: To delete after buil

-include_lib("neuron.hrl").

%% API
-export([]).
-export_type([]).

-type pool() :: ets:tid().
-type id()   :: neuron_id() | cortex_id().

-define(NN_POOL, nn_pool).
-define(TAB_CONFIGUTATION, [
    {read_concurrency, true}, % Prepared for concurrent reading
    protected                 % Any can query the table
]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Mounts the pool using a list of {id, pid}.
%% @end
%%--------------------------------------------------------------------
-spec mount(IdsPids) -> pool() when 
    IdsPids :: [{id(), pid()}].
mount(IdsPids) -> 
    Tid  = ets:new(unnamed, ?TAB_CONFIGUTATION),
    true = ets:insert(Tid, [{Id,Pid} || {Id,Pid} <- IdsPids]),
    true = ets:insert(Tid, [{Pid,Id} || {Id,Pid} <- IdsPids]),
    Tid.

%%--------------------------------------------------------------------
%% @doc Returns the pid of a neuron from its id.
%% @end
%%--------------------------------------------------------------------
-spec pid(Pool :: pool(), Id :: id()) -> Pid :: pid().
pid(Pool, Id) ->
    [{Id, Pid}] = ets:lookup(Pool, Id),
    Pid.

%%--------------------------------------------------------------------
%% @doc Returns the id of a neuron from its pid.
%% @end
%%--------------------------------------------------------------------
-spec id(Pool :: pool(), Pid :: pid()) -> Id :: id().
id(Pool, Pid) ->
    [{Pid, Id}] = ets:lookup(Pool, Pid),
    Id.


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
