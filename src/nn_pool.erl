%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(nn_pool).
-compile([export_all, nowarn_export_all]). %% TODO: To delete after buil

%% API
-export([]).
-export_type([pool/0]).

-type pool() :: ets:tid().

-define(TABS_CONFIGURATION, [
    {read_concurrency, true}, % Prepared for concurrent reading
    protected                 % Any can query the table
]).

-type id()   :: neuron:id() | cortex:id().
% -type conn() :: {network:d_type(), in | out, id()}.
-record(nn_pool, {
    ptab :: ets:tid(), % Contains the tab with pid<->id
    ctab :: ets:tid()  % Future expansion to integrate connections
}).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Mounts the pool using a list of {id, pid}.
%% @end
%%--------------------------------------------------------------------
-spec mount(Supervisor, Cortex_Id, NN) -> pool() when 
    Supervisor :: pid(),
    Cortex_Id  :: cortex:id(), 
    NN         :: network:network().
mount(Supervisor, Cx_Id, NN) -> 
    PT   = ets:new(processes, ?TABS_CONFIGURATION),
    Regs = [start_neuron(Supervisor,Id) || Id<-network:neurons(NN)],
    true = ets:insert(PT, [{Cx_Id,self()} || {self(),Cx_Id}]),
    true = ets:insert(PT, [{Id,Pid} || {Id,Pid} <- Regs]),
    true = ets:insert(PT, [{Pid,Id} || {Id,Pid} <- Regs]),
    NN_Pool = #nn_pool{ptab = PT},
    [neuron:go(Pid,network:node(NN,Id),NN_Pool) || {Id,Pid} <- Regs],
    NN_Pool.

%%--------------------------------------------------------------------
%% @doc Returns the pid of a neuron from its id.
%% @end
%%--------------------------------------------------------------------
-spec pid(Pool :: pool(), Id :: id()) -> Pid :: pid().
pid(#nn_pool{ptab = PT}, Id) ->
    [{Id, Pid}] = ets:lookup(PT, Id),
    Pid.

%%--------------------------------------------------------------------
%% @doc Returns the id of a neuron from its pid.
%% @end
%%--------------------------------------------------------------------
-spec id(Pool :: pool(), Pid :: pid()) -> Id :: id().
id(#nn_pool{ptab = PT}, Pid) ->
    [{Pid, Id}] = ets:lookup(PT, Pid),
    Id.


%%====================================================================
%% Internal functions
%%====================================================================

% Starts a neuron ---------------------------------------------------
start_neuron(Supervisor,Id) -> 
    {ok, Pid} = nn_sup:start_neuron(Supervisor,Id),
    {Id, Pid}.


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
