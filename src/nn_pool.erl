%%%-------------------------------------------------------------------
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
    public                    % The cortes muxt be able to write
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
%% @doc Returns a NN_Pool.
%% @end
%%--------------------------------------------------------------------
-spec new(Network::enn:network()) -> NN_Pool::pool().
new(Network) -> 
    NN_Pool = #nn_pool{
        ptab = ets:new(processes, ?TABS_CONFIGURATION)
    },
    true = enn_pool:register_nn_pool(Network, NN_Pool).
    
%%--------------------------------------------------------------------
%% @doc Mounts the pool using a list of {id, pid}.
%% @end
%%--------------------------------------------------------------------
-spec mount(Network, NNodes) -> ok when 
    Network :: enn:network(),
    NNodes  :: #{enn:nnode() => nnode}.
mount(Network, NNodes) -> 
    #{nn_sup:=Supervisor, nn_pool:=NN_Pool} = enn_pool:info(Network),
    #nn_pool{ptab=PT} = NN_Pool,
    Regs = [start_neuron(Supervisor,Id) || Id <- maps:keys(NNodes)],
    true = ets:insert(PT, cortex_registers(Network)),
    true = ets:insert(PT, [{Id,Pid} || {Id,Pid} <- Regs]),
    true = ets:insert(PT, [{Pid,Id} || {Id,Pid} <- Regs]),
    [neuron:cortex_synch(Pid, NN_Pool) || {_,Pid} <- Regs],
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

%%--------------------------------------------------------------------
%% @doc Returns a list with all the table pids (including cortex).
%% @end
%%--------------------------------------------------------------------
-spec pids(Pool :: pool()) -> [PId::pid()].
pids(#nn_pool{ptab = PT}) ->
    [Pid || {Pid,_} <- ets:tab2list(PT), is_pid(Pid)].


%%====================================================================
%% Internal functions
%%====================================================================

% Starts a neuron ---------------------------------------------------
start_neuron(Supervisor,Id) -> 
    {ok, Pid} = nn_sup:start_neuron(Supervisor,Id),
    {Id, Pid}.

% Cortex registers in the ptab --------------------------------------
cortex_registers(Network) ->
    Pid = self(),
    [{Network,Pid}, {Pid,Network}].


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
