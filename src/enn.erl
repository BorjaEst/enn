%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(enn).
-author("borja").
-compile({no_auto_import,[link/1]}).

%% API
-export([start/1, stop/1, predict/2, fit/3, clone/1]).
-export([compile/1, run/4, inputs/1, outputs/1]).
-export([status/1, info/1, link/1, cortex/1]).
-export([attributes_table/0, check/1]).
-export_types([id/0, model/0]).

-type id()    :: network:id().
-type model() :: model:definition().


%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Returns a list of tuples with the record name and attributes
%% list. This is mainly used to prepare the tables in mnesia.
%% @end
%%--------------------------------------------------------------------
-spec attributes_table() -> 
    [{Elem :: nnelements:element(), [Attr :: atom()]}].
attributes_table() ->
    [
        {cortex, elements:fields(cortex)},
        {neuron, elements:fields(neuron)}
    ].

%%--------------------------------------------------------------------
%% @doc Compiles and stores a model returning its network id.
%% @end
%%--------------------------------------------------------------------
-spec compile(Model :: model()) -> 
    Network_id :: id().
compile(Model) ->
    model:compile(Model).

%%--------------------------------------------------------------------
%% @doc Uses a ANN to create a prediction. The ANN is refered by using
%% the cortex pid.
%% @end
%%--------------------------------------------------------------------
-spec predict(Network_id :: id(), InputsList :: [[float()]]) ->
    [Prediction :: [float()]].
predict(Network_id, InputsList) ->
    Options = [{return, [prediction]}],
    [Prediction] = run(Network_id, InputsList, [], Options),
    Prediction.

%%--------------------------------------------------------------------
%% @doc Supervised ANN training function. Fits the Predictions to the 
%% OptimalOutputs. Returns the errors between prediction and optima.
%% @end
%%--------------------------------------------------------------------
-spec fit(Network_id :: id(), InputsList :: [float()], 
          OptimaList :: [float()]) ->
    Errors :: [float()].
fit(Network_id, InputsList, OptimaList) ->
    Options = [{return, [loss]}, {print, 10}],
    [Loss] = run(Network_id, InputsList, OptimaList, Options),
    Loss.

%%--------------------------------------------------------------------
%% @doc Runs an ANN with the criteria defined at the options.
%% @end
%%--------------------------------------------------------------------
-spec run(Network_id :: id(), InputsList :: [float()], 
          OptimaList :: [float()], Options :: [training:option()]) ->
    Errors :: [float()].
run(Network_id, InputsList, OptimaList, Options) ->
    Cortex_Pid = enn:cortex_pid(Network_id),
    training:start_link(Cortex_Pid, InputsList, OptimaList, Options).

%%--------------------------------------------------------------------
%% @doc Returns the number of inputs a network expects.
%% @end
%%--------------------------------------------------------------------
-spec inputs(Network :: model() | id()) ->
    NumberOfInputs :: integer().
inputs(Model) when is_map(Model) ->
    #{layers := #{-1.0 := #{units := N_Inputs}}} = Model,
    N_Inputs;
inputs({_, network} = Network_id) ->
    network:in_degree(edb:read(Network_id)). 

%%--------------------------------------------------------------------
%% @doc Returns the number of outputs a network expects.
%% @end
%%--------------------------------------------------------------------
-spec outputs(Network :: model() | id()) ->
    NumberOfOtputs :: integer().
outputs(Model) when is_map(Model) ->
    #{layers := #{1.0 := #{units := N_Outputs}}} = Model,
    N_Outputs;
outputs({_, network} = Network_id) ->
    network:out_degree(edb:read(Network_id)).

%%--------------------------------------------------------------------
%% @doc Clones a network. Each element of the newtork is cloned inside
%% the mnesia database but with a different id.
%% @end
%%--------------------------------------------------------------------
-spec clone(Network_id :: id()) -> 
    Cloned_Id :: id().
clone({_, network} = Network_id) ->
    NN    = edb:read(Network_id),
    NMap  = clone_and_save_neurons(network:neurons(NN)),
            clone_and_save_links(network:links(NN), NMap),
    Clone = network:replace(NN, NMap),
    edb:write(Clone),
    network:id(Clone).

%%--------------------------------------------------------------------
%% @doc Start a neural network, ready to receive inputs or training.
%% @end
%%--------------------------------------------------------------------
-spec start(Network :: model() | id()) ->
    Network_id :: id().
start(Model) when is_map(Model) ->
    start(compile(Model));
start(Cortex_Id) ->
    ok = enn_sup:start_nn(Cortex_Id),
    Cortex_Id.

%%--------------------------------------------------------------------
%% @doc Stops a neural network.
%% @end
%%--------------------------------------------------------------------
-spec stop(Network_id :: id()) -> Result when
      Result :: 'ok' | {'error', Error},
      Error :: 'not_found' | 'simple_one_for_one'.
stop(Cortex_Id) ->
    enn_sup:terminate_nn(Cortex_Id).

%%--------------------------------------------------------------------
%% @doc Returns the network information of the specified network id.
%% @end
%%--------------------------------------------------------------------
-spec info(Network_Id :: id()) -> Info when
      Info :: network:info().
info(Network_Id) -> 
    network:info(edb:read(Network_Id)).

%%--------------------------------------------------------------------
%% @doc Returns the status of the specified network id.
%% @end
%%--------------------------------------------------------------------
-spec status(Network_Id :: id()) -> Status when
      Status :: enn_pool:info() | not_started.
status(Network_Id) -> 
    try enn_pool:info(Network_Id) of 
          Info          -> Info
    catch error:badarg  -> not_started
    end.

%%--------------------------------------------------------------------
%% @doc Links the caller to the network supervisor so if the caller 
%% dies because of an exception, the newtwork die shutdown as well.
%% @end
%%--------------------------------------------------------------------
-spec link(Network_id :: id()) -> true.
link(Network_Id) -> 
    #{supervisor:=Pid} = enn_pool:info(Network_Id),
    erlang:link(Pid).

%%--------------------------------------------------------------------
%% @doc Returns the pid of the cortex.
%% @end
%%--------------------------------------------------------------------
-spec cortex(Network_id :: id()) -> pid().
cortex(Network_Id) -> 
    #{cortex:=Pid} = enn_pool:info(Network_Id),
    Pid.

%%--------------------------------------------------------------------
%% @doc Check the network is in optimal status.
%% @end
%%--------------------------------------------------------------------
-spec check(Network_id :: id()) -> ok.
check(Network_id) ->
    NN  = edb:read(Network_id),
    [] = network:sink_neurons(NN), 
    [] = network:bias_neurons(NN),
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================

% Clones and saves the neurons ----------------------------------------
clone_and_save_neurons(Neurons) -> 
    Clones = [neuron:clone(N) || N <- Neurons],
    edb:write(Clones),
    maps:from_list(lists:zip(Neurons, Clones)).

% Clones and saves the links ----------------------------------------
clone_and_save_links(Links, NMap) -> 
    Clones = link:replace(Links, NMap),
    edb:write(Clones).


