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
-export([compile/1, run/4, inputs/1, outputs/1, neurons/1]).
-export([status/1, info/1, link/1, cortex/1]).
-export_types([network/0, neuron/0, model/0]).

-type network() :: nnet:id().
-type neuron()  :: nnet:neuron().
-type model()   :: nnet:model().


%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Compiles and stores a model returning its network id.
%% @end
%%--------------------------------------------------------------------
-spec compile(Model::model()) -> {atomic, Network::network()}.
compile(Model) ->
    nnet:from_model(Model).

%%--------------------------------------------------------------------
%% @doc Clones a network. Each element of the newtork is cloned inside
%% the mnesia database but with a different id.
%% @end
%%--------------------------------------------------------------------
-spec clone(Network::network()) -> {atomic, Cloned::network()}.
clone(Network) ->
    nnet:clone(Network).

%%--------------------------------------------------------------------
%% @doc Uses a ANN to create a prediction. The ANN is refered by using
%% the cortex pid.
%% @end
%%--------------------------------------------------------------------
-spec predict(Network, Inputs) -> Predictions when
    Network     :: network(), 
    Inputs      :: [[float()]],
    Predictions :: [[float()]].
predict(Network, InputsList) ->
    Options = [{return, [prediction]}],
    [Prediction] = run(Network, InputsList, [], Options),
    Prediction.

%%--------------------------------------------------------------------
%% @doc Supervised ANN training function. Fits the Predictions to the 
%% OptimalOutputs. Returns the errors between prediction and optima.
%% @end
%%--------------------------------------------------------------------
-spec fit(Network, Inputs, Optima) -> Errors when
    Network :: network(), 
    Inputs  :: [[float()]],
    Optima  :: [[float()]],
    Errors  :: [[float()]].
fit(Network, InputsList, OptimaList) ->
    Options = [{return, [loss]}, {print, 10}],
    [Loss] = run(Network, InputsList, OptimaList, Options),
    Loss.

%%--------------------------------------------------------------------
%% @doc Runs an ANN with the criteria defined at the options.
%% @end
%%--------------------------------------------------------------------
-spec run(Network, Inputs, Optima, Options) -> Results when
    Network :: network(), 
    Inputs  :: [[float()]],
    Optima  :: [[float()]],
    Options :: [training:option()],
    Results :: [term()].
run(Network, InputsList, OptimaList, Options) ->
    Cortex_Pid = cortex(Network),
    training:start_link(Cortex_Pid, InputsList, OptimaList, Options).

%%--------------------------------------------------------------------
%% @doc Returns the number of inputs a network expects.
%% @end
%%--------------------------------------------------------------------
-spec inputs(Model::model() | Network::network()) -> 
    NumberOfInputs::integer().
inputs(Model) when is_map(Model) ->
    #{inputs := #{units := N_Inputs}} = Model,
    N_Inputs;
inputs(Network) ->
    #{inputs:=Inputs} = nnet:info(Network),
    length(Inputs). 

%%--------------------------------------------------------------------
%% @doc Returns the number of outputs a network expects.
%% @end
%%--------------------------------------------------------------------
-spec outputs(Model::model() | Network::network()) -> 
    NumberOfOtputs::integer().
outputs(Model) when is_map(Model) ->
    #{outputs := #{units := N_Outputs}} = Model,
    N_Outputs;
outputs(Network) ->
    #{outputs:=Outputs} = nnet:info(Network),
    length(Outputs).

%%-------------------------------------------------------------------
%% @doc Returns a list of all neurons of the network.  
%% @end
%%-------------------------------------------------------------------
-spec neurons(Network::network()) -> Neurons::[neuron()].
neurons(Network) -> 
    nnet:neurons(Network).

%%--------------------------------------------------------------------
%% @doc Start a neural network, ready to receive inputs or training.
%% @end
%%--------------------------------------------------------------------
-spec start(Model::model() | Network::network()) -> 
    Network::network().
start(Model) when is_map(Model) ->
    start(compile(Model));
start(Network) ->
    ok = enn_sup:start_nn(Network),
    Network.

%%--------------------------------------------------------------------
%% @doc Stops a neural network.
%% @end
%%--------------------------------------------------------------------
-spec stop(Network::network()) -> Result when
      Result :: 'ok' | {'error', Error},
      Error :: 'not_found'.
stop(Network) ->
    enn_sup:terminate_nn(Network).

%%--------------------------------------------------------------------
%% @doc Returns the network information of the specified network id.
%% @end
%%--------------------------------------------------------------------
-spec info(Network::network()) -> Info::nnet:info().
info(Network) -> 
    nnet:info(Network).

%%--------------------------------------------------------------------
%% @doc Returns the status of the specified network id.
%% @end
%%--------------------------------------------------------------------
-spec status(Network::network()) -> Status when
      Status :: enn_pool:info() | not_running.
status(Network) -> 
    try enn_pool:info(Network) of 
          Info          -> Info
    catch error:badarg  -> not_running 
    end.

%%--------------------------------------------------------------------
%% @doc Links the caller to the network supervisor so if the caller 
%% dies because of an exception, the newtwork die shutdown as well.
%% @end
%%--------------------------------------------------------------------
-spec link(Network::network()) -> true.
link(Network) -> 
    #{supervisor:=Pid} = enn_pool:info(Network),
    erlang:link(Pid).

%%--------------------------------------------------------------------
%% @doc Returns the pid of the cortex.
%% @end
%%--------------------------------------------------------------------
-spec cortex(Network::network()) -> pid().
cortex(Network) -> 
    #{cortex:=Pid} = enn_pool:info(Network),
    Pid.


%%%===================================================================
%%% Internal functions
%%%===================================================================

