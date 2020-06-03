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
-export([compile/2, run/4, inputs/1, outputs/1, neurons/1]).
-export([status/1, info/1, link/1, cortex/1]).
-export_types([id/0, model/0]).

-type network() :: nnet:id().
-type neuron()  :: nnet:neuron().
-type model()   :: model:definition().


%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Compiles and stores a model returning its network id.
%% @end
%%--------------------------------------------------------------------
-spec compile(Model, Layers) -> Network when 
    Model  :: #{Name::atom() => model:connections()},
    Layers :: #{Name::atom() => layer:definition() },
    Network :: network().
compile(Model, Layers) ->
    % TODO: Check keys in Model and Layers are the same
    CompiledLayers = maps:map(fun layer:compile/2, Layers),
    model:compile(Model, CompiledLayers).

%%--------------------------------------------------------------------
%% @doc Clones a network. Each element of the newtork is cloned inside
%% the mnesia database but with a different id.
%% @end
%%--------------------------------------------------------------------
-spec clone(Network :: network()) -> 
    Cloned :: network().
clone(Network) ->
    nnet:clone(Network).

%%--------------------------------------------------------------------
%% @doc Uses a ANN to create a prediction. The ANN is refered by using
%% the cortex pid.
%% @end
%%--------------------------------------------------------------------
-spec predict(Network :: network(), InputsList :: [[float()]]) ->
    [Prediction :: [float()]].
predict(Network, InputsList) ->
    Options = [{return, [prediction]}],
    [Prediction] = run(Network, InputsList, [], Options),
    Prediction.

%%--------------------------------------------------------------------
%% @doc Supervised ANN training function. Fits the Predictions to the 
%% OptimalOutputs. Returns the errors between prediction and optima.
%% @end
%%--------------------------------------------------------------------
-spec fit(Network :: network(), InputsList :: [float()], 
          OptimaList :: [float()]) ->
    Errors :: [float()].
fit(Network, InputsList, OptimaList) ->
    Options = [{return, [loss]}, {print, 10}],
    [Loss] = run(Network, InputsList, OptimaList, Options),
    Loss.

%%--------------------------------------------------------------------
%% @doc Runs an ANN with the criteria defined at the options.
%% @end
%%--------------------------------------------------------------------
-spec run(Network :: network(), InputsList :: [float()], 
          OptimaList :: [float()], Options :: [training:option()]) ->
    Errors :: [float()].
run(Network, InputsList, OptimaList, Options) ->
    Cortex_Pid = cortex(Network),
    training:start_link(Cortex_Pid, InputsList, OptimaList, Options).

%%--------------------------------------------------------------------
%% @doc Returns the number of inputs a network expects.
%% @end
%%--------------------------------------------------------------------
-spec inputs(Network :: model() | network()) ->
    NumberOfInputs :: integer().
inputs(Model) when is_map(Model) ->
    #{layers := #{-1.0 := #{units := N_Inputs}}} = Model,
    N_Inputs;
inputs(Network) ->
    nnet:in_degree(Network). 

%%--------------------------------------------------------------------
%% @doc Returns the number of outputs a network expects.
%% @end
%%--------------------------------------------------------------------
-spec outputs(Network :: model() | network()) ->
    NumberOfOtputs :: integer().
outputs(Model) when is_map(Model) ->
    #{layers := #{1.0 := #{units := N_Outputs}}} = Model,
    N_Outputs;
outputs(Network) ->
    nnet:out_degree(Network).

%%-------------------------------------------------------------------
%% @doc Returns a list of all neurons of the network.  
%% @end
%%-------------------------------------------------------------------
-spec neurons(Network :: network()) -> [neuron()].
neurons(Network) -> 
    nnet:neurons(Network).

%%--------------------------------------------------------------------
%% @doc Start a neural network, ready to receive inputs or training.
%% @end
%%--------------------------------------------------------------------
-spec start(Network :: model() | network()) ->
    Network :: network().
start(Model) when is_map(Model) ->
    start(compile(Model));
start(Network) ->
    ok = enn_sup:start_nn(Network),
    Network.

%%--------------------------------------------------------------------
%% @doc Stops a neural network.
%% @end
%%--------------------------------------------------------------------
-spec stop(Network :: network()) -> Result when
      Result :: 'ok' | {'error', Error},
      Error :: 'not_found' | 'simple_one_for_one'.
stop(Network) ->
    enn_sup:terminate_nn(Network).

%%--------------------------------------------------------------------
%% @doc Returns the network information of the specified network id.
%% @end
%%--------------------------------------------------------------------
-spec info(Network :: network()) -> Info when
      Info :: nnet:info().
info(Network) -> 
    nnet:info(Network).

%%--------------------------------------------------------------------
%% @doc Returns the status of the specified network id.
%% @end
%%--------------------------------------------------------------------
-spec status(Network :: network()) -> Status when
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
-spec link(Network :: network()) -> true.
link(Network) -> 
    #{supervisor:=Pid} = enn_pool:info(Network),
    erlang:link(Pid).

%%--------------------------------------------------------------------
%% @doc Returns the pid of the cortex.
%% @end
%%--------------------------------------------------------------------
-spec cortex(Network :: network()) -> pid().
cortex(Network) -> 
    #{cortex:=Pid} = enn_pool:info(Network),
    Pid.


%%%===================================================================
%%% Internal functions
%%%===================================================================



