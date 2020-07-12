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
-export([start/1, start_link/1, stop/1, predict/2, fit/3, clone/1]).
-export([compile/1, run/4, inputs/1, outputs/1, neurons/1]).
-export([status/1, info/2, cortex/1]).
-export_types([network/0, neuron/0, model/0]).

-type network() :: nnet:id().
-type neuron()  :: nnet:neuron().
-type model()   :: nnet:model().


%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Compiles and stores a model returning its network id.
%% Should run inside mnesia transaction.
%% @end
%%--------------------------------------------------------------------
-spec compile(Model::model()) -> Network::network().
compile(Model) ->
    nnet:from_model(Model).

%%--------------------------------------------------------------------
%% @doc Clones a network. Each element of the newtork is cloned inside
%% the mnesia database but with a different id.
%% Should run inside mnesia transaction.
%% @end
%%--------------------------------------------------------------------
-spec clone(Network::network()) -> Cloned::network().
clone(Network) ->
    nnet:clone(Network).

%%--------------------------------------------------------------------
%% @doc Returns the number of inputs a network expects.
%% Should run inside mnesia transaction.
%% @end
%%--------------------------------------------------------------------
-spec inputs(model() | network()) -> NumberOfInputs::integer().
inputs(Model) when is_map(Model) ->
    #{inputs := #{units := N_Inputs}} = Model,
    N_Inputs;
inputs(Network) -> % System inputs are the nnet outputs
    length(nnet:out(Network)). 

%%--------------------------------------------------------------------
%% @doc Returns the number of outputs a network expects.
%% Should run inside mnesia transaction.
%% @end
%%--------------------------------------------------------------------
-spec outputs(model() | network()) -> NumberOfOtputs::integer().
outputs(Model) when is_map(Model) ->
    #{outputs := #{units := N_Outputs}} = Model,
    N_Outputs;
outputs(Network) -> % System outputs are the nnet inputs
    length(nnet:in(Network)).

%%-------------------------------------------------------------------
%% @doc Returns a list of all neurons of the network.  
%% Should run inside mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec neurons(Network::network()) -> Neurons::[neuron()].
neurons(Network) -> 
    maps:keys(nnet:nodes(Network)).

%%--------------------------------------------------------------------
%% @doc Returns the network information of the specified network id.
%% Should run inside mnesia transaction.
%% @end
%%--------------------------------------------------------------------
-spec info(Network::network(), Item::term()) -> Info::nnet:info().
info(Network, Item) -> 
    nnet:info(Network, Item).

%%--------------------------------------------------------------------
%% @doc Start a neural network, ready to receive inputs or training.
%% @end
%%--------------------------------------------------------------------
-spec start(Model | Network) -> Network when 
    Model   :: model(),
    Network :: network().
start(Model) when is_map(Model) ->
    {atomic, Network} = mnesia:transaction(fun() -> compile(Model) end),
    start(Network);
start(Network) ->
    case enn_sup:start_nn(Network) of 
        {ok, _Pid}                       -> Network;
        {error,{{_,{_,_, broken_nn}},_}} -> error(broken_nn)
    end. 

%%--------------------------------------------------------------------
%% @doc Start a neural network, and links the caller.
%% @end
%%--------------------------------------------------------------------
-spec start_link(Model | Network) -> Network when 
    Model   :: model(),
    Network :: network().
start_link(NetworkTerm) ->
    Network = start(NetworkTerm),
    erlang:link(cortex(Network)),
    Network.

%%--------------------------------------------------------------------
%% @doc Stops a neural network.
%% @end
%%--------------------------------------------------------------------
-spec stop(Network::network()) -> Result when
      Result :: 'ok' | {'error', Error},
      Error :: 'not_found'.
stop(Network) ->
    erlang:unlink(cortex(Network)),
    enn_sup:terminate_nn(Network).

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
%% @doc Returns the pid of the cortex.
%% @end
%%--------------------------------------------------------------------
-spec cortex(Network::network()) -> pid().
cortex(Network) -> 
    #{cortex:=Pid} = enn_pool:info(Network),
    Pid.

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


%%%===================================================================
%%% Internal functions
%%%===================================================================

