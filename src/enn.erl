%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%% 
%%% @TODO: Remove concept of layer. In the model might be correct but
%%% not necessarely on the cortex. Be carefull with deadlocks on 
%%% recurrence and neurons call backs.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(enn).
-author("borja").

-include_lib("nn_pool.hrl").
-include_lib("kernel/include/logger.hrl").

%% API
-export([start/1, stop/1, predict/2, fit/3, clone/1]).
-export([compile/1, run/4, inputs/1, outputs/1, pformat/1]).
-export([link_network/1, cortex_pid/1]).
-export([attributes_table/0, check_nn/1]).
-export_types([id/0, model/0]).

-type id()    :: cortex:id().
-type model() :: model:specifications().


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
predict(Cortex_Id, InputsList) ->
    Options = [{return, [prediction]}],
    [Prediction] = run(Cortex_Id, InputsList, [], Options),
    Prediction.

%%--------------------------------------------------------------------
%% @doc Supervised ANN training function. Fits the Predictions to the 
%% OptimalOutputs. Returns the errors between prediction and optima.
%% @end
%%--------------------------------------------------------------------
-spec fit(Network_id :: id(), InputsList :: [float()], 
          OptimaList :: [float()]) ->
    Errors :: [float()].
fit(Cortex_Id, InputsList, OptimaList) ->
    Options = [{return, [loss]}, {print, 10}],
    [Loss] = run(Cortex_Id, InputsList, OptimaList, Options),
    Loss.

%%--------------------------------------------------------------------
%% @doc Runs an ANN with the criteria defined at the options.
%% @end
%%--------------------------------------------------------------------
-spec run(Network_id :: id(), InputsList :: [float()], 
          OptimaList :: [float()], Options :: [training:option()]) ->
    Errors :: [float()].
run(Cortex_Id, InputsList, OptimaList, Options) ->
    Cortex_Pid = cortex_pid(Cortex_Id),
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
inputs({_, cortex} = Cortex_Id) ->
    Cortex = edb:read(Cortex_Id),
    % Cortex inputs are the output neurons
    length(elements:outputs_ids(Cortex)). 

%%--------------------------------------------------------------------
%% @doc Returns the number of outputs a network expects.
%% @end
%%--------------------------------------------------------------------
-spec outputs(Network :: model() | id()) ->
    NumberOfOtputs :: integer().
outputs(Model) when is_map(Model) ->
    #{layers := #{1.0 := #{units := N_Outputs}}} = Model,
    N_Outputs;
outputs({_, cortex} = Cortex_Id) ->
    Cortex = edb:read(Cortex_Id),
    % Cortex outputs are the input neurons
    length(elements:inputs_ids(Cortex)). 

%%--------------------------------------------------------------------
%% @doc Clones a network. Each element of the newtork is cloned inside
%% the mnesia database but with a different id.
%% @end
%%--------------------------------------------------------------------
-spec clone(Network_id :: id()) -> 
    Cloned_Id :: id().
clone({_, cortex} = Cortex_Id) ->
    Cortex = edb:read(Cortex_Id),
    {Clone, ConversionETS} = elements:clone_cortex(Cortex),
    Neurons_Ids = elements:neurons(Cortex),
    Neurons = [elements:clone_neuron(Neuron, ConversionETS) 
                || Neuron <- edb:read(Neurons_Ids)],
    ets:delete(ConversionETS),
    edb:write([Clone | Neurons]),
    elements:id(Clone).

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
%% @doc Links the caller to the network supervisor so if the caller 
%% dies because of an exception, the newtwork die shutdown as well.
%% @end
%%--------------------------------------------------------------------
-spec link_network(Network_id :: id()) -> true.
link_network(Cortex_Id) -> 
    Pid = ets:lookup_element(?NN_POOL, Cortex_Id, #nn.supervisor),
    link(Pid).

%%--------------------------------------------------------------------
%% @doc Returns the pid of the cortex.
%% @end
%%--------------------------------------------------------------------
-spec cortex_pid(Network_id :: id()) -> pid().
cortex_pid(Cortex_Id) -> 
    ets:lookup_element(?NN_POOL, Cortex_Id, #nn.cortex).

%%--------------------------------------------------------------------
%% @doc Returns a character list that represents the element of the Id
%% formatted in accordance with Format.
%% @end
%%--------------------------------------------------------------------
-spec pformat(Id) -> Chars when 
      Id :: neuron:id() | cortex:id(),
      Chars :: io_lib:chars().
pformat(Id) ->
    elements:pformat(Id).

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
check_nn(Cortex_Id) ->
    Cortex = edb:read(Cortex_Id),
    Neurons = edb:read(elements:neurons(Cortex)),
    check_links(Cortex, Neurons),
    check_inputs(Cortex, Neurons),
    check_outputs(Cortex, Neurons),
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================

% ....................................................................
check_links(Cortex, Neurons) -> %TODO: Check using elements:link
    InL = lists:append(
        [[{In, elements:id(Cortex)} || In <- elements:inputs_ids(Cortex)] |
         [[{In, neuron:id(N)} || In <- elements:inputs_ids(N)] || N <- Neurons]]),
    OutL = lists:append(
        [[{elements:id(Cortex), Out} || Out <- elements:outputs_ids(Cortex)] |
         [[{neuron:id(N), Out} || Out <- elements:outputs_ids(N)] || N <- Neurons]]),
    case InL -- OutL of
        [] -> ok;
        Diff ->
            ?LOG_ERROR("Broken NN on cortex ~p with links ~p", [elements:id(Cortex), Diff]),
            error(broken_nn)
    end.

% ....................................................................
check_inputs(Cortex, Neurons) ->
    is_broken_at_inputs(Cortex),
    case lists:any(fun is_broken_at_inputs/1, Neurons) of
        false -> ok;
        true ->
            ?LOG_ERROR("Broken NN on ~p neurons", [elements:id(Cortex)]),
            error(broken_nn)
    end.

is_broken_at_inputs(Element) ->
    Inputs = elements:inputs_idps(Element),
    case Inputs of
        [] ->
            ?LOG_ERROR("Broken NN ~p, empty neuron inputs", [elements:id(Element)]),
            true;
        _NonEmpty ->
            false
    end.

% ....................................................................
check_outputs(Cortex, Neurons) ->
    is_broken_at_outputs(Cortex),
    case lists:any(fun is_broken_at_outputs/1, Neurons) of
        false -> ok;
        true ->
            ?LOG_ERROR("Broken NN on ~p neurons", [elements:id(Cortex)]),
            error(broken_nn)
    end.

is_broken_at_outputs(Element) ->
    Outputs = elements:outputs_ids(Element),
    case Outputs of
        [] ->
            ?LOG_ERROR("Broken NN ~p, empty neuron outputs", [elements:id(Element)]),
            true;
        _NonEmpty ->
            false
    end.

