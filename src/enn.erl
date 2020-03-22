%%%-------------------------------------------------------------------
%%% @author borja
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 
%%% @TODO: Remove concept of layer. In the model might be correct but
%%% not necessarely on the cortex. Be carefull with deadlocks on 
%%% recurrence and neurons call backs.
%%%
%%% @end
%%% Created : 22. Sep 2018 18:46
%%%-------------------------------------------------------------------
-module(enn).
-author("borja").
-compile([export_all, nowarn_export_all]). %%TODO: To delete after build

-include_lib("kernel/include/logger.hrl").

%% API
-export([]).


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
%% @doc Returns a sequential model from the defined layers.
%% @end
%%--------------------------------------------------------------------
-spec sequential([Layer :: layer:specifications()]) -> 
	Model_specifications :: model:specifications().
sequential(Layers) ->
	model:sequential(Layers).

%%--------------------------------------------------------------------
%% @doc Returns a recurrent model from the defined layers.
%% @end
%%--------------------------------------------------------------------
-spec recurrent(Layers :: [layer:specifications()], 
			    RLevel :: integer()) ->
	Model_specifications :: model:specifications().
recurrent(Layers, RLevel) ->
	model:recurrent(Layers, RLevel).

%%--------------------------------------------------------------------
%% @doc Compiles and stores a model in the DB returning its cortex_id.
%% @end
%%--------------------------------------------------------------------
-spec compile(Model :: model:specifications()) -> 
	Cortex_id :: cortex:id().
compile(Model) ->
	model:compile(Model).

%%--------------------------------------------------------------------
%% @doc Uses a ANN to create a prediction. The ANN is refered by using
%% the cortex pid.
%% @end
%%--------------------------------------------------------------------
-spec predict(Cortex_Pid :: pid(), InputsList :: [[float()]]) ->
	[Prediction :: [float()]].
predict(Cortex_Pid, InputsList) ->
	Options = [{return, [prediction]}],
	[Prediction] = run(Cortex_Pid, InputsList, [], Options),
	Prediction.

%%--------------------------------------------------------------------
%% @doc Supervised ANN training function. Fits the Predictions to the 
%% OptimalOutputs. Returns the errors between prediction and optima.
%% @end
%%--------------------------------------------------------------------
-spec fit(Cortex_Pid :: pid(), InputsList :: [float()], 
          OptimaList :: [float()]) ->
	Errors :: [float()].
fit(Cortex_Pid, InputsList, OptimaList) ->
	Options = [{return, [errors]}],
	[Errors] = run(Cortex_Pid, InputsList, OptimaList, Options),
	Errors.

%%--------------------------------------------------------------------
%% @doc Runs an ANN with the criteria defined at the options.
%% @end
%%--------------------------------------------------------------------
-spec run(Cortex_Pid :: pid(), InputsList :: [float()], 
          OptimaList :: [float()], Options :: [training:option()]) ->
	Errors :: [float()].
run(Cortex_Pid, InputsList, OptimaList, Options) ->
	training:start_link(Cortex_Pid, InputsList, OptimaList, Options).

%%--------------------------------------------------------------------
%% @doc Returns the number of inputs a Model/Cortex expects.
%% @end
%%--------------------------------------------------------------------
-spec inputs(ANN :: model:specifications() | cortex:id()) ->
	NumberOfInputs :: integer().
inputs(Model) when is_map(Model) ->
	#{layers := #{-1.0 := #{units := N_Inputs}}} = Model,
	N_Inputs;
inputs({_, cortex} = Cortex_Id) ->
	Cortex = edb:read(Cortex_Id),
	% Cortex inputs are the output neurons
	length(elements:outputs_ids(Cortex)). 

%%--------------------------------------------------------------------
%% @doc Returns the number of outputs a Model/Cortex expects.
%% @end
%%--------------------------------------------------------------------
-spec outputs(ANN :: model:specifications() | cortex:id()) ->
	NumberOfOtputs :: integer().
outputs(Model) when is_map(Model) ->
	#{layers := #{1.0 := #{units := N_Outputs}}} = Model,
	N_Outputs;
outputs({_, cortex} = Cortex_Id) ->
	Cortex = edb:read(Cortex_Id),
	% Cortex outputs are the input neurons
	length(elements:inputs_idps(Cortex)). 

%%--------------------------------------------------------------------
%% @doc Clones a network. Each element of the newtork is cloned inside
%% the mnesia database but with a different id.
%% @end
%%--------------------------------------------------------------------
-spec clone(Cortex_Id :: cortex:id()) -> 
	CortexClone_Id :: cortex:id().
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
-spec start_nn(Cortex_Id :: cortex:id()) -> 
	{ok, Cortex_Pid :: pid()}.
start_nn(Cortex_Id) ->
	{ok, NN_Pid} = enn_sup:start_nn_supervisor(Cortex_Id),
	{ok, Cortex_Pid} = nn_sup:start_cortex(NN_Pid, Cortex_Id),
	{ok, Cortex_Pid}.

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
stop_nn(Cortex_Id) ->
	enn_sup:terminate_nn_supervisor(Cortex_Id).

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
pformat(Element) ->
	elements:pformat(Element).

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

% ......................................................................................................................
check_links(Cortex, Neurons) ->
	InL = lists:append(
		[[{In, elements:id(Cortex)} || In <- elements:inputs_ids(Cortex)] |
		 [[{In, neuron:id(N)} || In <- elements:inputs_ids(N) ++ elements:rcc_inputs_ids(N)] || N <- Neurons]]),
	OutL = lists:append(
		[[{elements:id(Cortex), Out} || Out <- elements:outputs_ids(Cortex)] |
		 [[{neuron:id(N), Out} || Out <- elements:outputs_ids(N) ++ elements:rcc_outputs_ids(N)] || N <- Neurons]]),
	case InL -- OutL of
		[] -> ok;
		Diff ->
			?LOG_ERROR("Broken NN on cortex ~p with links ~p", [elements:id(Cortex), Diff]),
			error(broken_nn)
	end.

% ......................................................................................................................
check_inputs(Cortex, Neurons) ->
	is_broken_at_inputs(Cortex),
	case lists:any(fun is_broken_at_inputs/1, Neurons) of
		false -> ok;
		true ->
			?LOG_ERROR("Broken NN on ~p neurons", [elements:id(Cortex)]),
			error(broken_nn)
	end.

is_broken_at_inputs(Element) ->
	Inputs = elements:inputs_idps(Element) ++ elements:rcc_inputs_idps(Element),
	case Inputs of
		[] ->
			?LOG_ERROR("Broken NN ~p, empty neuron inputs", [elements:id(Element)]),
			true;
		_NonEmpty ->
			false
	end.

% ......................................................................................................................
check_outputs(Cortex, Neurons) ->
	is_broken_at_outputs(Cortex),
	case lists:any(fun is_broken_at_outputs/1, Neurons) of
		false -> ok;
		true ->
			?LOG_ERROR("Broken NN on ~p neurons", [elements:id(Cortex)]),
			error(broken_nn)
	end.

is_broken_at_outputs(Element) ->
	Outputs = elements:outputs_ids(Element) ++ elements:rcc_outputs_ids(Element),
	case Outputs of
		[] ->
			?LOG_ERROR("Broken NN ~p, empty neuron outputs", [elements:id(Element)]),
			true;
		_NonEmpty ->
			false
	end.


