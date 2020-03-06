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
	sequential(Layers, nnref:new()).

-spec sequential(Layers :: [layer:specifications()], 
                 Name :: atom()) ->
	Model_specifications :: model:specifications().
sequential(Layers, Name) ->
	model:sequential(Layers, Name).

%%--------------------------------------------------------------------
%% @doc Returns a recurrent model from the defined layers.
%% @end
%%--------------------------------------------------------------------
-spec recurrent(Layers :: [layer:specifications()],
			    RLevel :: integer()) ->
	Model_specifications :: model:specifications().
recurrent(Layers, RLevel) ->
	recurrent(Layers, RLevel, nnref:new()).

-spec recurrent(Layers :: [layer:specifications()], 
			    RLevel :: integer(), Name :: atom()) ->
	Model_specifications :: model:specifications().
recurrent(Layers, RLevel, Name) ->
	model:recurrent(Layers, RLevel, Name).

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
-spec predict(Cortex_PId :: pid(), ExternalInputs :: [float()]) ->
	[Prediction :: float()].
predict(Cortex_PId, [_ | _] = ExternalInputs) ->
	[cortex:predict(Cortex_PId, Inputs) || Inputs <- ExternalInputs];
predict(_Cortex_PId, []) ->
	[].

%%--------------------------------------------------------------------
%% @doc Supervised ANN training function. Fits the Predictions to the 
%% OptimalOutputs.
%% @end
%%--------------------------------------------------------------------
-spec fit(Cortex_PId :: pid(), ExternalInputs :: [float()], 
          OptimalOutputs :: [float()]) ->
	{Loss :: [float()], Predictions :: [float()]}.
fit(Cortex_PId, ExternalInputs, OptimalOutputs) ->
	Result = [fit_cycle(Cortex_PId, I, O) || {I, O} <- lists:zip(ExternalInputs, OptimalOutputs)],
	{_Loss, _Predictions} = lists:unzip(Result).

%% TODO: Implement a real by BatchSize functionality
fit(Cortex_PId, ExternalInputs, OptimalOutputs, Batch_Size) ->
	{LossList, Predictions} = fit(Cortex_PId, ExternalInputs, OptimalOutputs),
	{averageLoss(LossList, Batch_Size), Predictions}.

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
	Cortex = nndb:read(Cortex_Id),
	length(elements:outputs_ids(Cortex)). % Cortex inputs are the output neurons

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
	Cortex = nndb:read(Cortex_Id),
	length(elements:inputs_idps(Cortex)). % Cortex outputs are the input neurons

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
clone({_, cortex} = Cortex_Id) ->
	Cortex = nndb:read(Cortex_Id),
	{Clone, ConversionETS} = elements:clone_cortex(Cortex),
	Neurons_Ids = elements:neurons(Cortex),
	Neurons = [elements:clone_neuron(Neuron, ConversionETS) || Neuron <- nndb:read(Neurons_Ids)],
	ets:delete(ConversionETS),
	nndb:write([Clone | Neurons]),
	elements:id(Clone).

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
start_nn(Cortex_Id) ->
	{ok, NN_PId} = enn_sup:start_nn_supervisor(Cortex_Id),
	{ok, Cortex_PId} = nn_sup:start_cortex(NN_PId, Cortex_Id),
	{ok, Cortex_PId}.

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
	Cortex = nndb:read(Cortex_Id),
	Neurons = nndb:read(elements:neurons(Cortex)),
	check_links(Cortex, Neurons),
	check_inputs(Cortex, Neurons),
	check_outputs(Cortex, Neurons),
	ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================

% ......................................................................................................................
fit_cycle(Cortex_PId, ExternalInputs, OptimalOutputs) ->
	Predictions = cortex:predict(Cortex_PId, ExternalInputs),
	Errors = cortex:fit(Cortex_PId, OptimalOutputs),
	{_Loss = math:sqrt(lists:sum([math:pow(E, 2) || E <- Errors])), Predictions}.

% ......................................................................................................................
averageLoss(LossList, Batch_Size) ->
	averageLoss(LossList, Batch_Size, length(LossList)).

averageLoss(LossList, Batch_Size, C) when Batch_Size < C ->
	{Set, Rest} = lists:split(Batch_Size, LossList),
	[lists:sum(Set) / Batch_Size | averageLoss(Rest, Batch_Size, C - Batch_Size)];
averageLoss(_LossList, _Batch_Size, _C) ->
	[].

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


