%%%-------------------------------------------------------------------
%%% @author borja
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Sep 2018 18:46
%%%-------------------------------------------------------------------
-module(enn).
-author("borja").
-compile([export_all, nowarn_export_all]). %%TODO: To delete after build

%%% TODO -------------------------------------------------------------
% TODO: Eliminar el concepto de layer, en el modelo es correcto pero no el en cortex
% TODO: Si se elimina el concepto de layers, hay que tener cuidado con los bloqueos conmo en la recurrencia

-include_lib("layers.hrl").
-include_lib("nnelements.hrl").
-include_lib("kernel/include/logger.hrl").

%% API
-export([]).

-define(ENN_TABLES_ATTRIBUTES_LIST,
	[
		{cortex, record_info(fields, cortex)},
		{neuron, record_info(fields, neuron)}
	]).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
attributes_table() ->
	?ENN_TABLES_ATTRIBUTES_LIST.

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
sequential(Layers) ->
	_Model = sequential(Layers, nnref:new()).

sequential(Layers, Name) ->
	_Model = model:sequential(Layers, Name).

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
recurrent(Layers, RLevel) ->
	_Model = recurrent(Layers, RLevel, nnref:new()).

recurrent(Layers, RLevel, Name) ->
	_Model = model:recurrent(Layers, RLevel, Name).

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
compile(Model) ->
	_Cortex_Id = model:compile(Model).

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
predict(Cortex_PId, [_ | _] = ExternalInputs) ->
	_Predictions = [cortex:predict(Cortex_PId, Inputs) || Inputs <- ExternalInputs];
predict(_Cortex_PId, []) ->
	[].

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
fit(Cortex_PId, ExternalInputs, OptimalOutputs) ->
	Result = [fit_cycle(Cortex_PId, I, O) || {I, O} <- lists:zip(ExternalInputs, OptimalOutputs)],
	{_Loss, _Predictions} = lists:unzip(Result).

fit(Cortex_PId, ExternalInputs, OptimalOutputs, Batch_Size) ->
	{LossList, Predictions} = fit(Cortex_PId, ExternalInputs, OptimalOutputs),
	{averageLoss(LossList, Batch_Size), Predictions}.

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
inputs(Model) when is_map(Model) ->
	#{layers := #{-1.0 := #{units := N_Inputs}}} = Model,
	N_Inputs;
inputs({_, cortex} = Cortex_Id) ->
	Cortex = nndb:read(Cortex_Id),
	length(Cortex#cortex.outputs_ids). % Cortex inputs are the output neurons

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
outputs(Model) when is_map(Model) ->
	#{layers := #{1.0 := #{units := N_Outputs}}} = Model,
	N_Outputs;
outputs({_, cortex} = Cortex_Id) ->
	Cortex = nndb:read(Cortex_Id),
	length(Cortex#cortex.inputs_idps). % Cortex outputs are the input neurons

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
clone({_, cortex} = Cortex_Id) ->
	Cortex = nndb:read(Cortex_Id),
	{Clone, ConversionETS} = nn_elements:clone_cortex(Cortex),
	Neurons_Ids = nn_elements:neurons(Cortex),
	Neurons = [nn_elements:clone_neuron(Neuron, ConversionETS) || Neuron <- nndb:read(Neurons_Ids)],
	ets:delete(ConversionETS),
	nndb:write([Clone | Neurons]),
	Clone#cortex.id.

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
	nn_elements:pformat(Element).

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
check_nn(Cortex_Id) ->
	Cortex = nndb:read(Cortex_Id),
	Neurons = nndb:read(nn_elements:neurons(Cortex)),
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
		[[{In, Cortex#cortex.id} || In <- nn_elements:inputs_ids(Cortex)] |
		 [[{In, N#neuron.id} || In <- nn_elements:inputs_ids(N)] || N <- Neurons]]),
	OutL = lists:append(
		[[{Cortex#cortex.id, Out} || Out <- nn_elements:outputs_ids(Cortex)] |
		 [[{N#neuron.id, Out} || Out <- nn_elements:outputs_ids(N)] || N <- Neurons]]),
	case InL -- OutL of
		[] -> ok;
		Diff ->
			?LOG_ERROR("Broken NN on cortex ~p with links ~p", [Cortex#cortex.id, Diff]),
			error(broken_nn)
	end.

% ......................................................................................................................
check_inputs(Cortex, Neurons) ->
	is_broken_at_inputs(Cortex),
	case lists:any(fun is_broken_at_inputs/1, Neurons) of
		false -> ok;
		true ->
			?LOG_ERROR("Broken NN on ~p neurons", [Cortex#cortex.id]),
			error(broken_nn)
	end.

is_broken_at_inputs(Element) ->
	case nn_elements:inputs_idps(Element) of
		[] ->
			?LOG_ERROR("Broken NN ~p, empty neuron inputs", [nn_elements:element_id(Element)]),
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
			?LOG_ERROR("Broken NN on ~p neurons", [Cortex#cortex.id]),
			error(broken_nn)
	end.

is_broken_at_outputs(Element) ->
	case nn_elements:outputs_ids(Element) of
		[] ->
			?LOG_ERROR("Broken NN ~p, empty neuron outputs", [nn_elements:element_id(Element)]),
			true;
		_NonEmpty ->
			false
	end.


