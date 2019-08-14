%%%-------------------------------------------------------------------
%%% @author borja
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Aug 2018 14:25
%%%-------------------------------------------------------------------
-module(nn_elements).
-compile([export_all, nowarn_export_all]). %% TODO: To delete after build

-include_lib("eunit/include/eunit.hrl").
-include_lib("nnelements.hrl").

-define(MIN_LINK_WEIGHT, 0.05).

%% API
%%-export([]).

-ifdef(debug_mode).
-define(LOG(X), io:format("{~p,~p,~p}: ~p~n", [self(), ?MODULE, ?LINE, X])).
-define(STDCALL_TIMEOUT, infinity).
-else.
-define(LOG(X), true).
-define(STDCALL_TIMEOUT, 5000).
-endif.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
create_neuron(Layer, AF, AggrF, Options) ->
	Neuron = #neuron{id = ?NEW_NEURON_ID(Layer), af = AF, aggrf = AggrF},
	write_neuron_options(Neuron, Options).

write_neuron_options(Neuron, [{id, Value} | Options]) ->
	write_neuron_options(Neuron#neuron{id = Value}, Options);
write_neuron_options(Neuron, [{bias, Value} | Options]) ->
	write_neuron_options(Neuron#neuron{bias = Value}, Options);
write_neuron_options(Neuron, [{inputs_idps, Value} | Options]) ->
	write_neuron_options(Neuron#neuron{inputs_idps = Value}, Options);
write_neuron_options(Neuron, [{outputs_ids, Value} | Options]) ->
	write_neuron_options(Neuron#neuron{outputs_ids = Value}, Options);
write_neuron_options(Neuron, [{rcc_inputs_idps, Value} | Options]) ->
	write_neuron_options(Neuron#neuron{rcc_inputs_idps = Value}, Options);
write_neuron_options(Neuron, [{rcc_outputs_ids, Value} | Options]) ->
	write_neuron_options(Neuron#neuron{rcc_outputs_ids = Value}, Options);
write_neuron_options(Neuron, []) ->
	Neuron.

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
create_cortex(Name, CompiledLayers, Options) ->
	Cortex = #cortex{id = ?NEW_CORTEX_ID(Name), layers = CompiledLayers},
	write_cortex_options(Cortex, Options).

write_cortex_options(Cortex, []) ->
	Cortex.

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
% TODO: To make description and specs
neurons(Cortex) ->
	lists:append(maps:values(Cortex#cortex.layers)).

neurons(Cortex, hidden) ->
	[_ | HiddenLayers] = lists:droplast(maps:values(Cortex#cortex.layers)),
	lists:append(HiddenLayers);
neurons(Cortex, Layer) ->
	maps:get(Layer, Cortex#cortex.layers).

%%-------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
% TODO: To make description and specs
layers(Cortex) ->
	maps:keys(Cortex#cortex.layers).

%%-------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
% TODO: To make description and specs
links([Neuron | Neurons]) ->
	[{In, Neuron#neuron.id} || In <- inputs_ids(Neuron)] ++ links(Neurons);
links([]) ->
	[].


%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
% TODO: To make description and specs
size(Cortex) ->
	lists:sum([length(Neurons) || Neurons <- maps:values(Cortex#cortex.layers)]).

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
element_id(Element) when is_record(Element, neuron) ->
	Element#neuron.id;
element_id(Element) when is_record(Element, cortex) ->
	Element#cortex.id.

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
layerIndex({{LI, _}, neuron}) -> LI;
layerIndex({_, cortex})       -> cortex;
layerIndex(Element)           -> layerIndex(element_id(Element)).

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
outputs_ids(Element) when is_record(Element, neuron) ->
	Element#neuron.outputs_ids ++ Element#neuron.rcc_outputs_ids;
outputs_ids(Element) when is_record(Element, cortex) ->
	Element#cortex.outputs_ids.

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
inputs_ids(Element) ->
	[Id || {Id, _W} <- inputs_idps(Element)].

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
inputs_idps(Element) when is_record(Element, neuron) ->
	Element#neuron.inputs_idps ++ Element#neuron.rcc_inputs_idps;
inputs_idps(Element) when is_record(Element, cortex) ->
	Element#cortex.inputs_idps.

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
add_output_id(Element, ToId) when is_record(Element, neuron) ->
	FromLI = layerIndex(Element),
	ToLI = layerIndex(ToId),
	if
		FromLI >= ToLI ->
			Element#neuron{rcc_outputs_ids = [ToId | Element#neuron.rcc_outputs_ids]};
		true ->
			Element#neuron{outputs_ids = [ToId | Element#neuron.outputs_ids]}
	end;
add_output_id(Element, ToId) when is_record(Element, cortex) ->
	Element#cortex{outputs_ids = [ToId | Element#cortex.outputs_ids]}.

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
remove_output_id(Element, ToId) when is_record(Element, neuron) ->
	Element#neuron{outputs_ids     = lists:delete(ToId, Element#neuron.outputs_ids),
	               rcc_outputs_ids = lists:delete(ToId, Element#neuron.rcc_outputs_ids)};
remove_output_id(Element, ToId) when is_record(Element, cortex) ->
	Element#cortex{outputs_ids = lists:delete(ToId, Element#cortex.outputs_ids)}.

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
add_input_id(Element, FromId) when is_record(Element, neuron) ->
	FromLI = layerIndex(FromId),
	ToLI = layerIndex(Element),
	W = ?DELTA_MULTIPLIER * (rand:uniform() - 0.5),
	if
		FromLI < ToLI ->
			Element#neuron{inputs_idps = [{FromId, W} | Element#neuron.inputs_idps]};
		FromLI == cortex ->
			Element#neuron{inputs_idps = [{FromId, cortex} | Element#neuron.inputs_idps]};
		true ->
			Element#neuron{rcc_inputs_idps = [{FromId, W} | Element#neuron.rcc_inputs_idps]}
	end;
add_input_id(Element, FromId) when is_record(Element, cortex) ->
	Element#cortex{inputs_idps = [{FromId, _Weight = 1.0} | Element#cortex.inputs_idps]}.

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
remove_input_id(Element, FromId) when is_record(Element, neuron) ->
	Element#neuron{inputs_idps     = lists:keydelete(FromId, 1, Element#neuron.inputs_idps),
	               rcc_inputs_idps = lists:keydelete(FromId, 1, Element#neuron.rcc_inputs_idps)};
remove_input_id(Element, FromId) when is_record(Element, cortex) ->
	Element#cortex{inputs_idps = lists:keydelete(FromId, 1, Element#cortex.inputs_idps)}.

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
edit_input_id(Element, FromId, W) when is_record(Element, neuron) ->
	Element#neuron{inputs_idps     = lists:keyreplace(FromId, 1, Element#neuron.inputs_idps, {FromId, W}),
	               rcc_inputs_idps = lists:keyreplace(FromId, 1, Element#neuron.rcc_inputs_idps, {FromId, W})}.

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Make specs
clone_cortex(Cortex) ->
	clone_cortex(Cortex, nnref:new()).

clone_cortex(Cortex, Name) ->
	CloneCx_Id = ?NEW_CORTEX_ID(Name), % Preparation for the clonation
	ConversionETS = ets:new(idsNcloneids, [set, private]),
	ets:insert(ConversionETS, {Cortex#cortex.id, CloneCx_Id}),
	{  Cortex#cortex{
		id          = CloneCx_Id,
		layers      = map_layers(Cortex#cortex.layers, ConversionETS),
		inputs_idps = map_inputs_idps(Cortex#cortex.inputs_idps, ConversionETS),
		outputs_ids = map_outputs_ids(Cortex#cortex.outputs_ids, ConversionETS)
	}, ConversionETS}. % FIRST map the elements (over IdsNCloneIds) and build the new cortex

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Make specs
clone_neuron(Neuron, ConversionETS) ->
	Neuron#neuron{
		id              = ets:lookup_element(ConversionETS, Neuron#neuron.id, 2),
		inputs_idps     = map_inputs_idps(Neuron#neuron.inputs_idps, ConversionETS),
		outputs_ids     = map_outputs_ids(Neuron#neuron.outputs_ids, ConversionETS),
		rcc_inputs_idps = map_inputs_idps(Neuron#neuron.rcc_inputs_idps, ConversionETS),
		rcc_outputs_ids = map_outputs_ids(Neuron#neuron.rcc_outputs_ids, ConversionETS)
	}.

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
pformat(Element) when is_record(Element, neuron) -> pformat(Element, record_info(fields, neuron));
pformat(Element) when is_record(Element, cortex) -> pformat(Element, record_info(fields, cortex)).

pformat(Element, Fields) ->
	[io_lib:format("Element record: ~w ~n", [element(1, Element)]) |
	 pformat(Element, Fields, 2)].

pformat(Element, [Field | ListOf_Fields], Index) ->
	[io_lib:format(" --> ~w = ~w ~n", [Field, element(Index, Element)]) |
	 pformat(Element, ListOf_Fields, Index + 1)];
pformat(_Element, [], _Index) ->
	[].

%%====================================================================
%% Internal functions
%%====================================================================

%.......................................................................................................................
map_layers(Layers, ConversionETS) ->
	LayersList = maps:to_list(Layers),
	NewLayerList = [{Layer, map_neurons_ids(Neurons_Ids, ConversionETS)} || {Layer, Neurons_Ids} <- LayersList],
	_NewLayer = maps:from_list(NewLayerList).

map_neurons_ids(Neurons_Ids, ConversionETS) ->
	[map_neuron_id(Neuron_Id, ConversionETS) || Neuron_Id <- Neurons_Ids].

map_neuron_id(Id, ConversionETS) ->
	{{LayerIndex, _NumId}, Type} = Id,
	CloneId = {{LayerIndex, nnref:new()}, Type},
	ets:insert(ConversionETS, {Id, CloneId}),
	CloneId.

% ......................................................................................................................
map_inputs_idps(Inputs_IdPs, ConversionETS) ->
	[{ets:lookup_element(ConversionETS, I_Id, 2), W} || {I_Id, W} <- Inputs_IdPs].

% ......................................................................................................................
map_outputs_ids(Outputs_Ids, ConversionETS) ->
	[ets:lookup_element(ConversionETS, O_Id, 2) || O_Id <- Outputs_Ids].


%%====================================================================
%% Eunit white box tests
%%====================================================================

% ----------------------------------------------------------------------------------------------------------------------
% TESTS DESCRIPTIONS ---------------------------------------------------------------------------------------------------
print_elements_test_() ->
	% {setup, Where, Setup, Cleanup, Tests | Instantiator}
	[
		{"It is possible to print a neuron correctly using ?LOG_INFO",
		 {setup, local, fun no_setup/0, fun no_cleanup/1, fun test_for_print_neuron/1}}
%%		{"It is possible to print all record types correctly using ?LOG_INFO",
%%		 {setup, local, fun no_setup/0, fun no_cleanup/1, fun test_for_print_all/1}}
	].


% ----------------------------------------------------------------------------------------------------------------------
% SPECIFIC SETUP FUNCTIONS ---------------------------------------------------------------------------------------------
no_setup() ->
	ok.

no_cleanup(_) ->
	ok.

% ----------------------------------------------------------------------------------------------------------------------
% ACTUAL TESTS ---------------------------------------------------------------------------------------------------------
test_for_print_neuron(_) ->
	Element = #neuron{bias = 0.0},
	TestListToPrint = lists:append([
		                               "Element record: neuron \n",
		                               " --> id = undefined \n",
		                               " --> af = undefined \n",
		                               " --> aggrf = undefined \n",
		                               " --> bias = 0.0 \n",
		                               " --> inputs_idps = [] \n",
		                               " --> outputs_ids = [] \n",
		                               " --> rcc_inputs_idps = [] \n",
		                               " --> rcc_outputs_ids = [] \n"
	                               ]),
	{inorder, [
		?_assertEqual(TestListToPrint, lists:flatten(pformat(Element)))
	]}.

%%test_for_print_all(_) ->
%%	EList = [#input{}, #output{}, #neuron{}, #cortex{}],
%%	lazy_for_print_all(EList).

% ----------------------------------------------------------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS --------------------------------------------------------------------------------------------

%%lazy_for_print_all([Element | T]) ->
%%	{generator, fun() -> [?_assert(print_aux(Element)) | lazy_for_print_all(T)] end};
%%lazy_for_print_all([]) ->
%%	[].
