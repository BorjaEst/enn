%%%-------------------------------------------------------------------
%%% @author borja
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Sep 2018 20:14
%%%-------------------------------------------------------------------
-module(mutation).
-compile([export_all, nowarn_export_all]). %%TODO: To delete after build

-include_lib("eunit/include/eunit.hrl").

%% API
%%-export([]).

%%%===================================================================
%%% API - LEVEL 1 mutations
%%%===================================================================
%% Features:
%%      - Frequency: High
%%      - Effects: Only a link is modified

%%--------------------------------------------------------------------
%% @doc
%% % TODO: To make description
%%
%% @end
%%--------------------------------------------------------------------
% TODO: Define specs
edit_link(FromElement_Id, {_, neuron} = ToNeuron_Id, NewWeight) ->
	ToNeuron = edb:read(ToNeuron_Id),
	ToInputs = elements:inputs_ids(ToNeuron) ++ elements:rcc_inputs_ids(ToNeuron),
	case lists:member(FromElement_Id, ToInputs) of
		true ->
			edb:write(elements:edit_input_id(ToNeuron, FromElement_Id, NewWeight));
		false ->
			exit({not_member, FromElement_Id, ToInputs})
	end.

%%--------------------------------------------------------------------
%% @doc
%% % TODO: To make description
%%
%% @end
%%--------------------------------------------------------------------
% TODO: Define specs
edit_bias({_, neuron} = Neuron_Id, NewValue) ->
	Neuron = edb:read(Neuron_Id),
	NewNeuron = elements:edit(Neuron, [{bias, NewValue}]),
	edb:write(NewNeuron).


%%%===================================================================
%%% API - LEVEL 2 mutations
%%%===================================================================
%% Features:
%%      - Frequency: medium
%%      - Effects: Creates, removes connections and modifies neuron functions

%%--------------------------------------------------------------------
%% @doc
%% % TODO: To make description
%%
%% @end
%%--------------------------------------------------------------------
% TODO: Define specs
create_link(SameElement_Id, SameElement_Id) ->
	SameElement = edb:read(SameElement_Id),
	U1_SameElement = link_only_From(SameElement, SameElement),
	U2_SameElement = link_only_To(U1_SameElement, U1_SameElement),
	edb:write(U2_SameElement);
create_link(FromElement_Id, ToElement_Id) ->
	[FromElement, ToElement] = edb:read([FromElement_Id, ToElement_Id]),
	U_FromElement = link_only_From(FromElement, ToElement),
	U_ToElement = link_only_To(U_FromElement, ToElement),
	edb:write([U_FromElement, U_ToElement]).

%%--------------------------------------------------------------------
%% @doc
%% % TODO: To make description
%%
%% @end
%%--------------------------------------------------------------------
% TODO: Define specs
remove_link(SameElement_Id, SameElement_Id) ->
	SameElement = edb:read(SameElement_Id),
	U1_SameElement = unlink_only_From(SameElement, SameElement),
	U2_SameElement = unlink_only_To(U1_SameElement, U1_SameElement),
	edb:write(U2_SameElement);
remove_link(FromElement_Id, ToElement_Id) ->
	[FromElement, ToElement] = edb:read([FromElement_Id, ToElement_Id]),
	U_FromElement = unlink_only_From(FromElement, ToElement),
	U_ToElement = unlink_only_To(U_FromElement, ToElement),
	edb:write([U_FromElement, U_ToElement]).

%%--------------------------------------------------------------------
%% @doc
%% % TODO: To make description
%%
%% @end
%%--------------------------------------------------------------------
% TODO: Define specs
change_af({_, neuron} = Neuron_Id, NewAFun) ->
	Neuron = edb:read(Neuron_Id),
	NewNeuron = elements:edit(Neuron, [{af, NewAFun}]),
	edb:write(NewNeuron).

%%--------------------------------------------------------------------
%% @doc
%% % TODO: To make description
%%
%% @end
%%--------------------------------------------------------------------
% TODO: Define specs
change_aggrf({_, neuron} = Neuron_Id, NewAggrFun) ->
	Neuron = edb:read(Neuron_Id),
	NewNeuron = elements:edit(Neuron, [{aggrf, NewAggrFun}]),
	edb:write(NewNeuron).


%%%===================================================================
%%% API - LEVEL 3 mutations
%%%===================================================================
%% Features:
%%      - Frequency: Low
%%      - Effects: Changes the architecture

%%--------------------------------------------------------------------
%% @doc
%% % TODO: To make description
%%
%% @end
%%--------------------------------------------------------------------
% TODO: Define specs
insert_neuron(Layer, AFun, AggrFun, Cortex_Id) ->
	Cortex = edb:read(Cortex_Id),
	Neurons_Ids = maps:get(Layer, elements:layers(Cortex), []),
	Neuron_Id = neuron:new(Layer, AFun, AggrFun, _Options = []),
	NewLayer = maps:put(Layer, [Neuron_Id | Neurons_Ids], elements:layers(Cortex)),
	NewCortex = elements:edit(Cortex, [{layers,NewLayer}]),
	edb:write(NewCortex).

%%--------------------------------------------------------------------
%% @doc
%% % TODO: To make description
%%
%% @end
%%--------------------------------------------------------------------
% TODO: Define specs
remove_neuron({{Layer, _}, neuron} = Neuron_Id, Cortex_Id) ->
	Cortex = edb:read(Cortex_Id),
	Neurons_Ids = maps:get(Layer, elements:layers(Cortex)),
	Neuron = edb:read(Neuron_Id),
	[remove_link(E_Id, Neuron_Id) || E_Id <- elements:inputs_ids(Neuron) ++ elements:rcc_inputs_ids(Neuron)],
	[remove_link(Neuron_Id, E_Id) || E_Id <- elements:outputs_ids(Neuron) ++ elements:rcc_outputs_ids(Neuron)],
	NewCortex = case lists:delete(Neuron_Id, Neurons_Ids) of
		[] ->
			NewLayer = maps:remove(Layer, elements:layers(Cortex)),
			elements:edit(Cortex, [{layer, NewLayer}]);
		Neurons ->
			NewLayer = maps:update(Layer, Neurons, elements:layers(Cortex)),
			elements:edit(Cortex, [{layer, NewLayer}])
	end,
	edb:write(NewCortex).


%%====================================================================
%% Internal functions
%%====================================================================

%.......................................................................................................................
link_only_From(From, To) ->
	ToId = elements:id(To),
	case lists:member(ToId, elements:outputs_ids(From) ++ elements:rcc_outputs_ids(From)) of
		false ->
			_U_From = elements:add_output_id(From, ToId);
		true ->
			FromId = elements:id(From),
			Error_Text = io_lib:format("[can not add O_Id]: ~w already a member of ~w outputs", [ToId, FromId]),
			error({link_fail, lists:flatten(Error_Text)})
	end.

%.......................................................................................................................
link_only_To(From, To) ->
	FromId = elements:id(From),
	case lists:member(FromId, elements:inputs_ids(To) ++ elements:rcc_inputs_ids(To)) of
		false ->
			_U_To = elements:add_input_id(To, FromId);
		true ->
			ToId = elements:id(To),
			Error_Text = io_lib:format("[can not add I_Id]: ~w already a member of ~w inputs", [FromId, ToId]),
			error({link_fail, lists:flatten(Error_Text)})
	end.

%.......................................................................................................................
unlink_only_From(From, To) ->
	ToId = elements:id(To),
	case lists:member(ToId, elements:outputs_ids(From) ++ elements:rcc_outputs_ids(From)) of
		true ->
			_U_From = elements:remove_output_id(From, ToId);
		false ->
			FromId = elements:id(From),
			Error_Text = io_lib:format("[can not remove O_Id]: ~w not a member of ~w outputs", [ToId, FromId]),
			error({unlink_fail, lists:flatten(Error_Text)})
	end.

%.......................................................................................................................
unlink_only_To(From, To) ->
	FromId = elements:id(From),
	case lists:member(FromId, elements:inputs_ids(To) ++ elements:rcc_inputs_ids(To)) of
		true ->
			_U_To = elements:remove_input_id(To, FromId);
		false ->
			ToId = elements:id(To),
			Error_Text = io_lib:format("[can not remove I_Id]: ~w not a member of ~w intputs", [FromId, ToId]),
			error({unlink_fail, lists:flatten(Error_Text)})
	end.

%.......................................................................................................................
next_of(_Element, [], If_Last)                      -> If_Last;
next_of(Element, [Next | _], _) when Element < Next -> Next;
next_of(Element, [_ | Rest], If_Last)               -> next_of(Element, Rest, If_Last).

%.......................................................................................................................
prev_of(Element, List, If_First) -> prev_of2(Element, lists:reverse(List), If_First).

prev_of2(_Element, [], If_Last)                      -> If_Last;
prev_of2(Element, [Next | _], _) when Element > Next -> Next;
prev_of2(Element, [_ | Rest], If_Last)               -> prev_of2(Element, Rest, If_Last).

%%====================================================================
%% Eunit white box tests
%%====================================================================

% ----------------------------------------------------------------------------------------------------------------------
% TESTS DESCRIPTIONS ---------------------------------------------------------------------------------------------------
eunit_example_test_() ->
	% {setup, Where, Setup, Cleanup, Tests | Instantiator}
	[
		{"Eunit example test",
		 {setup, local, fun no_setup/0, fun no_cleanup/1, fun test_example/1}}
	].

% ----------------------------------------------------------------------------------------------------------------------
% SPECIFIC SETUP FUNCTIONS ---------------------------------------------------------------------------------------------
no_setup() ->
	ok.

no_cleanup(_) ->
	ok.

% ----------------------------------------------------------------------------------------------------------------------
% ACTUAL TESTS ---------------------------------------------------------------------------------------------------------
test_example(_) ->
	True = true,
	[
		?_assert(True)
	].

% ----------------------------------------------------------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS --------------------------------------------------------------------------------------------








