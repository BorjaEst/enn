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
	ToNeuron = nndb:read(ToNeuron_Id),
	case lists:keymember(FromElement_Id, 1, nn_elements:inputs_idps(ToNeuron)) of
		true ->
			nndb:write(nn_elements:edit_input_id(ToNeuron, FromElement_Id, NewWeight));
		false ->
			exit({not_member, FromElement_Id, elements:inputs_idps(ToNeuron)})
	end.

%%--------------------------------------------------------------------
%% @doc
%% % TODO: To make description
%%
%% @end
%%--------------------------------------------------------------------
% TODO: Define specs
edit_bias({_, neuron} = Neuron_Id, NewValue) ->
	Neuron = nndb:read(Neuron_Id),
	NewNeuron = elements:edit_neuron(Neuron, [{bias, NewValue}]),
	nndb:write(NewNeuron).


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
	SameElement = nndb:read(SameElement_Id),
	U1_SameElement = link_only_From(SameElement, SameElement),
	U2_SameElement = link_only_To(U1_SameElement, U1_SameElement),
	nndb:write(U2_SameElement);
create_link(FromElement_Id, ToElement_Id) ->
	[FromElement, ToElement] = nndb:read([FromElement_Id, ToElement_Id]),
	U_FromElement = link_only_From(FromElement, ToElement),
	U_ToElement = link_only_To(U_FromElement, ToElement),
	nndb:write([U_FromElement, U_ToElement]).

%%--------------------------------------------------------------------
%% @doc
%% % TODO: To make description
%%
%% @end
%%--------------------------------------------------------------------
% TODO: Define specs
remove_link(SameElement_Id, SameElement_Id) ->
	SameElement = nndb:read(SameElement_Id),
	U1_SameElement = unlink_only_From(SameElement, SameElement),
	U2_SameElement = unlink_only_To(U1_SameElement, U1_SameElement),
	nndb:write(U2_SameElement);
remove_link(FromElement_Id, ToElement_Id) ->
	[FromElement, ToElement] = nndb:read([FromElement_Id, ToElement_Id]),
	U_FromElement = unlink_only_From(FromElement, ToElement),
	U_ToElement = unlink_only_To(U_FromElement, ToElement),
	nndb:write([U_FromElement, U_ToElement]).

%%--------------------------------------------------------------------
%% @doc
%% % TODO: To make description
%%
%% @end
%%--------------------------------------------------------------------
% TODO: Define specs
change_af({_, neuron} = Neuron_Id, NewAFun) ->
	Neuron = nndb:read(Neuron_Id),
	NewNeuron = elements:edit_neuron(Neuron, [{af, NewAFun}]),
	nndb:write(NewNeuron).

%%--------------------------------------------------------------------
%% @doc
%% % TODO: To make description
%%
%% @end
%%--------------------------------------------------------------------
% TODO: Define specs
change_aggrf({_, neuron} = Neuron_Id, NewAggrFun) ->
	Neuron = nndb:read(Neuron_Id),
	NewNeuron = elements:edit_neuron(Neuron, [{aggrf, NewAggrFun}]),
	nndb:write(NewNeuron).


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
	Cortex = nndb:read(Cortex_Id),
	Neurons_Ids = maps:get(Layer, Cortex#cortex.layers, []),
	Neuron_Id = neuron:new(Layer, AFun, AggrFun, _Options = []),
	nndb:write(Cortex#cortex{
		layers = maps:put(Layer, [Neuron_Id | Neurons_Ids], Cortex#cortex.layers)
	}).

%%--------------------------------------------------------------------
%% @doc
%% % TODO: To make description
%%
%% @end
%%--------------------------------------------------------------------
% TODO: Define specs
remove_neuron({{Layer, _}, neuron} = Neuron_Id, Cortex_Id) ->
	Cortex = nndb:read(Cortex_Id),
	Neurons_Ids = maps:get(Layer, Cortex#cortex.layers),
	Neuron = nndb:read(Neuron_Id),
	[remove_link(E_Id, Neuron_Id) || E_Id <- nn_elements:inputs_ids(Neuron)],
	[remove_link(Neuron_Id, E_Id) || E_Id <- nn_elements:outputs_ids(Neuron)],
	case lists:delete(Neuron_Id, Neurons_Ids) of
		[] ->
			nndb:write(Cortex#cortex{layers = maps:remove(Layer, Cortex#cortex.layers)});
		Neurons ->
			nndb:write(Cortex#cortex{layers = maps:update(Layer, Neurons, Cortex#cortex.layers)})
	end.


%%====================================================================
%% Internal functions
%%====================================================================

%.......................................................................................................................
link_only_From(From, To) ->
	ToId = nn_elements:element_id(To),
	case lists:member(ToId, nn_elements:outputs_ids(From)) of
		false ->
			_U_From = nn_elements:add_output_id(From, ToId);
		true ->
			FromId = nn_elements:element_id(From),
			Error_Text = io_lib:format("[can not add O_Id]: ~w already a member of ~w outputs", [ToId, FromId]),
			error({link_fail, lists:flatten(Error_Text)})
	end.

%.......................................................................................................................
link_only_To(From, To) ->
	FromId = nn_elements:element_id(From),
	case lists:member(FromId, nn_elements:inputs_ids(To)) of
		false ->
			_U_To = nn_elements:add_input_id(To, FromId);
		true ->
			ToId = nn_elements:element_id(To),
			Error_Text = io_lib:format("[can not add I_Id]: ~w already a member of ~w inputs", [FromId, ToId]),
			error({link_fail, lists:flatten(Error_Text)})
	end.

%.......................................................................................................................
unlink_only_From(From, To) ->
	ToId = nn_elements:element_id(To),
	case lists:member(ToId, nn_elements:outputs_ids(From)) of
		true ->
			_U_From = nn_elements:remove_output_id(From, ToId);
		false ->
			FromId = nn_elements:element_id(From),
			Error_Text = io_lib:format("[can not remove O_Id]: ~w not a member of ~w outputs", [ToId, FromId]),
			error({unlink_fail, lists:flatten(Error_Text)})
	end.

%.......................................................................................................................
unlink_only_To(From, To) ->
	FromId = nn_elements:element_id(From),
	case lists:member(FromId, nn_elements:inputs_ids(To)) of
		true ->
			_U_To = nn_elements:remove_input_id(To, FromId);
		false ->
			ToId = nn_elements:element_id(To),
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








