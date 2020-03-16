%%%-------------------------------------------------------------------
%%% File    : example_SUITE.erl
%%% Author  :
%%% Description :
%%%
%%% Created :
%%%-------------------------------------------------------------------
-module(mutations_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").

-define(INFO(Info), ct:log(?LOW_IMPORTANCE, "Info report: ~p", [Info])).
-define(ERROR(Error), ct:pal(?HI_IMPORTANCE, "Error report: ~p", [Error])).

-define(MAX_UNITS_PER_LAYER, 20).
-define(MAX_NUMBER_OF_LAYERS, 4).

%%--------------------------------------------------------------------
%% Function: suite() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
suite() ->
	[{timetrap, {seconds, 30}}].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_suite(Config) ->
	application:start(neurnet),
	Model = test_architectures:random_dense(?MAX_UNITS_PER_LAYER, ?MAX_NUMBER_OF_LAYERS),
	Cortex_Id = enn:compile(Model),
	% TODO: Test with different types of models
	_NewConfig = [{cortex_id, Cortex_Id} | Config].

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
	application:stop(neurnet),
	ok.

%%--------------------------------------------------------------------
%% Function: init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
	Config.

%%--------------------------------------------------------------------
%% Function: end_per_group(GroupName, Config0) ->
%%               term() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
	ok.

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_testcase(_GroupName, Config) ->
	Config.

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config0) ->
%%               term() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
	ok.

%%--------------------------------------------------------------------
%% Function: groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%%--------------------------------------------------------------------
groups() ->
	[
		{test_for_level_1_mutations, [],
		 [
			 test_for_level_1_mutations_neurons
		 ]},
		{test_for_level_2_mutations, [],
		 [
			 test_for_level_2_mutations_neurons
		 ]}
	].

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%%--------------------------------------------------------------------
all() ->
	[
		{group, test_for_level_1_mutations},
		{group, test_for_level_2_mutations}
%%		{group, test_for_level_3_mutations},
%%		{group, test_for_level_4_mutations},
%%		{group, test_for_level_5_mutations}
	].

%%--------------------------------------------------------------------
%% Function: TestCase() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
my_test_case_example() ->
	[].

%%--------------------------------------------------------------------
%% Function: TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%%--------------------------------------------------------------------
my_test_case_example(_Config) ->
	ok.

% --------------------------------------------------------------------
% TESTS --------------------------------------------------------------

% ......................................................................................................................
test_for_level_1_mutations_neurons() ->
	[].
test_for_level_1_mutations_neurons(Config) ->
	Cortex_Id = ?config(cortex_id, Config),
	Cortex = edb:read(Cortex_Id),
	[test_for_level_1_edit_link_neuron(Neuron_Id) || Neuron_Id <- elements:neurons(Cortex)],
	[test_for_level_1_edit_bias_neuron(Neuron_Id) || Neuron_Id <- elements:neurons(Cortex)],
	ok.

test_for_level_1_edit_link_neuron(Neuron_Id) ->
	?INFO("Correct weigth neuron mutations ......................................................"),
	N_0 = edb:read(Neuron_Id),
	{LinkedInput_Id, _W_0} = randomElement(elements:inputs_idps(N_0) ++ elements:rcc_inputs_idps(N_0)),
	mutation:edit_link(LinkedInput_Id, Neuron_Id, W_1 = rand:uniform()),
	N_1 = edb:read(Neuron_Id),
	?INFO(lists:flatten(enn:pformat(N_0))),
	?INFO(lists:flatten(enn:pformat(N_1))),
	{LinkedInput_Id, W_1} = lists:keyfind(LinkedInput_Id, 1, elements:inputs_idps(N_1) ++ elements:rcc_inputs_idps(N_1)),
	?INFO("____________________________________________________________________________________OK"),
	ok.

test_for_level_1_edit_bias_neuron(Neuron_Id) ->
	?INFO("Correct bias neuron mutations ........................................................"),
	N_0 = edb:read(Neuron_Id),
	_Bias_0 = elements:bias(N_0),
	mutation:edit_bias(Neuron_Id, Bias_1 = rand:uniform()),
	N_1 = edb:read(Neuron_Id),
	?INFO(lists:flatten(enn:pformat(N_0))),
	?INFO(lists:flatten(enn:pformat(N_1))),
	Bias_1 = elements:bias(N_1),
	?INFO("____________________________________________________________________________________OK"),
	ok.

% ......................................................................................................................
test_for_level_2_mutations_neurons() ->
	[].
test_for_level_2_mutations_neurons(Config) ->
	Cortex_Id = ?config(cortex_id, Config),
	Cortex = edb:read(Cortex_Id),
	Neurons_Ids = elements:neurons(Cortex),
	[test_for_level_2_create_link_neuron(Neuron_Id, Cortex) || Neuron_Id <- Neurons_Ids],
	[test_for_level_2_remove_link_neuron(Neuron_Id, Cortex) || Neuron_Id <- Neurons_Ids],
	[test_for_level_2_change_af_neuron(Neuron_Id) || Neuron_Id <- Neurons_Ids],
	[test_for_level_2_change_aggrf_neuron(Neuron_Id) || Neuron_Id <- Neurons_Ids],
	ok.

test_for_level_2_create_link_neuron(Neuron_Id, Cortex) ->
	?INFO("Correct link with random neuron (not linked already) ................................"),
	N_0 = edb:read(Neuron_Id),
	PossibleToLink = elements:neurons(Cortex) -- elements:outputs_ids(N_0) ++ elements:rcc_outputs_ids(N_0),
	ToLink_Id = randomElement(PossibleToLink),
	E_0 = edb:read(ToLink_Id),
	false = lists:member(Neuron_Id, elements:inputs_ids(E_0) ++ elements:rcc_inputs_ids(E_0)),
	mutation:create_link(Neuron_Id, ToLink_Id),
	N_1 = edb:read(Neuron_Id),
	E_1 = edb:read(ToLink_Id),
	?INFO(lists:flatten(enn:pformat(N_0))),
	?INFO(lists:flatten(enn:pformat(N_1))),
	?INFO(lists:flatten(enn:pformat(E_0))),
	?INFO(lists:flatten(enn:pformat(E_1))),
	true = lists:member(ToLink_Id, elements:outputs_ids(N_1) ++ elements:rcc_outputs_ids(N_1)),
	true = lists:member(Neuron_Id, elements:inputs_ids(E_1) ++ elements:rcc_inputs_ids(E_1)),
	?INFO("Error when link with an already linked random neuron/output ........................"),
	AlreadyLink_Id = randomElement(elements:outputs_ids(N_1) ++ elements:rcc_outputs_ids(N_1)),
	?INFO(lists:flatten(enn:pformat(N_1))),
	?INFO(AlreadyLink_Id),
	{'EXIT', _} = (catch mutation:create_link(Neuron_Id, AlreadyLink_Id)),
	?INFO("____________________________________________________________________________________OK"),
	ok.

test_for_level_2_remove_link_neuron(Neuron_Id, Cortex) ->
	?INFO("Correct unlink from random neurons (linked already) .................................."),
	N_0 = edb:read(Neuron_Id),
	ToUnlink_Id = randomElement(elements:outputs_ids(N_0) ++ elements:rcc_outputs_ids(N_0)),
	E_0 = edb:read(ToUnlink_Id),
	true = lists:member(Neuron_Id, elements:inputs_ids(E_0) ++ elements:rcc_inputs_ids(E_0)),
	mutation:remove_link(Neuron_Id, ToUnlink_Id),
	N_1 = edb:read(Neuron_Id),
	E_1 = edb:read(ToUnlink_Id),
	?INFO(lists:flatten(enn:pformat(N_0))),
	?INFO(lists:flatten(enn:pformat(N_1))),
	?INFO(lists:flatten(enn:pformat(E_0))),
	?INFO(lists:flatten(enn:pformat(E_1))),
	false = lists:member(ToUnlink_Id, elements:outputs_ids(N_1) ++ elements:rcc_outputs_ids(N_1)),
	false = lists:member(Neuron_Id, elements:inputs_ids(E_1) ++ elements:rcc_inputs_ids(E_1)),
	?INFO("Error when unlink from a non linked random neuron ...................................."),
	PossibleToLink = elements:neurons(Cortex) -- elements:outputs_ids(N_0) ++ elements:rcc_outputs_ids(N_0),
	NonLinked_Id = randomElement(PossibleToLink),
	?INFO(lists:flatten(enn:pformat(N_1))),
	?INFO(NonLinked_Id),
	{'EXIT', _} = (catch mutation:remove_link(Neuron_Id, NonLinked_Id)),
	?INFO("____________________________________________________________________________________OK"),
	ok.

test_for_level_2_change_af_neuron(Neuron_Id) ->
	?INFO("Change neuron activation function ...................................................."),
	N_0 = edb:read(Neuron_Id),
	AFList = [fun activation_function:sigmoid/1, fun activation_function:tanh/1],
	mutation:change_af(Neuron_Id, RandomAF = randomElement(AFList -- [elements:af(N_0)])),
	N_1 = edb:read(Neuron_Id),
	?INFO(lists:flatten(enn:pformat(N_0))),
	?INFO(lists:flatten(enn:pformat(N_1))),
	RandomAF = elements:af(N_1),
	?INFO("____________________________________________________________________________________OK"),
	ok.

test_for_level_2_change_aggrf_neuron(Neuron_Id) ->
	?INFO("Change neuron aggregation function ..................................................."),
	N_0 = edb:read(Neuron_Id),
	AggrFList = [fun aggregation_function:diff_product/2, fun aggregation_function:dot_power/2],
	mutation:change_aggrf(Neuron_Id, RandomAggrF = randomElement(AggrFList -- [elements:aggrf(N_0)])),
	N_1 = edb:read(Neuron_Id),
	?INFO(lists:flatten(enn:pformat(N_0))),
	?INFO(lists:flatten(enn:pformat(N_1))),
	RandomAggrF = elements:aggrf(N_1),
	?INFO("____________________________________________________________________________________OK"),
	ok.


% TODO: Implement Level 3, 4 and 5

% ----------------------------------------------------------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS --------------------------------------------------------------------------------------------

% ......................................................................................................................
randomElement(List) -> lists:nth(rand:uniform(length(List)), List).

% ......................................................................................................................
randomElements(List, Per) -> [X || {Rand, X} <- [{rand:uniform(), N} || N <- List], Rand > Per].













