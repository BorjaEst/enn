%%%-------------------------------------------------------------------
%%% File    : example_SUITE.erl
%%% Author  :
%%% Description :
%%%
%%% Created :
%%%-------------------------------------------------------------------
-module(enn_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("layers.hrl").

-define(INFO(Info), ct:log(?LOW_IMPORTANCE, "Info report: ~p", [Info])).
-define(ERROR(Error), ct:pal(?HI_IMPORTANCE, "Error report: ~p", [Error])).

-define(MAX_UNITS_PER_LAYER, 20).
-define(MAX_NUMBER_LAYERS, 4).
-define(TRAINING_LINES, 2000).
-define(DISPLAY_LINES, 10).
-define(PARALLEL_NN, 8).

% TODO: If you kill the nn sup/cortex/neuron, etc, the nn is destroyed, the nn_sup dead and is not restarted

%%--------------------------------------------------------------------
%% Function: suite() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
suite() ->
	[{timetrap, {seconds, 4}}].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_suite(Config) ->
	application:start(enn),
	Config.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
	application:stop(enn),
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
		{test_for_multiple_networks, [parallel],
		 [
			 xor_gate_static_inputs,
			 xor_gate_random_inputs,
			 addition_static_inputs,
			 addition_random_inputs,
			 weights_0_network,
			 sequence_1_input |
			 [random_dense_random_inputs || _ <- lists:seq(1, ?PARALLEL_NN - 5)] ++
			 [perturb_and_restore_weights || _ <- lists:seq(1, ?PARALLEL_NN - 5)]
		 ]},
		{test_of_error_networks, [parallel],
		 [test_for_empty_nn,
		  test_for_broken_nn]}
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
%%		addition_static_inputs,
%%		addition_random_inputs,
		xor_gate_static_inputs % GROUPS CANNOT BE DEBUGGED
%%		{group, test_for_multiple_networks}
%%		{group, test_of_error_networks}
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
xor_gate_static_inputs() ->
	[].
xor_gate_static_inputs(_Config) ->
	test_model(
		test_architectures:xor_gate(),
		fun test_data_generators:static_xor_of_inputs/3
	).

% ......................................................................................................................
xor_gate_random_inputs() ->
	[].
xor_gate_random_inputs(_Config) ->
	test_model(
		test_architectures:xor_gate(),
		fun test_data_generators:random_xor_of_inputs/3
	).

% ......................................................................................................................
addition_static_inputs() ->
	[].
addition_static_inputs(_Config) ->
	test_model(
		test_architectures:addition(),
		fun test_data_generators:static_sum_of_inputs/3
	).

% ......................................................................................................................
addition_random_inputs() ->
	[].
addition_random_inputs(_Config) ->
	test_model(
		test_architectures:addition(),
		fun test_data_generators:random_sum_of_inputs/3
	).

% ......................................................................................................................
sequence_1_input() ->
	[].
sequence_1_input(_Config) ->
	test_model(
		test_architectures:sequence(),
		fun test_data_generators:sequence_of_1_input/3
	).

% ......................................................................................................................
weights_0_network() ->
	[].
weights_0_network(_Config) ->
	test_model(
		test_architectures:network_0_weights(),
		fun test_data_generators:inputs_always_0/3
	).

% ......................................................................................................................
random_dense_random_inputs() ->
	[].
random_dense_random_inputs(_Config) ->
	test_model(
		test_architectures:random_dense(?MAX_UNITS_PER_LAYER, ?MAX_NUMBER_LAYERS),
		fun test_data_generators:random_sum_of_inputs/3
	).

% ----------------------------------------------------------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS --------------------------------------------------------------------------------------------

% ......................................................................................................................
test_model(Model, Training_Generator) ->
	{ok, Cortex_Id} = correct_model_compilation(Model),
	{ok, Cortex_PId} = correct_model_start(Cortex_Id),
	{ok, _TrainedPredictions} = correct_model_training(Cortex_Id, Cortex_PId, Training_Generator),
	ok = correct_model_stop(Cortex_Id, Cortex_PId),
	ok.

% ......................................................................................................................
correct_model_compilation(Model) ->
	?INFO("Correct model compilation ............................................................"),
	Cortex_Id = enn:compile(Model), ?INFO(Model),
	Cortex = nndb:read(Cortex_Id), ?INFO(Cortex),
	true = elements:is_cortex(Cortex),
	?INFO("____________________________________________________________________________________OK"),
	{ok, Cortex_Id}.

% ......................................................................................................................
correct_model_start(Cortex_Id) ->
	?INFO("Correct neural network start form a cortex id ........................................"),
	{ok, Cortex_PId} = enn:start_nn(Cortex_Id),
	timer:sleep(10),
	true = is_process_alive(Cortex_PId),
	?INFO("____________________________________________________________________________________OK"),
	{ok, Cortex_PId}.

% ......................................................................................................................
correct_model_training(Cortex_Id, Cortex_PId, Training_Generator) ->
	?INFO("Correct fit of model using backpropagation ..........................................."),
	{Inputs, Outputs} = Training_Generator(enn:inputs(Cortex_Id), enn:outputs(Cortex_Id), ?TRAINING_LINES),
	{Loss, Predictions} = enn:fit(Cortex_PId, Inputs, Outputs, round(?TRAINING_LINES / ?DISPLAY_LINES)),
	?INFO(Loss),
	timer:sleep(10),
	true = is_process_alive(Cortex_PId),
	
	?INFO(sumUp_list(zipLists_3(Inputs, Outputs, Predictions), round(?TRAINING_LINES / ?DISPLAY_LINES))),
	ct:pal("Results ~p",
	       [sumUp_list(zipLists_3(Inputs, Outputs, Predictions), round(?TRAINING_LINES / ?DISPLAY_LINES))]),
	ct:pal("Loss ~p", [Loss]),
	
	?INFO("____________________________________________________________________________________OK"),
	{ok, Predictions}.

correct_model_stop(Cortex_Id, Cortex_PId) ->
	?INFO("Correct neural network stop form a cortex id ........................................."),
	[Neuron_Id | _] = elements:neurons(nndb:read(Cortex_Id)),
	Neuron_BeforeTraining = nndb:read(Neuron_Id), ?INFO(Neuron_BeforeTraining),
	enn:stop_nn(Cortex_Id),
	false = is_process_alive(Cortex_PId),
	Neuron_AfterTraining = nndb:read(Neuron_Id), ?INFO(Neuron_AfterTraining),
	true = Neuron_BeforeTraining /= Neuron_AfterTraining,
	?INFO("____________________________________________________________________________________OK"),
	ok.

% ......................................................................................................................
zipLists_2(ListOfLists_1, ListOfLists_2) ->
	lists:zip(
		[[round(X * 1.0e5) / 1.0e5 || X <- List] || List <- ListOfLists_1],
		[[round(X * 1.0e5) / 1.0e5 || X <- List] || List <- ListOfLists_2]
	).

% ......................................................................................................................
zipLists_3(ListOfLists_1, ListOfLists_2, ListOfLists_3) ->
	lists:zip3(
		[[round(X * 1.0e5) / 1.0e5 || X <- List] || List <- ListOfLists_1],
		[[round(X * 1.0e5) / 1.0e5 || X <- List] || List <- ListOfLists_2],
		[[round(X * 1.0e5) / 1.0e5 || X <- List] || List <- ListOfLists_3]
	).

% ......................................................................................................................
sumUp_list(List, EachN) ->
	[X || {N, X} <- [A || A <- lists:zip(lists:seq(1, length(List)), List)], N rem EachN == 0].





