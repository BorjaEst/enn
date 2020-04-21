%%%-------------------------------------------------------------------
%%% File    : example_SUITE.erl
%%% Author  :
%%% Description :
%%%
%%% Created :
%%%-------------------------------------------------------------------
-module(transform_SUITE).
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
    Cortex_id = enn:compile(Model),
    % TODO: Test with different types of models
    _NewConfig = [{cortex_id, Cortex_id} | Config].

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
        {test_for_level_1_transforms, [],
         [
             test_for_level_1_transforms_neurons
         ]},
        {test_for_level_2_transforms, [],
         [
             test_for_level_2_transforms_neurons
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
        {group, test_for_level_1_transforms},
        {group, test_for_level_2_transforms}
%%        {group, test_for_level_3_transforms},
%%        {group, test_for_level_4_transforms},
%%        {group, test_for_level_5_transforms}
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

% -------------------------------------------------------------------
test_for_level_1_transforms_neurons() ->
    [].
test_for_level_1_transforms_neurons(Config) ->
    Cortex_id = ?config(cortex_id, Config),
    Cortex = edb:read(Cortex_id),
    [test_for_level_1_edit_link_neuron(Neuron_id) || Neuron_id <- elements:neurons(Cortex)],
    [test_for_level_1_edit_bias_neuron(Neuron_id) || Neuron_id <- elements:neurons(Cortex)],
    ok.

test_for_level_1_edit_link_neuron(Neuron_id) ->
    ?INFO("Correct weigth neuron transforms ......................................................"),
    N_0 = edb:read(Neuron_id),
    {LinkedInput_id, _W_0} = randomElement(elements:inputs_idps(N_0)),
    transform:edit_link(LinkedInput_id, Neuron_id, W_1 = rand:uniform()),
    N_1 = edb:read(Neuron_id),
    ?INFO(lists:flatten(enn:pformat(N_0))),
    ?INFO(lists:flatten(enn:pformat(N_1))),
    {LinkedInput_id, W_1} = lists:keyfind(LinkedInput_id, 1, elements:inputs_idps(N_1)),
    ?INFO("____________________________________________________________________________________OK"),
    ok.

test_for_level_1_edit_bias_neuron(Neuron_id) ->
    ?INFO("Correct bias neuron transforms ........................................................"),
    N_0 = edb:read(Neuron_id),
    _Bias_0 = elements:bias(N_0),
    transform:edit_bias(Neuron_id, Bias_1 = rand:uniform()),
    N_1 = edb:read(Neuron_id),
    ?INFO(lists:flatten(enn:pformat(N_0))),
    ?INFO(lists:flatten(enn:pformat(N_1))),
    Bias_1 = elements:bias(N_1),
    ?INFO("____________________________________________________________________________________OK"),
    ok.

% -------------------------------------------------------------------
test_for_level_2_transforms_neurons() ->
    [].
test_for_level_2_transforms_neurons(Config) ->
    Cortex_id = ?config(cortex_id, Config),
    Cortex = edb:read(Cortex_id),
    Neurons_ids = elements:neurons(Cortex),
    [test_for_level_2_create_link_neuron(Neuron_id, Cortex) || Neuron_id <- Neurons_ids],
    [test_for_level_2_remove_link_neuron(Neuron_id, Cortex) || Neuron_id <- Neurons_ids],
    [test_for_level_2_change_af_neuron(Neuron_id) || Neuron_id <- Neurons_ids],
    [test_for_level_2_change_aggrf_neuron(Neuron_id) || Neuron_id <- Neurons_ids],
    ok.

test_for_level_2_create_link_neuron(Neuron_id, Cortex) ->
    ?INFO("Correct link with random neuron (not linked already) ................................"),
    N_0 = edb:read(Neuron_id),
    PossibleToLink = elements:neurons(Cortex) -- elements:outputs_ids(N_0),
    ToLink_id = randomElement(PossibleToLink),
    E_0 = edb:read(ToLink_id),
    false = lists:member(Neuron_id, elements:inputs_ids(E_0)),
    transform:create_link(Neuron_id, ToLink_id),
    N_1 = edb:read(Neuron_id),
    E_1 = edb:read(ToLink_id),
    ?INFO(lists:flatten(enn:pformat(N_0))),
    ?INFO(lists:flatten(enn:pformat(N_1))),
    ?INFO(lists:flatten(enn:pformat(E_0))),
    ?INFO(lists:flatten(enn:pformat(E_1))),
    true = lists:member(ToLink_id, elements:outputs_ids(N_1)),
    true = lists:member(Neuron_id, elements:inputs_ids(E_1)),
    ?INFO("Error when link with an already linked random neuron/output ........................"),
    AlreadyLink_id = randomElement(elements:outputs_ids(N_1)),
    ?INFO(lists:flatten(enn:pformat(N_1))),
    ?INFO(AlreadyLink_id),
    {'EXIT', _} = (catch transform:create_link(Neuron_id, AlreadyLink_id)),
    ?INFO("____________________________________________________________________________________OK"),
    ok.

test_for_level_2_remove_link_neuron(Neuron_id, Cortex) ->
    ?INFO("Correct unlink from random neurons (linked already) .................................."),
    N_0 = edb:read(Neuron_id),
    ToUnlink_id = randomElement(elements:outputs_ids(N_0)),
    E_0 = edb:read(ToUnlink_id),
    true = lists:member(Neuron_id, elements:inputs_ids(E_0)),
    transform:remove_link(Neuron_id, ToUnlink_id),
    N_1 = edb:read(Neuron_id),
    E_1 = edb:read(ToUnlink_id),
    ?INFO(lists:flatten(enn:pformat(N_0))),
    ?INFO(lists:flatten(enn:pformat(N_1))),
    ?INFO(lists:flatten(enn:pformat(E_0))),
    ?INFO(lists:flatten(enn:pformat(E_1))),
    false = lists:member(ToUnlink_id, elements:outputs_ids(N_1)),
    false = lists:member(Neuron_id, elements:inputs_ids(E_1)),
    ?INFO("Error when unlink from a non linked random neuron ...................................."),
    PossibleToLink = elements:neurons(Cortex) -- elements:outputs_ids(N_0),
    NonLinked_id = randomElement(PossibleToLink),
    ?INFO(lists:flatten(enn:pformat(N_1))),
    ?INFO(NonLinked_id),
    {'EXIT', _} = (catch transform:remove_link(Neuron_id, NonLinked_id)),
    ?INFO("____________________________________________________________________________________OK"),
    ok.

test_for_level_2_change_af_neuron(Neuron_id) ->
    ?INFO("Change neuron activation function ...................................................."),
    N_0 = edb:read(Neuron_id),
    AFList = [fun activation_function:sigmoid/1, fun activation_function:tanh/1],
    transform:change_activation(Neuron_id, RandomAF = randomElement(AFList -- [elements:activation(N_0)])),
    N_1 = edb:read(Neuron_id),
    ?INFO(lists:flatten(enn:pformat(N_0))),
    ?INFO(lists:flatten(enn:pformat(N_1))),
    RandomAF = elements:activation(N_1),
    ?INFO("____________________________________________________________________________________OK"),
    ok.

test_for_level_2_change_aggrf_neuron(Neuron_id) ->
    ?INFO("Change neuron aggregation function ..................................................."),
    N_0 = edb:read(Neuron_id),
    AggrFList = [fun aggregation_function:diff_product/2, fun aggregation_function:dot_power/2],
    transform:change_aggregation(Neuron_id, RandomAggrF = randomElement(AggrFList -- [elements:aggregation(N_0)])),
    N_1 = edb:read(Neuron_id),
    ?INFO(lists:flatten(enn:pformat(N_0))),
    ?INFO(lists:flatten(enn:pformat(N_1))),
    RandomAggrF = elements:aggregation(N_1),
    ?INFO("____________________________________________________________________________________OK"),
    ok.


% TODO: Implement Level 3, 4 and 5

% -------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS --------------------------------------------------------------------------------------------

% -------------------------------------------------------------------
randomElement(List) -> lists:nth(rand:uniform(length(List)), List).

% -------------------------------------------------------------------
randomElements(List, Per) -> [X || {Rand, X} <- [{rand:uniform(), N} || N <- List], Rand > Per].













