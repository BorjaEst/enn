%%%-------------------------------------------------------------------
%%% Author  :
%%% Description :
%%% Created :
%%%-------------------------------------------------------------------
-module(enn_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("layers.hrl").

-define(HEAD, [$- || _ <-lists:seq(1,80)] ++ "\n").
-define(HEAD(Text), ct:log(?LOW_IMPORTANCE, ?HEAD ++ "~p", [Text])).
-define(END,  [$_ || _ <-lists:seq(1,80)]).
-define(END(V), ct:log(?LOW_IMPORTANCE, "~p~n" ++ ?END, [V]), V).

-define(INFO(A,B),    ct:log(?LOW_IMPORTANCE,    "~p: ~p",   [A,B])).
-define(ERROR(Error), ct:pal( ?HI_IMPORTANCE, "Error: ~p", [Error])).
-define(PROGRESS_BAR, #{size => 20}).

-define(TEST_MODEL(Model, Training), 
    test_model(atom_to_list(?FUNCTION_NAME) ++ ".json", Model, Training)).

-define(MAX_UNITS_PER_LAYER, 20).
-define(MAX_NUMBER_LAYERS,    4).
-define(TRAINING_LINES,    8000).
-define(PARALLEL_NN,          8).


%%--------------------------------------------------------------------
%% Function: suite() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
suite() ->
    [{timetrap, {seconds, 8}}].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    ok = application:start(mnesia),
    ok = application:start(datalog),
    ok = application:start(enn),
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok = application:stop(mnesia),
    ok = application:stop(datalog),
    ok = application:stop(enn),
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
        {test_simple_architectures, [sequence],
         [
            xor_gate_static_inputs,
            xor_gate_random_inputs,
            addition_static_inputs,
            addition_random_inputs
         ]
        },
        {test_complex_architectures, [sequence],
         [
            mult_random_inputs,
            recurrent_1_input
         ]
        },
        {test_error_networks, [parallel],
         [
            test_for_empty_nn,
            test_for_broken_nn
         ]
        },
        {test_parallel_networks, [parallel, {repeat,?PARALLEL_NN}],
            [random_dense_random_inputs]
        }
    ].

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%%--------------------------------------------------------------------
all() ->
    [ % NOTE THAT GROUPS CANNOT BE DEBUGGED WITH {step, ?STEP_OPTS}
        {group, test_simple_architectures},
        {group, test_complex_architectures},
        % {group, test_error_networks}
        {group, test_parallel_networks}
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
xor_gate_static_inputs() ->
    [].
xor_gate_static_inputs(_Config) ->
    {ok, Loss10} = ?TEST_MODEL(
        _Model = test_architectures:xor_gate(),
        _Data  = fun test_data_generators:static_xor_of_inputs/3
    ),
    console_print_loss(?FUNCTION_NAME, Loss10).

% -------------------------------------------------------------------
xor_gate_random_inputs() ->
    [].
xor_gate_random_inputs(_Config) ->
    {ok, Loss10} = ?TEST_MODEL(
        _Model = test_architectures:xor_gate(),
        _Data  = fun test_data_generators:random_xor_of_inputs/3
    ),
    console_print_loss(?FUNCTION_NAME, Loss10).

% -------------------------------------------------------------------
addition_static_inputs() ->
    [].
addition_static_inputs(_Config) ->
    {ok, Loss10} = ?TEST_MODEL(
        _Model = test_architectures:addition(),
        _Data  = fun test_data_generators:static_sum_of_inputs/3
    ),
    console_print_loss(?FUNCTION_NAME, Loss10).

% -------------------------------------------------------------------
addition_random_inputs() ->
    [].
addition_random_inputs(_Config) ->
    {ok, Loss10} = ?TEST_MODEL(
        _Model = test_architectures:addition(),
        _Data  = fun test_data_generators:random_sum_of_inputs/3
    ),
    console_print_loss(?FUNCTION_NAME, Loss10).

% -------------------------------------------------------------------
mult_random_inputs() ->
    [].
mult_random_inputs(_Config) ->
    {ok, Loss10} = ?TEST_MODEL(
        _Model = test_architectures:multiplication(),
        _Data  = fun test_data_generators:random_mult_of_inputs/3
    ),
    console_print_loss(?FUNCTION_NAME, Loss10).

% -------------------------------------------------------------------
recurrent_1_input() ->
    [].
recurrent_1_input(_Config) ->
    {ok, Loss10} = ?TEST_MODEL(
        _Model = test_architectures:recurrent(),
        _Data  = fun test_data_generators:recurrent_of_1_input/3
    ),
    console_print_loss(?FUNCTION_NAME, Loss10).

% -------------------------------------------------------------------
random_dense_random_inputs() ->
    [].
random_dense_random_inputs(_Config) ->
    N     = erlang:unique_integer([positive, monotonic]),
    NameJ = "random_dense" ++ integer_to_list(N) ++ ".json",
    DataF = fun test_data_generators:random_sum_of_inputs/3,
    Model = test_architectures:random_dense(?MAX_UNITS_PER_LAYER,
                                            ?MAX_NUMBER_LAYERS),
    test_model(NameJ, Model, DataF).


% --------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS ------------------------------------------

% -------------------------------------------------------------------
test_model(FileName, Model, Training) ->
    {ok,    Id}  = correct_model_compilation(Model),
    ok           = correct_model_start(Id),
    {ok, Loss10} = correct_model_training(Id, Training, FileName),
    ok           = correct_model_stop(Id),
    {ok, Loss10}.

% -------------------------------------------------------------------
correct_model_compilation(Model) ->
    ?HEAD("Correct model compilation .............................."),
    {atomic,Id} = enn:compile(Model), ?INFO(  "Model",   Model),
    {atomic,NN_Info} = enn:info(Id),  ?INFO("Network", NN_Info),
    ?END({ok, Id}).

% -------------------------------------------------------------------
correct_model_start(Id) ->
    ?HEAD("Correct neural network start form a network id ........"),
    Id = enn:start(Id), % Return NN_id for non_compiled start
    ?END(ok).

% -------------------------------------------------------------------
correct_model_training(Id, Training, FileName) ->
    ?HEAD("Correct fit of model using backpropagation ............."),
    Options = [{print, 3}, {log, FileName}, {return, [loss]}],
    {Inputs, Optimas} = Training(enn:inputs(Id), 
                                 enn:outputs(Id), 
                                 ?TRAINING_LINES),
    [Loss] = enn:run(Id, Inputs, Optimas, Options),
    ?END({ok, average(Loss, 10)}).

% -------------------------------------------------------------------
correct_model_stop(Id) ->
    ?HEAD("Correct neural network stop form a network id ........."),
    Cortex = enn:cortex(Id),
    true   = is_process_alive(Cortex), 
    ok     = enn:stop(Id),
    false  = is_process_alive(Cortex), 
    not_running = enn:status(Id),
    ?END(ok).


% --------------------------------------------------------------------
% RESULTS CONSOLE PRINT ----------------------------------------------

% -------------------------------------------------------------------
average(List, N) when length(List) >= N ->
    NdArray = ndarray:new([length(List)], List),
    NdAMean = numerl:mean(ndarray:reshape(NdArray, [-1,N]), 0),
    ndarray:data(NdAMean);
average(List, N) when length(List) < N  -> 
    List.

% -------------------------------------------------------------------
console_print_loss(FunName, LossList) -> 
    Step = round(?TRAINING_LINES/length(LossList)),
    Seq  = lists:zip(lists:seq(1, length(LossList)), 
                     LossList),
    Data = [{Step*N, Loss} || {N, Loss} <- Seq],
    Format = atom_to_list(FunName) ++ "\n" ++  
             console_print("loss:", ?TRAINING_LINES, Data),
    ct:print(Format).

console_print(Title, Size, [{N, Value} | Rest]) -> 
    Data = [N, N/Size, Title, Value],
    Format = reports:progress_line(2, Data, ?PROGRESS_BAR),
    Format ++ "\n" ++ console_print(Title, Size, Rest);
console_print(_, _, []) -> 
    [].



