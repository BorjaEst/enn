%%%-------------------------------------------------------------------
%%% Author  :
%%% Description :
%%% Created :
%%%-------------------------------------------------------------------
-module(enn_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").

-define(HEAD, [$- || _ <-lists:seq(1,80)] ++ "\n").
-define(HEAD(Text), ct:log(?LOW_IMPORTANCE, ?HEAD ++ "~p", [Text])).
-define(END,  [$_ || _ <-lists:seq(1,80)]).
-define(END(V), ct:log(?LOW_IMPORTANCE, "~p~n" ++ ?END, [V]), V).

-define(INFO(A,B),    ct:log(?LOW_IMPORTANCE,    "~p: ~p",   [A,B])).
-define(ERROR(Error), ct:pal( ?HI_IMPORTANCE, "Error: ~p", [Error])).
-define(PROGRESS_BAR, #{size => 20}).

-define(ACC_TRAINING_LINES,  8000).
-define(DEBUG_TRAINING_LINES, 100).
-define(PARALLEL_NN,            8).


%%--------------------------------------------------------------------
%% Function: suite() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
suite() ->
    [{timetrap, {seconds, 20}}].

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
init_per_group(test_simple_architectures, Config) -> 
    [{training_lines, ?ACC_TRAINING_LINES}|Config];
init_per_group(test_complex_architectures, Config) -> 
    [{training_lines, ?ACC_TRAINING_LINES}|Config];
init_per_group(test_broken_architectures, Config) -> 
    [{training_lines, ?DEBUG_TRAINING_LINES}|Config];
init_per_group(test_parallel_networks, Config) -> 
    [{training_lines, ?DEBUG_TRAINING_LINES}|Config];
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
init_per_testcase(xor_gate_static_inputs, Config) ->
    [{generator, data_generators:static_xor_of_inputs()},
     {model,     test_architectures:xor_gate()}|Config];
init_per_testcase(xor_gate_random_inputs, Config) ->
    [{generator, data_generators:random_xor_of_inputs()},
     {model,     test_architectures:xor_gate()}|Config];
init_per_testcase(addition_static_inputs, Config) ->
    [{generator, data_generators:static_sum_of_inputs()},
     {model,     test_architectures:addition()}|Config];
init_per_testcase(addition_random_inputs, Config) ->
    [{generator, data_generators:random_sum_of_inputs()},
     {model,     test_architectures:addition()}|Config];
init_per_testcase(mult_random_inputs, Config) ->
    [{generator, data_generators:random_mult_of_inputs()},
     {model,     test_architectures:multiplication()}|Config];     
init_per_testcase(recurrent_1_input, Config) ->
    [{generator, data_generators:recurrent_of_1_input()},
     {model,     test_architectures:recurrent()}|Config];
init_per_testcase(broken_connections, Config) ->
    [{model, test_architectures:broken_connections()}|Config];
init_per_testcase(infinite_loop, Config) ->
    [{generator, data_generators:random_sum_of_inputs()},
     {model,     test_architectures:infinite_loop()}|Config];     
init_per_testcase(dummy_neurons, Config) ->
    [{generator, data_generators:random_sum_of_inputs()},
     {model,     test_architectures:dummy_neurons()}|Config];
init_per_testcase(random_dense_random_inputs, Config) ->
    [{generator, data_generators:random_sum_of_inputs()},
     {model,     test_architectures:random_dense()}|Config];
init_per_testcase(test_pid_link, Config) ->
    [{model, test_architectures:example()}|Config];
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
        {test_simple_architectures, [parallel],
         [
            xor_gate_static_inputs,
            xor_gate_random_inputs,
            addition_static_inputs,
            addition_random_inputs
         ]
        },
        {test_complex_architectures, [parallel],
         [
            mult_random_inputs,
            recurrent_1_input
         ]
        },
        {test_broken_architectures, [parallel],
         [
            broken_connections,
            infinite_loop,
            dummy_neurons
         ]
        },
        {test_parallel_networks, [parallel],
            [random_dense_random_inputs || _ <- lists:seq(1, ?PARALLEL_NN)]
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
        {group, test_broken_architectures},
        {group, test_parallel_networks},
        test_pid_link
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
xor_gate_static_inputs(Config) ->
    {ok,     Id} = correct_model_compilation(Config),
    {ok,     Id} = correct_model_start(Id),
    {ok, Loss10} = correct_model_training(Id, Config),
    ok           = correct_model_stop(Id),
    console_print_loss(?FUNCTION_NAME, Loss10).

% -------------------------------------------------------------------
xor_gate_random_inputs(Config) ->
    {ok,     Id} = correct_model_compilation(Config),
    {ok,     Id} = correct_model_start(Id),
    {ok, Loss10} = correct_model_training(Id, Config),
    ok           = correct_model_stop(Id),
    console_print_loss(?FUNCTION_NAME, Loss10).

% -------------------------------------------------------------------
addition_static_inputs(Config) ->
    {ok,     Id} = correct_model_compilation(Config),
    {ok,     Id} = correct_model_start(Id),
    {ok, Loss10} = correct_model_training(Id, Config),
    ok           = correct_model_stop(Id),
    console_print_loss(?FUNCTION_NAME, Loss10).

% -------------------------------------------------------------------
addition_random_inputs(Config) ->
    {ok,     Id} = correct_model_compilation(Config),
    {ok,     Id} = correct_model_start(Id),
    {ok, Loss10} = correct_model_training(Id, Config),
    ok           = correct_model_stop(Id),
    console_print_loss(?FUNCTION_NAME, Loss10).

% -------------------------------------------------------------------
mult_random_inputs(Config) ->
    {ok,     Id} = correct_model_compilation(Config),
    {ok,     Id} = correct_model_start(Id),
    {ok, Loss10} = correct_model_training(Id, Config),
    ok           = correct_model_stop(Id),
    console_print_loss(?FUNCTION_NAME, Loss10).

% -------------------------------------------------------------------
recurrent_1_input(Config) ->
    {ok,     Id} = correct_model_compilation(Config),
    {ok,     Id} = correct_model_start(Id),
    {ok, Loss10} = correct_model_training(Id, Config),
    ok           = correct_model_stop(Id),
    console_print_loss(?FUNCTION_NAME, Loss10).

% -------------------------------------------------------------------
broken_connections(Config) ->
    {ok, Id} = correct_model_compilation(Config),
    case correct_model_start(Id) of 
        {ok, Id} ->
            ?ERROR("Error not raised, result"),
            error("Error not raised");
        {error, broken_nn} -> 
            ?INFO("Error raised", broken_nn),
            ?END(ok)
    end.

% -------------------------------------------------------------------
infinite_loop(Config) ->
    {ok, Id}  = correct_model_compilation(Config),
    {atomic, {N_in, N_out}} = mnesia:transaction(
        fun() -> {enn:inputs(Id), enn:outputs(Id)} end
    ),
    {ok, Id} = correct_model_start(Id),
    timer:sleep(200), % Neurons need some time to exit
    #{nn_pool := NNpool} = enn:status(Id),
    Alive = [Pid || Pid <- nn_pool:pids(NNpool), is_process_alive(Pid)],
    true = length(Alive) == N_in + N_out + 1, %Cortex=>+1 
    ok = correct_model_stop(Id),
    ?END({ok, Id}).

% -------------------------------------------------------------------
dummy_neurons(Config) ->
    {ok, Id}  = correct_model_compilation(Config),
    {atomic, {N_in, N_out}} = mnesia:transaction(
        fun() -> {enn:inputs(Id), enn:outputs(Id)} end
    ),
    {ok, Id} = correct_model_start(Id),
    timer:sleep(200), % Neurons need some time to exit
    #{nn_pool := NNpool} = enn:status(Id),
    Alive = [Pid || Pid <- nn_pool:pids(NNpool), is_process_alive(Pid)],
    true = length(Alive) == N_in + N_out + 1, %Cortex=>+1 
    {ok,  _} = correct_model_training(Id, Config),
    ok       = correct_model_stop(Id),
    ?END({ok, Id}).

% -------------------------------------------------------------------
random_dense_random_inputs(Config) ->
    {ok,     Id} = correct_model_compilation(Config),
    test_architectures:shuffle_connections(Id, rand:uniform(4)),
    case correct_model_start(Id) of 
        {ok, Id} -> 
            {ok, _} = correct_model_training(Id, Config),
            ok      = correct_model_stop(Id);
        {error, broken_nn} -> 
            ?INFO("Pass test: error raised", broken_nn),
            ok
    end.

% -------------------------------------------------------------------
test_pid_link(Config) -> 
    {ok, Id}  = correct_model_compilation(Config),
    {ok, Id} = correct_model_start(Id),
    process_flag(trap_exit, true), % To catch network exit
    enn:cortex(Id) ! error,
    receive Message -> {'EXIT',_, {{_,info,error},_}} = Message
    after 100 -> error("Caller not down")
    end,
    process_flag(trap_exit, false), % To do not catch network exit
    timer:sleep(100),
    {ok, Id} = correct_model_start(Id),
    ok       = correct_model_stop(Id), % Caller does not dies
    ?END({ok, Id}).


% --------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS ------------------------------------------

% -------------------------------------------------------------------
correct_model_compilation(Config) ->
    ?HEAD("Correct model compilation .............................."),
    Model = ?config(model, Config),
    {atomic,Id} = mnesia:transaction(fun() -> enn:compile(Model) end), 
    ?INFO("Model", Model),
    {atomic,NN_Info} = mnesia:transaction(fun() -> enn:info(Id,out) end),
    ?INFO("Network", NN_Info),
    ?END({ok, Id}).

% -------------------------------------------------------------------
correct_model_start(Id) ->
    ?HEAD("Correct neural network start form a network id ........"),
    try enn:start_link(Id) of % Return NN_id for non_compiled start
        Id ->
            ?INFO("Correct network start", Id),
            ?END({ok, Id})
    catch
        error:broken_nn ->
            ?INFO("Network start fail by: ", broken_nn), 
            ?END({error, broken_nn})
    end.

% -------------------------------------------------------------------
correct_model_training(Id, Config) ->
    ?HEAD("Correct fit of model using backpropagation ............"),
    Generator = ?config(generator, Config),
    Lines     = ?config(training_lines, Config),
    Options = [{return, [loss]}],
    {atomic, {N_in, N_out}} = mnesia:transaction(
        fun() -> {enn:inputs(Id), enn:outputs(Id)} end
    ),
    {Inputs, Optimas} = Generator(N_in, N_out, Lines),
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
    Step = round(?ACC_TRAINING_LINES/length(LossList)),
    Seq  = lists:zip(lists:seq(1, length(LossList)), 
                     LossList),
    Data = [{Step*N, Loss} || {N, Loss} <- Seq],
    Format = atom_to_list(FunName) ++ "\n" ++  
             console_print("loss:", ?ACC_TRAINING_LINES, Data),
    ct:print(Format).

console_print(Title, Size, [{N, Value} | Rest]) -> 
    Data = [N, N/Size, Title, Value],
    Format = reports:progress_line(2, Data, ?PROGRESS_BAR),
    Format ++ "\n" ++ console_print(Title, Size, Rest);
console_print(_, _, []) -> 
    [].



