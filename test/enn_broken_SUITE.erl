%%%-------------------------------------------------------------------
%%% Author  :
%%% Description :
%%% Created :
%%%-------------------------------------------------------------------
-module(enn_broken_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").

-define(HEAD, [$- || _ <-lists:seq(1,80)] ++ "\n").
-define(HEAD(Text), ct:log(?LOW_IMPORTANCE, ?HEAD ++ "~p", [Text])).
-define(END,  [$_ || _ <-lists:seq(1,80)]).
-define(END(V), ct:log(?LOW_IMPORTANCE, "~p~n" ++ ?END, [V]), V).

-define(INFO(A,B),    ct:log(?LOW_IMPORTANCE,    "~p: ~p",   [A,B])).
-define(ERROR(Error), ct:pal( ?HI_IMPORTANCE, "Error: ~p", [Error])).

-define(TRAINING_LINES, 8000).


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
    [].

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%%--------------------------------------------------------------------
all() ->
    [ 
        broken_connections,
        infinite_loop,
        dummy_neurons
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
broken_connections(_Config) ->
    {ok, Id}  = correct_model_compilation(
        test_architectures:broken_connections()
    ),
    try enn:start(Id) of 
        Result ->
            ?INFO("Error not raised, result:", Result),
            error("error not raised")
    catch
        error:broken_nn -> ?INFO("Error raised", broken_nn)
    end,
    ?END({ok, Id}).

% -------------------------------------------------------------------
infinite_loop(_Config) ->
    {ok, Id}  = correct_model_compilation(
        test_architectures:infinite_loop()
    ),
    {atomic, {N_in, N_out}} = mnesia:transaction(
        fun() -> {enn:inputs(Id), enn:outputs(Id)} end
    ),
    Id = enn:start(Id),
    timer:sleep(200), % Neurons need some time to exit
    #{nn_pool := NNpool} = enn:status(Id),
    Alive = [Pid || Pid <- nn_pool:pids(NNpool), is_process_alive(Pid)],
    true = length(Alive) == N_in + N_out + 1, %Cortex=>+1 
    correct_model_stop(Id),
    ?END({ok, Id}).

% -------------------------------------------------------------------
dummy_neurons(_Config) ->
    {ok, Id}  = correct_model_compilation(
        test_architectures:dummy_neurons()
    ),
    {atomic, {N_in, N_out}} = mnesia:transaction(
        fun() -> {enn:inputs(Id), enn:outputs(Id)} end
    ),
    Id = enn:start(Id),
    timer:sleep(200), % Neurons need some time to exit
    #{nn_pool := NNpool} = enn:status(Id),
    Alive = [Pid || Pid <- nn_pool:pids(NNpool), is_process_alive(Pid)],
    true = length(Alive) == N_in + N_out + 1, %Cortex=>+1 
    correct_model_training(
        Id, fun test_data_generators:random_sum_of_inputs/3
    ),
    correct_model_stop(Id),
    ?END({ok, Id}).


% --------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS ------------------------------------------

% -------------------------------------------------------------------
correct_model_compilation(Model) ->
    ?HEAD("Correct model compilation .............................."),
    {atomic,Id} = mnesia:transaction(fun() -> enn:compile(Model) end), 
    ?INFO("Model", Model),
    {atomic,NN_Info} = mnesia:transaction(fun() -> enn:info(Id) end),
    ?INFO("Network", NN_Info),
    ?END({ok, Id}).

% -------------------------------------------------------------------
correct_model_training(Id, Training) ->
    ?HEAD("Correct fit of model using backpropagation ............."),
    {atomic, {N_in, N_out}} = mnesia:transaction(
        fun() -> {enn:inputs(Id), enn:outputs(Id)} end
    ),
    {Inputs, Optimas} = Training(N_in, N_out, ?TRAINING_LINES),
    _Loss = enn:fit(Id, Inputs, Optimas),
    ?END(ok).

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

