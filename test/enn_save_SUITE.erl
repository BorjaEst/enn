%%%-------------------------------------------------------------------
%%% Author  :
%%% Description :
%%% Created :
%%%-------------------------------------------------------------------
-module(enn_save_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").

-define(HEAD, [$- || _ <-lists:seq(1,80)] ++ "\n").
-define(HEAD(Text), ct:log(?LOW_IMPORTANCE, ?HEAD ++ "~p", [Text])).
-define(END,  [$_ || _ <-lists:seq(1,80)]).
-define(END(V), ct:log(?LOW_IMPORTANCE, "~p~n" ++ ?END, [V]), V).

-define(INFO(A,B),    ct:log(?LOW_IMPORTANCE,    "~p: ~p",   [A,B])).
-define(ERROR(Error), ct:pal( ?HI_IMPORTANCE, "Error: ~p", [Error])).


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
    Id = enn:compile(test_architectures:example()),
    [{network, Id} | Config].

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
        {sequence_training_and_saving, [sequence, shuffle],
         [
            training_saved_after_stop,
            correct_network_clonation
         ]
        },
        {parallel_training_and_saving, [parallel, shuffle],
         [
            training_saved_after_stop | % Parallel training not possible
            [correct_network_clonation || _ <- lists:seq(1,5)]
         ]
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
    [ 
        {group, sequence_training_and_saving},
        {group, parallel_training_and_saving}
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
training_saved_after_stop() ->
    [].
training_saved_after_stop(Config) ->
    ?HEAD("Correct neural network saving after stop .............."),
    Id = ?config(network, Config),
    Id = enn:start(Id),
    {atomic, Ns1} = mnesia:transaction(fun() -> neurons(Id) end),
    {atomic, Ws1} = mnesia:transaction(fun() -> weights(Id) end),
    {In,Opt} = test_data_generators:random_sum_of_inputs(
                                  enn:inputs(Id),enn:outputs(Id),10),
    _  = enn:fit(Id, In, Opt),
    ok = enn:stop(Id),
    {atomic, Ns2} = mnesia:transaction(fun() -> neurons(Id) end),
    {atomic, Ws2} = mnesia:transaction(fun() -> weights(Id) end),
    true = Ns1 /= Ns2,
    true = Ws1 /= Ws2,
    ?END(ok).

% -------------------------------------------------------------------
correct_network_clonation() -> 
    [].
correct_network_clonation(Config) -> 
    ?HEAD("Network can be cloned ................................."),
    Id1 = ?config(network, Config),
    Id2 = enn:clone(Id1),
    {atomic, true} = mnesia:transaction(
        fun() -> 
            true = neurons(Id1) /= neurons(Id2),
            true =  biases(Id1) ==  biases(Id2),
            true =   links(Id1) /=   links(Id2),
            true = weights(Id1) == weights(Id2)
        end
    ), ?END(ok).


% --------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS ------------------------------------------

% Gets the network id neurons ---------------------------------------
neurons(Network) -> 
    [NN] = mnesia:read(network, Network),
    Neurons = [hd(mnesia:read(neuron, Id)) || Id <- network:neurons(NN)],
    ?INFO("Neurons: ", {Network, Neurons}),
    Neurons. 

% Gets the network id neuron bias -----------------------------------
biases(Network) -> 
    Biases = [neuron:bias(N) || N <- neurons(Network)],
    ?INFO("Biases: ", {Network, Biases}),
    Biases.

% Gets the network id links -----------------------------------------
links(Network) -> 
    [NN]  = mnesia:read(network, Network),
    Links = network:links(NN),
    ?INFO("Links: ", {Network, Links}),
    Links.

% Gets the network id link weights ----------------------------------
weights(Network) -> 
    Links = links(Network),
    Weights = [link:read(L) || L <- Links],
    ?INFO("Weights: ", {Network, Weights}),
    Weights.


% --------------------------------------------------------------------
% RESULTS CONSOLE PRINT ----------------------------------------------

