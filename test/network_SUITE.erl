%%%-------------------------------------------------------------------
%%% File    : network_SUITE.erl
%%% Author  :
%%% Description :
%%%
%%% Created :
%%%-------------------------------------------------------------------
-module(network_SUITE).
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
    [{timetrap, {seconds, 5}}].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
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
    [ ].

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%%--------------------------------------------------------------------
all() ->
    [
        test_add_nodes,
        test_add_links,
        test_start_node,
        test_end_node,
        test_delete_node
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
test_add_nodes() ->
    [].
test_add_nodes(_Config) ->
    ?HEAD("Correct addition of nodes in a network ................"),
    NN0 = network:new(sequential),
    NN1 = network:add_neurons(NN0, [n1,n2,n3]),
    ?END({ok, NN1}).

% -------------------------------------------------------------------
test_add_links() ->
    [].
test_add_links(_Config) -> 
    ?HEAD("Correct addition of links in a network ................"),
    % SeqLinks = [{A,B} || A <- [n1],    B <- [n2]          ],
    % RecLinks = [{A,B} || A <- [n1,n2], B <- [n1,n2], A=/=B],
    SeqL = {n1,n2},
    RccL = {n1,n1},
    [SeqL] = network:links(
               network:add_link(seq_network([n1,n2]), SeqL)),
    [SeqL] = network:links( 
               network:add_link(rcc_network([n1,n2]), SeqL)),
    {'EXIT',{{bad_link,[n1]}, _}} = 
        (catch network:add_link(seq_network([n1,n2]), RccL)),
    [RccL] = network:links(
               network:add_link(rcc_network([n1,n2]), RccL)),
    ?END(ok).

% -------------------------------------------------------------------
test_start_node() ->
    [].
test_start_node(_Config) ->
    ?HEAD("Correct behaviour of start node in network ............"),
    NN0 = seq_network([n1,n2]),
    0   = network:in_degree(NN0), 
    NN1 = network:add_links(NN0, 
            [{A,B} || A <- [start], B <- [n1,n2]]),
    []  = network:bias_neurons(NN1),
    2   = network:in_degree(NN1),
    ?END(ok).

% -------------------------------------------------------------------
test_end_node() ->
    [].
test_end_node(_Config) -> 
    ?HEAD("Correct behaviour of end node in network .............."),
    NN0 = seq_network([n1,n2]),
    0   = network:out_degree(NN0),
    NN1 = network:add_links(NN0, 
            [{A,B} || A <- [n1,n2], B <- ['end']]),
    []  = network:sink_neurons(NN1),
    2   = network:out_degree(NN1),
    ?END(ok).

% -------------------------------------------------------------------
test_delete_node() ->
    [].
test_delete_node(_Config) -> 
    ?HEAD("Correct delete of a network node ......................"),
    NN0 = seq_network([n1,n2]),
    NN1 = network:add_links(NN0, [{start,X} || X<-[n1,n2]] ++ 
                                 [{X,'end'} || X<-[n1,n2]] ++
                                 [{n1,n2}]),
    NN2 = network:del_neurons(NN1, [n2]),
    [n1] = network:in_nodes(NN2), 
    [n1] = network:out_nodes(NN2),
    {_,'end'} = hd(network:out_links(NN2, n1)),
    {start,_} = hd( network:in_links(NN2, n1)),
    ?END(ok).


% -------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS -----------------------------------------

% Creates a simple sequential network -------------------------------
seq_network(Nodes) -> 
    NN = network:new(sequential),
    network:add_neurons(NN, Nodes).

% Creates a simple recurrent network --------------------------------
rcc_network(Nodes) -> 
    NN = network:new(recurrent),
    network:add_neurons(NN, Nodes). 

