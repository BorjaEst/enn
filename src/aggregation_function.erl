%%%-------------------------------------------------------------------
%%% @author borja
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Aug 2018 14:25
%%%-------------------------------------------------------------------
-module(aggregation_function).
-compile([debug_info, export_all, nowarn_export_all, {d, debug_mode}]). %% TODO: To delete after build

-include_lib("eunit/include/eunit.hrl").

-define(EULER, 2.718281828459045235360287471352662497757247093699959574966967627724076630353).

%% API
-export([apply/3]).

-type aggregation_function() :: dotprod | diffprod.
-export_type([aggregation_function/0]).

%%%===================================================================
%%% API
%%%===================================================================

% ......................................................................................................................
% TODO: Define specs and comments
apply(direct, Tensor, Bias)   -> direct(Tensor, Bias); % Special, must be used only by inputs and outputs
apply(dotprod, Tensor, Bias)  -> dot_product(Tensor, Bias);
apply(diffprod, Tensor, Bias) -> diff_product(Tensor, Bias);
apply(_Reference, _, _)           -> error("activation function not defined").


%%====================================================================
%% Internal functions
%%====================================================================

% ......................................................................................................................
% TODO: Define specs and comments
direct(Tensor, Bias) -> %% TODO: Evaluate if better without Bias
	lists:sum([Input || {_, Input} <- Tensor]) + Bias.

% ......................................................................................................................
% TODO: Define specs and comments
dot_product([{Weight, Input} | TensorAcc], Acc) ->
	dot_product(TensorAcc, (Weight * Input) + Acc);
dot_product([], Acc) ->
	Acc.

% ......................................................................................................................
% TODO: Define specs and comments
diff_product(TensorAcc, Bias) ->
	try
		Prev_TensorAcc = put(diff_product, TensorAcc),
		diff_product(TensorAcc, Prev_TensorAcc, Bias)
	catch
		error:function_clause -> Bias % TODO: To evaluate only on undefined diff_prod, or mutation affected
	end.

diff_product([{Weight, Input} | TensorAcc], [{_, Prev_Input} | Prev_TensorAcc], Acc) ->
	Diff = Input - Prev_Input,
	diff_product(TensorAcc, Prev_TensorAcc, (Weight * Diff) + Acc);
diff_product([], [], Acc) ->
	Acc.

% ......................................................................................................................
% TODO: Define specs and comments
dot_power(TensorAcc, Bias) ->
	math:pow(?EULER, dot_product(TensorAcc, 0.0)) * Bias.

% ......................................................................................................................
% TODO: Define specs and comments
diff_power(TensorAcc, Bias) ->
	math:pow(?EULER, diff_product(TensorAcc, 0.0)) * Bias.


%%====================================================================
%% Eunit white box tests
%%====================================================================

% ----------------------------------------------------------------------------------------------------------------------
% TESTS DESCRIPTIONS ---------------------------------------------------------------------------------------------------
this_example_test_() ->
	% {setup, Where, Setup, Cleanup, Tests | Instantiator}
	[
		{"Test for the dot product aggregation functions",
		 {setup, local, fun iacc_setup/0, fun no_cleanup/1, fun test_dot_product_functions/1}},
		{"Test for the diff product aggregation functions",
		 {setup, local, fun iacc_setup/0, fun no_cleanup/1, fun test_diff_product_functions/1}},
		{"Test for the dot power aggregation functions",
		 {setup, local, fun iacc_setup/0, fun no_cleanup/1, fun test_dot_power_functions/1}}
%%		{"Test for the diff power aggregation functions",
%%		 {setup, local, fun iacc_setup/0, fun no_cleanup/1, fun test_diff_power_functions/1}}
	].

% ----------------------------------------------------------------------------------------------------------------------
% SPECIFIC SETUP FUNCTIONS ---------------------------------------------------------------------------------------------
iacc_setup() ->
	Tensor1 = {1.0, 1.0},
	Tensor2 = {-1.0, 1.0},
	Tensor2_2 = {-2.0, 3.0},
	Tensor3 = {0.0, 2.0},
	Tensor4 = {-2.0, 1.0},
	#{iacc   => [Tensor1, Tensor2, Tensor3, Tensor4],
	  iacc_2 => [Tensor1, Tensor2_2, Tensor3, Tensor4]}.

no_cleanup(_) ->
	ok.

% ----------------------------------------------------------------------------------------------------------------------
% ACTUAL TESTS ---------------------------------------------------------------------------------------------------------
test_dot_product_functions(Data) ->
	#{iacc := TensorAcc} = Data,
	[
		?_assertEqual(0.5, dot_product(TensorAcc, 2.5))
	].

test_diff_product_functions(Data) ->
	#{iacc := TensorAcc,
	  iacc_2 := TensorAcc_2} = Data,
	[
		?_assertEqual(-0.5, diff_product(TensorAcc, -0.5)),
		?_assertEqual(-2.5, diff_product(TensorAcc_2, 1.5))
	].

test_dot_power_functions(Data) ->
	#{iacc := TensorAcc} = Data,
	[
		?_assertEqual(math:pow(?EULER, -2.0) * (-0.2), dot_power(TensorAcc, -0.2))
	].

test_diff_power_functions(Data) ->
	#{iacc := TensorAcc,
	  iacc_2 := TensorAcc_2} = Data,
	[
		?_assertEqual(math:pow(?EULER, 0.0) * (-0.2), diff_power(TensorAcc, -0.2)),
		?_assertEqual(math:pow(?EULER, -4.0) * (-0.2), diff_power(TensorAcc_2, -0.2))
	].


% ----------------------------------------------------------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS --------------------------------------------------------------------------------------------

