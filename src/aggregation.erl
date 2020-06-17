%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% TODO: Remove get and put (find a better solution).
%%% @end
%%%-------------------------------------------------------------------
-module(aggregation).

-include_lib("math_constants.hrl").
-include_lib("enn_logger.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([func/3]).
-export_type([func/0]).

-type func() :: direct | dot_prod | diff_prod | product | dot_power |
                diff_prod | diff_power.


%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc Applies the indicated aggregation funtion.
%% Note "direct" should be used only by inputs and outputs.
%% @end
%%--------------------------------------------------------------------
-spec func(Function, Tensor, Bias) ->  Result when 
    Function :: func(),
    Tensor   :: [{Wi :: float(), Xi :: float()}],
    Bias     :: float(),
    Result   :: float().
func(Function, Tensor, Bias) ->
    ?LOG_AGGREGATION_FUNCTION_REQUEST(Function, Tensor, Bias),
    Result = apply_fun(Function, Tensor, Bias),
    ?LOG_AGGREGATION_FUNCTION_RESULT(Function, Result),
    Result.

apply_fun(      direct, Tensor, Bias) ->       direct(Tensor, Bias); 
apply_fun(    dot_prod, Tensor, Bias) ->  dot_product(Tensor, Bias);
apply_fun(   diff_prod, Tensor, Bias) -> diff_product(Tensor, Bias);
apply_fun(     product, Tensor, Bias) ->      product(Tensor, Bias);
apply_fun(   dot_power, Tensor, Bias) ->    dot_power(Tensor, Bias);
apply_fun(  diff_power, Tensor, Bias) ->   diff_power(Tensor, Bias);
apply_fun(        _Ref,_Tensor,_Bias) ->        error(not_defined).


%%====================================================================
%% Internal functions
%%====================================================================

% -------------------------------------------------------------------
% TODO: Define specs and comments
direct(Tensor, _Bias) -> %% TODO:Add bias if separated init for inputs 
    lists:sum([Input || {_, Input} <- Tensor]).

% -------------------------------------------------------------------
% TODO: Define specs and comments
dot_product([{Weight, Input} | TensorAcc], Acc) ->
    dot_product(TensorAcc, (Weight * Input) + Acc);
dot_product([], Acc) ->
    Acc.

% -------------------------------------------------------------------
% TODO: Define specs and comments
diff_product(TensorAcc, Bias) ->
    try
        Prev_TensorAcc = put(diff_product, TensorAcc),
        diff_product(TensorAcc, Prev_TensorAcc, Bias)
    catch
        error:function_clause -> Bias % TODO: To evaluate only on undefined diff_prod, or transform affected
    end.

diff_product([{W, I} | TAcc], [{_, Prev_I} | Prev_TAcc], Acc) ->
    Diff = I - Prev_I,
    diff_product(TAcc, Prev_TAcc, (W * Diff) + Acc);
diff_product([], [], Acc) ->
    Acc.

% -------------------------------------------------------------------
% TODO: Define specs and comments
product([{_, Input} | TensorAcc], Acc) ->
    product(TensorAcc, Input * Acc);
product([], Acc) ->
    Acc.

% -------------------------------------------------------------------
% TODO: Define specs and comments
dot_power(TensorAcc, Bias) ->
    math:pow(?EULER, dot_product(TensorAcc, 0.0)) * Bias.

% -------------------------------------------------------------------
% TODO: Define specs and comments
diff_power(TensorAcc, Bias) ->
    math:pow(?EULER, diff_product(TensorAcc, 0.0)) * Bias.


%%====================================================================
%% Eunit white box tests
%%====================================================================

% --------------------------------------------------------------------
% TESTS DESCRIPTIONS -------------------------------------------------
this_example_test_() ->
    % {setup, Where, Setup, Cleanup, Tests | Instantiator}
    [
        {"Test for the dot product aggregation functions",
         {setup, local, fun iacc_setup/0, fun no_cleanup/1, 
          fun test_dot_product_functions/1}},
        {"Test for the diff product aggregation functions",
         {setup, local, fun iacc_setup/0, fun no_cleanup/1, 
          fun test_diff_product_functions/1}},
        {"Test for the dot power aggregation functions",
         {setup, local, fun iacc_setup/0, fun no_cleanup/1, 
          fun test_dot_power_functions/1}},
        {"Test for the diff power aggregation functions",
         {setup, local, fun iacc_setup/0, fun no_cleanup/1, 
          fun test_diff_power_functions/1}}
    ].

% --------------------------------------------------------------------
% SPECIFIC SETUP FUNCTIONS -------------------------------------------
iacc_setup() ->
    Tensor1 = {1.0, 1.0},
    Tensor2 = {-1.0, 1.0},
    Tensor2_2 = {-2.0, 3.0},
    Tensor3 = {0.0, 2.0},
    Tensor4 = {-2.0, 1.0},
    #{iacc_1 => [Tensor1, Tensor2, Tensor3, Tensor4],
      iacc_2 => [Tensor1, Tensor2_2, Tensor3, Tensor4]}.

no_cleanup(_) ->
    ok.

% --------------------------------------------------------------------
% ACTUAL TESTS -------------------------------------------------------
test_dot_product_functions(Data) ->
    #{iacc_1 := TensorAcc} = Data,
    [
        ?_assertEqual(0.5, dot_product(TensorAcc, 2.5))
    ].

test_diff_product_functions(Data) ->
    #{iacc_1 := TensorAcc_1,
      iacc_2 := TensorAcc_2} = Data,
    put(diff_product, TensorAcc_1),
    [
        ?_assertEqual(-2.5, diff_product(TensorAcc_2, 1.5))
    ].

test_dot_power_functions(Data) ->
    #{iacc_1 := TensorAcc} = Data,
    [
        ?_assertEqual(math:pow(?EULER, -2.0) * (-0.2), 
                               dot_power(TensorAcc, -0.2))
    ].

test_diff_power_functions(Data) ->
    #{iacc_1 := TensorAcc_1,
      iacc_2 := TensorAcc_2} = Data,
    put(diff_product, TensorAcc_1),
    [
        ?_assertEqual(math:pow(?EULER, -4.0) * (-0.2), 
                               diff_power(TensorAcc_2, -0.2))
    ].


% --------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS ------------------------------------------

