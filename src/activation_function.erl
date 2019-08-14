%%%-------------------------------------------------------------------
%%% @author borja
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Aug 2018 14:25
%%%-------------------------------------------------------------------
-module(activation_function).
-compile([export_all, nowarn_export_all]). %% TODO: To delete after build

-include_lib("eunit/include/eunit.hrl").

-define(PI, 3.141592653589793238462643383279502884197169399375105820974944592307816406286).
-define(EULER, 2.718281828459045235360287471352662497757247093699959574966967627724076630353).
-define(EQUAL_TOLERANCE, 0.001).

%% API
-export([apply/2, beta/3]).

-type activation_function() :: direct | sigmoid | tanh | softplus | softsign | elu | selu | relu | crelu | relu_x | dropout.
-export_type([activation_function/0]).

%%%===================================================================
%%% API
%%%===================================================================

% ......................................................................................................................
% TODO: Define specs and comments
apply(sigmoid, Soma)  -> sigmoid(Soma);
apply(tanh, Soma)     -> tanh(Soma);
apply(softplus, Soma) -> softplus(Soma);
apply(softsign, Soma) -> softsign(Soma);
apply(elu, Soma)      -> elu(Soma);
apply(selu, Soma)     -> selu(Soma);
apply(relu, Soma)     -> relu(Soma);
apply(crelu, _Soma)   -> error("activation function not defined");
apply(relu_x, _Soma)  -> error("activation function not defined");
apply(dropout, _Soma) -> error("activation function not defined");
apply(_Reference, _)  -> error("activation function not defined").

% ......................................................................................................................
% TODO: Define specs and comments
beta(sigmoid, Error, Soma)      -> do_beta(Error, Soma, fun d_sigmoid/1);
beta(tanh, Error, Soma)         -> do_beta(Error, Soma, fun d_tanh/1);
beta(softplus, Error, Soma)     -> do_beta(Error, Soma, fun d_softplus/1);
beta(softsign, Error, Soma)     -> do_beta(Error, Soma, fun d_softsign/1);
beta(elu, Error, Soma)          -> do_beta(Error, Soma, fun d_elu/1);
beta(selu, Error, Soma)         -> do_beta(Error, Soma, fun d_selu/1);
beta(relu, Error, Soma)         -> do_beta(Error, Soma, fun d_relu/1);
beta(crelu, _Error, _Soma)      -> error("activation derivative function not defined");
beta(relu_x, _Error, _Soma)     -> error("activation derivative function not defined");
beta(dropout, _Error, _Soma)    -> error("activation derivative function not defined");
beta(_Reference, _Error, _Soma) -> error("activation derivative function not defined").

do_beta(Error, Soma, DerF) when Error =< 0 ->
	if
		Soma =< 0 -> DerF(Soma) * Error;
		true -> Error
	end;
do_beta(Error, Soma, DerF) ->
	- do_beta(-Error, -Soma, DerF).


%%====================================================================
%% Internal functions
%%====================================================================

% ......................................................................................................................
% TODO: Define specs and comments
sigmoid(X) when X > 10.0  -> 1.0;
sigmoid(X) when X < -10.0 -> 0.0;
sigmoid(X) -> %(0 : 1)--Der:Y*(1-Y) %TODO: To check
	1 / (1 + math:exp(-X)).

% TODO: Define specs and comments
d_sigmoid(X) ->
	Sig = sigmoid(X),
	Sig * (1 - Sig).

% ......................................................................................................................
% TODO: Define specs and comments
tanh(X) ->
	math:tanh(X).

% TODO: Define specs and comments
d_tanh(X) ->
	1 - math:pow(tanh(X), 2).

% ......................................................................................................................
% TODO: Define specs and comments
softplus(X) ->
	math:log(1 + math:exp(X)).

% TODO: Define specs and comments
d_softplus(X) ->
	1 / (1 + math:exp(X)).

% ......................................................................................................................
% TODO: Define specs and comments
softsign(X) ->
	X / (1 + abs(X)).

% TODO: Define specs and comments
d_softsign(X) ->
	1 / math:pow(1 + abs(X), 2).

% ......................................................................................................................
% TODO: Define specs and comments
elu(X) when X =< 0 ->
	math:exp(X) - 1;
elu(X) when X > 0 ->
	X.

% TODO: Define specs and comments
d_elu(X) when X =< 0 ->
	math:exp(X);
d_elu(X) when X > 0 ->
	1.

% ......................................................................................................................
% TODO: Define specs and comments
selu(X) when X =< 0 ->
	1.0507 * 1.67326 * elu(X);
selu(X) when X > 0 ->
	1.0507 * elu(X).

% TODO: Define specs and comments
d_selu(X) when X =< 0 ->
	1.0507 * 1.67326 * d_elu(X);
d_selu(X) when X > 0 ->
	1.0507 * d_elu(X).

% ......................................................................................................................
% TODO: Define specs and comments
relu(X) when X < 0 ->
	0;
relu(X) when X >= 0 ->
	X.

% TODO: Define specs and comments
d_relu(X) when X < 0 ->
	0;
d_relu(X) when X >= 0 ->
	1.


%%====================================================================
%% Eunit white box tests
%%====================================================================

% ----------------------------------------------------------------------------------------------------------------------
% TESTS DESCRIPTIONS ---------------------------------------------------------------------------------------------------
this_example_test_() ->
	% {setup, Where, Setup, Cleanup, Tests | Instantiator}
	[
		{"Test for the trigonometrical activation functions",
		 {setup, local, fun no_setup/0, fun no_cleanup/1, fun test_trigonometric_functions/1}},
		{"Test for sigmoid activation functions",
		 {setup, local, fun no_setup/0, fun no_cleanup/1, fun test_sigmoid_functions/1}}
	].

% ----------------------------------------------------------------------------------------------------------------------
% SPECIFIC SETUP FUNCTIONS ---------------------------------------------------------------------------------------------
no_setup() ->
	ok.

no_cleanup(_) ->
	ok.

% ----------------------------------------------------------------------------------------------------------------------
% ACTUAL TESTS ---------------------------------------------------------------------------------------------------------
test_trigonometric_functions(_) ->
	[
		?_assert(almost_equal(0.0, tanh(0.0))),
		?_assert(almost_equal(1.0, tanh(10.0))),
		?_assert(almost_equal(-1.0, tanh(-10.0)))
	].

test_sigmoid_functions(_) ->
	[
		?_assert(almost_equal(0.0, sigmoid(-10.0))),
		?_assert(almost_equal(0.5, sigmoid(0.0))),
		?_assert(almost_equal(1.0, sigmoid(10.0)))
	].

% ----------------------------------------------------------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS --------------------------------------------------------------------------------------------
almost_equal(Ref_Val, Value) when is_list(Ref_Val), is_list(Value), length(Ref_Val) == length(Value) ->
	lists:all(fun(X) -> X end, [almost_equal(X, Y) || {X, Y} <- lists:zip(Ref_Val, Value)]);
almost_equal(Ref_Val, Value) ->
	Sup = Ref_Val + ?EQUAL_TOLERANCE,
	Inf = Ref_Val - ?EQUAL_TOLERANCE,
	if
		Sup > Value, Inf < Value ->
			true;
		true ->
			false
	end.


