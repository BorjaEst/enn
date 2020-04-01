%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%%
%%% TODO: Implement crelu
%%% TODO: Implement relu_x
%%% TODO: Implement dropout (Probably this is not an activation)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(activation).

-include_lib("math_constants.hrl").
-include_lib("enn_logger.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(EQUAL_TOLERANCE, 0.001).

%% API
-export([func/2, dfun/2, beta/3]).
-export_type([func/0]).

-type func() :: direct | sigmoid | tanh | softplus | softsign 
              | elu | selu | relu | crelu | relu_x | dropout.


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Applies the indicated activation funtion.
%% @end
%%--------------------------------------------------------------------
-spec func(Function :: func(), Soma :: float()) -> 
    Result :: float().
func(Function, Soma) -> 
    ?LOG_ACTIVATION_FUNCTION_REQUEST(Function, Soma),
    Result = apply_fun(Function, Soma),
    ?LOG_ACTIVATION_FUNCTION_RESULT(Function, Result),
    Result.

apply_fun(direct,   Soma) -> direct(Soma);
apply_fun(sigmoid,  Soma) -> sigmoid(Soma);
apply_fun(tanh,     Soma) -> tanh(Soma);
apply_fun(softplus, Soma) -> softplus(Soma);
apply_fun(softsign, Soma) -> softsign(Soma);
apply_fun(elu,      Soma) -> elu(Soma);
apply_fun(selu,     Soma) -> selu(Soma);
apply_fun(relu,     Soma) -> relu(Soma);
apply_fun(_Ref,    _Soma) -> error(not_defined).

%%--------------------------------------------------------------------
%% @doc Applies the derivade of the indicated activation funtion.
%% @end
%%--------------------------------------------------------------------
-spec dfun(Function :: func(), Soma :: float()) -> 
    Result :: float().
dfun(Function, Soma) -> 
    ?LOG_ACTIVATION_DERIVADE_REQUEST(Function, Soma),
    Result = apply_dfun(Function, Soma),
    ?LOG_ACTIVATION_DERIVADE_RESULT(Function, Result),
    Result.

apply_dfun(direct,   Soma) -> d_direct(Soma);
apply_dfun(sigmoid,  Soma) -> d_sigmoid(Soma);
apply_dfun(tanh,     Soma) -> d_tanh(Soma);
apply_dfun(softplus, Soma) -> d_softplus(Soma);
apply_dfun(softsign, Soma) -> d_softsign(Soma);
apply_dfun(elu,      Soma) -> d_elu(Soma);
apply_dfun(selu,     Soma) -> d_selu(Soma);
apply_dfun(relu,     Soma) -> d_relu(Soma);
apply_dfun(_Ref,    _Soma) -> error(not_defined).

%%--------------------------------------------------------------------
%% @doc Applies the beta calculation for the activation funtion.
%% @end
%%--------------------------------------------------------------------
-spec beta(Function :: func(), Error :: float(), Soma :: float()) -> 
    Result :: float().
beta(Function, Error, Soma) -> 
    ?LOG_ACTIVATION_BETA_REQUEST(Function, Error, Soma),
    Result = apply_beta(Function, Error, Soma),
    ?LOG_ACTIVATION_BETA_RESULT(Function, Result),
    Result.

apply_beta(Function, Error, Soma) when Error =< 0 ->
    if
        Soma =< 0 -> Error * dfun(Function, Soma);
        true      -> Error
    end;
apply_beta(Function, Error, Soma) ->
    - apply_beta(Function, -Error, -Soma).


%%====================================================================
%% Internal functions
%%====================================================================

% ....................................................................
% TODO: Define specs and comments
direct(X) -> X.

% TODO: Define specs and comments
d_direct(_) -> 1.0.

% ....................................................................
% TODO: Define specs and comments
sigmoid(X) when X > 10.0  -> 1.0;
sigmoid(X) when X < -10.0 -> 0.0;
sigmoid(X) -> %(0 : 1)--Der:Y*(1-Y) %TODO: To check
    1 / (1 + math:exp(-X)).

% TODO: Define specs and comments
d_sigmoid(X) ->
    Sig = sigmoid(X),
    Sig * (1 - Sig).

% ....................................................................
% TODO: Define specs and comments
tanh(X) ->
    math:tanh(X).

% TODO: Define specs and comments
d_tanh(X) ->
    1 - math:pow(tanh(X), 2).

% ....................................................................
% TODO: Define specs and comments
softplus(X) ->
    math:log(1 + math:exp(X)).

% TODO: Define specs and comments
d_softplus(X) ->
    1 / (1 + math:exp(X)).

% ....................................................................
% TODO: Define specs and comments
softsign(X) ->
    X / (1 + abs(X)).

% TODO: Define specs and comments
d_softsign(X) ->
    1 / math:pow(1 + abs(X), 2).

% ....................................................................
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

% ....................................................................
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

% ....................................................................
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

% --------------------------------------------------------------------
% TESTS DESCRIPTIONS -------------------------------------------------
this_example_test_() ->
    % {setup, Where, Setup, Cleanup, Tests | Instantiator}
    [
        {"Test for the trigonometrical activation functions",
         {setup, local, fun no_setup/0, fun no_cleanup/1, 
          fun test_trigonometric_functions/1}},
        {"Test for sigmoid activation functions",
         {setup, local, fun no_setup/0, fun no_cleanup/1, 
          fun test_sigmoid_functions/1}}
    ].

% --------------------------------------------------------------------
% SPECIFIC SETUP FUNCTIONS -------------------------------------------
no_setup() ->
    ok.

no_cleanup(_) ->
    ok.

% --------------------------------------------------------------------
% ACTUAL TESTS -------------------------------------------------------
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

% --------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS ------------------------------------------

% almost_equal({Ref_Val, Value}) -> 
%     almost_equal(Ref_Val, Value);
% almost_equal(RefV_ValList) ->
%     lists:all(fun almost_equal/1, RefV_ValList).

almost_equal(Ref_Val, Value) ->
    Sup = Ref_Val + ?EQUAL_TOLERANCE,
    Inf = Ref_Val - ?EQUAL_TOLERANCE,
    if
        Value > Sup -> false;
        Value < Inf -> false;
        true -> true
    end.


