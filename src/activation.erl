%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(activation).

-include_lib("math_constants.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(EQUAL_TOLERANCE, 0.001).

%% API
-export([apply/2, beta/3]).
-export_type([func/0]).

-type func() :: direct | sigmoid | tanh | softplus | softsign 
              | elu | selu | relu | crelu | relu_x | dropout.


%%%===================================================================
%%% API
%%%===================================================================

% ....................................................................
% TODO: Define specs and comments
apply(sigmoid, Soma)  -> sigmoid(Soma);
apply(tanh, Soma)     -> tanh(Soma);
apply(softplus, Soma) -> softplus(Soma);
apply(softsign, Soma) -> softsign(Soma);
apply(elu, Soma)      -> elu(Soma);
apply(selu, Soma)     -> selu(Soma);
apply(relu, Soma)     -> relu(Soma);
apply(crelu, _Soma)   -> error(not_defined); %% TODO: To implement
apply(relu_x, _Soma)  -> error(not_defined); %% TODO: To implement
apply(dropout, _Soma) -> error(not_defined); %% TODO: To implement
apply(_Ref, _)        -> error(not_defined).

% ....................................................................
% TODO: Define specs and comments
beta(sigmoid, Err, Soma)   -> do_beta(Err, Soma, fun d_sigmoid/1);
beta(tanh, Err, Soma)      -> do_beta(Err, Soma, fun d_tanh/1);
beta(softplus, Err, Soma)  -> do_beta(Err, Soma, fun d_softplus/1);
beta(softsign, Err, Soma)  -> do_beta(Err, Soma, fun d_softsign/1);
beta(elu, Err, Soma)       -> do_beta(Err, Soma, fun d_elu/1);
beta(selu, Err, Soma)      -> do_beta(Err, Soma, fun d_selu/1);
beta(relu, Err, Soma)      -> do_beta(Err, Soma, fun d_relu/1);
beta(crelu, _Err, _Soma)   -> error(not_defined); %% TODO: To implement
beta(relu_x, _Err, _Soma)  -> error(not_defined); %% TODO: To implement
beta(dropout, _Err, _Soma) -> error(not_defined); %% TODO: To implement
beta(_Ref, _Err, _Soma)    -> error(not_defined).

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


