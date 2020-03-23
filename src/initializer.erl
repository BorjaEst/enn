%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc Module containing all the initialisation methods for the new
%%% inputs weights.
%%%
%%% TODO: Implement zeros
%%% TODO: Implement ones
%%% TODO: Implement constant
%%% TODO: Implement random_normal
%%% TODO: Implement random_uniform
%%% TODO: Implement truncated_normal
%%% TODO: Implement variance_scaling
%%% TODO: Implement orthogonal
%%% TODO: Implement identity
%%% TODO: Implement lecun_uniform
%%% TODO: Implement glorot_normal
%%% TODO: Implement glorot_uniform
%%% TODO: Implement he_normal
%%% TODO: Implement lecun_normal
%%% TODO: Implement he_uniform
%%% @end
%%%-------------------------------------------------------------------
-module(initializer).

-include_lib("math_constants.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([apply/2]).
-export_type([func/0]).

-type func() :: zeros | ones | {constant, Value :: float()} | 
                random_normal | random_uniform | truncated_normal | 
                variance_scaling | orthogonal | identity | 
                lecun_uniform | glorot_normal | glorot_uniform | 
                he_normal | lecun_normal | he_uniform.


%%%===================================================================
%%% API
%%%===================================================================

% ....................................................................
% TODO: Define specs and comments
apply(zeros, Arg)             -> zeros(Arg);
apply(random_normal, Arg)     -> error(not_defined);
apply(glorot_uniform, Arg)    -> error(not_defined);
apply(_Ref, _)                -> error(not_defined).



%%====================================================================
%% Internal functions
%%====================================================================

% ....................................................................
% TODO: Define specs and comments
zeros(_) -> 
    0.0.


%%====================================================================
%% Eunit white box tests
%%====================================================================

% --------------------------------------------------------------------
% TESTS DESCRIPTIONS -------------------------------------------------



% --------------------------------------------------------------------
% SPECIFIC SETUP FUNCTIONS -------------------------------------------



% --------------------------------------------------------------------
% ACTUAL TESTS -------------------------------------------------------


% --------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS ------------------------------------------

