%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc Module containing all the initialisation methods for the new
%%% inputs weights.
%%%
%%% TODO: Implement orthogonal
%%% TODO: Implement identity
%%% @end
%%%-------------------------------------------------------------------
-module(initializer).

-include_lib("math_constants.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([apply/2]).
-export_type([func/0, arguments/0]).

-type func() :: zeros | ones | constant | random | 
                variance_scaling | orthogonal | lecun |
                glorot | he.

-define(ARG(Key, Arguments), maps:get(Key, Arguments)).
-define(ARG(Key, Arg, Def),  maps:get(Key, Arg, Def)).
-type arguments() :: #{
    fan_in       := integer(),
    fan_out      := integer(),
    value        => Value :: float(),
    distribution => normal | uniform,
    mode         => fan_in | fan_out | fan_all | fan_avg,
    mean         => Mean   :: float(),
    stddev       => Stddev :: float(),
    scale        => Scale  :: float(),
    maxval       => Value  :: float(),
    minval       => Value  :: float()
}.


%%%===================================================================
%%% API
%%%===================================================================

% ....................................................................
% TODO: Define specs and comments
apply(zeros,    Arg) -> zeros(Arg);
apply(ones,     Arg) -> ones(Arg);
apply(constant, Arg) -> constant(Arg);
apply(random,   Arg) -> random(Arg);
apply(lecun,    Arg) -> vscaling(Arg#{mode => fan_in,  scale => 1.0});
apply(glorot,   Arg) -> vscaling(Arg#{mode => fan_all, scale => 2.0});
apply(he,       Arg) -> vscaling(Arg#{mode => fan_in,  scale => 2.0});
apply(variance_scaling, Arg) -> vscaling(Arg);
apply(_Ref,     _) -> error(not_defined).


%%====================================================================
%% Internal functions
%%====================================================================

% ....................................................................
% TODO: Define specs and comments
zeros(_) -> 
    0.0.

zeros_test() -> 
    Arg = #{},
    ?assertEqual(0.0, zeros(Arg)).

% ....................................................................
% TODO: Define specs and comments
ones(_) -> 
    1.0.

ones_test() -> 
    Arg = #{},
    ?assertEqual(1.0, ones(Arg)).

% ....................................................................
% TODO: Define specs and comments
constant(Arg) -> 
    ?ARG(value, Arg).

constant_test() -> 
    F = fun(V) -> {V, #{value => V}} end,
    L = [F(rand:uniform(10)) || _ <- lists:seq(1, 9)],
    [?assertEqual(V, constant(Arg)) || {V, Arg} <- L].

% ....................................................................
% TODO: Define specs and comments
random(#{distribution := normal} = Arg) -> 
    Mean   = ?ARG(mean, Arg, 0.00),
    Stddev = ?ARG(stddev, Arg, 0.05),
    rand:normal(Mean, Stddev);
random(#{distribution := uniform} = Arg) -> 
    Maxval = ?ARG(maxval, Arg, +0.05),
    Minval = ?ARG(minval, Arg, -0.05),
    (Maxval - Minval)*rand:uniform() - Minval.

random_test() -> 
    % Test the normal distribution 
    ?assert(0.1 > lists:sum([random(#{distribution => normal}) 
                                || _<- lists:seq(1, 9)])),
    % Test the uniform distribution
    [?assert(+0.05 > random(#{distribution => uniform})) 
        || _<- lists:seq(1, 9)],
    [?assert(-0.05 < random(#{distribution => uniform})) 
        || _<- lists:seq(1, 9)],
    % Undefined distribution raises an error
    ?assertError(function_clause, random(#{})).
    
% ....................................................................
% TODO: Define specs and comment
vscaling(Arg) -> 
    Scale = ?ARG(scale, Arg, 1.0),
    N = case ?ARG(mode, Arg, fan_in) of
        fan_in  ->  ?ARG(fan_in,  Arg);
        fan_out ->  ?ARG(fan_out, Arg);
        fan_all ->  ?ARG(fan_in, Arg) + ?ARG(fan_out, Arg);
        fan_avg -> (?ARG(fan_in, Arg) + ?ARG(fan_out, Arg))/2
    end,
    case ?ARG(distribution, Arg, normal) of 
        normal  ->  
            Stddev = math:sqrt(Scale/N),
            random(Arg#{stddev => Stddev});
        uniform -> 
            Limit = math:sqrt(3*Scale/N),
            random(Arg#{maxval => Limit, minval => -Limit})
    end.

vscaling_test() -> 
    % TODO: to be defined,
    ok.


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

