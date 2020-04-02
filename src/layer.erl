%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(layer).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([dense/2, input/2, output/2, compile/2]).
-export_type([specifications/0, compiled/0]).

-type specifications() :: #{
    units       := integer(),
    activation  := activation:func(),
    aggregation := aggregation:func(),
    initializer := initializer:func()
}.
-type properties() :: #{
    activation  => activation:func(),
    aggregation => aggregation:func(),
    initializer => initializer:func()
}.
-type compiled() :: [Neuron_Id :: neuron:id()].


-ifdef(debug_mode).
-define(LOG(X), io:format("{~p,~p,~p}: ~p~n", [self(), ?MODULE, ?LINE, X])).
-define(STDCALL_TIMEOUT, infinity).
-else.
-define(LOG(X), true).
-define(STDCALL_TIMEOUT, 5000).
-endif.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns the complilation specifications for a dense layer.
%% @end
%%--------------------------------------------------------------------
-spec dense(Units, Properties) -> DenseLayer when 
    Units      :: integer(),
    Properties :: properties(),
    DenseLayer :: specifications().
dense(Units, Prop) ->
    #{
        units       => Units,
        activation  => maps:get(activation,  Prop, sigmoid),
        aggregation => maps:get(aggregation, Prop, dot_prod),
        initializer => maps:get(initializer, Prop, glorot)
    }.

%%--------------------------------------------------------------------
%% @doc Returns the compilation specifications for an input layer.
%% @end
%%--------------------------------------------------------------------
-spec input(Units, Properties) -> InputsLayer when 
    Units      :: integer(),
    Properties :: properties(),
    InputsLayer :: specifications().
input(Units, Prop) ->
    #{
        units       => Units,
        activation  => maps:get(activation,  Prop, direct),
        aggregation => maps:get(aggregation, Prop, direct),
        initializer => maps:get(initializer, Prop, ones  )
    }.

%%--------------------------------------------------------------------
%% @doc Returns the compilation specifications for an output layer.
%% @end
%%--------------------------------------------------------------------
-spec output(Units, Properties) -> OutputsLayer when 
    Units      :: integer(),
    Properties :: properties(),
    OutputsLayer :: specifications().
output(Units, Prop) ->
    #{
        units       => Units,
        activation  => maps:get(activation,  Prop, tanh),
        aggregation => maps:get(aggregation, Prop, dot_prod),
        initializer => maps:get(initializer, Prop, glorot)
    }.

%%--------------------------------------------------------------------
%% @doc Compiles a layer. Returns a tuple indicating the layer type 
%% together with the ids of all the neuron specifications.
%% @end
%%--------------------------------------------------------------------
-spec compile(Coordinade, Specifications) -> CompiledLayer when 
    Coordinade     :: float(),
    Specifications :: specifications(), 
    CompiledLayer  :: compiled().
compile(Coordinade, Specifications) ->
    {Units, Prop} = maps:take(units, Specifications),
    [neuron:new(Coordinade, Prop) || _ <- lists:seq(1, Units)].


%%====================================================================
%% Internal functions
%%====================================================================


%%====================================================================
%% Eunit white box tests
%%====================================================================
%% TODO: To implement some eunit tests

% --------------------------------------------------------------------
% TESTS DESCRIPTIONS -------------------------------------------------

% --------------------------------------------------------------------
% SPECIFIC SETUP FUNCTIONS -------------------------------------------

% --------------------------------------------------------------------
% ACTUAL TESTS -------------------------------------------------------

% --------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS ------------------------------------------

