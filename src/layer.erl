%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(layer).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([dense/1, input/1, output/1]).
-export([dense/2, input/2, output/2, compile/2]).
-export_type([definition/0, compiled/0]).

-type definition() :: #{
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
%% @doc Returns the complilation definition for a dense layer.
%% @end
%%--------------------------------------------------------------------
-spec dense(Units) -> DenseLayer when 
    Units      :: integer(),
    DenseLayer :: definition().
dense(Units) ->
    dense(Units, #{}).

-spec dense(Units, Properties) -> DenseLayer when 
    Units      :: integer(),
    Properties :: properties(),
    DenseLayer :: definition().
dense(Units, Prop) ->
    #{
        units       => Units,
        activation  => maps:get(activation,  Prop, direct),
        aggregation => maps:get(aggregation, Prop, dot_prod),
        initializer => maps:get(initializer, Prop, glorot)
    }.

%%--------------------------------------------------------------------
%% @doc Returns the compilation definition for an input layer.
%% @end
%%--------------------------------------------------------------------
-spec input(Units) -> InputsLayer when 
    Units      :: integer(),
    InputsLayer :: definition().
input(Units) -> 
    input(Units, #{}).

-spec input(Units, Properties) -> InputsLayer when 
    Units      :: integer(),
    Properties :: properties(),
    InputsLayer :: definition().
input(Units, Prop) ->
    #{
        units       => Units,
        activation  => maps:get(activation,  Prop, direct),
        aggregation => maps:get(aggregation, Prop, direct),
        initializer => maps:get(initializer, Prop, ones  )
    }.

%%--------------------------------------------------------------------
%% @doc Returns the compilation definition for an output layer.
%% @end
%%--------------------------------------------------------------------
-spec output(Units) -> OutputsLayer when 
    Units      :: integer(),
    OutputsLayer :: definition().
output(Units) -> 
    output(Units, #{}).

-spec output(Units, Properties) -> OutputsLayer when 
    Units      :: integer(),
    Properties :: properties(),
    OutputsLayer :: definition().
output(Units, Prop) ->
    #{
        units       => Units,
        activation  => maps:get(activation,  Prop, direct),
        aggregation => maps:get(aggregation, Prop, dot_prod),
        initializer => maps:get(initializer, Prop, glorot)
    }.

%%--------------------------------------------------------------------
%% @doc Compiles a layer. Returns a tuple indicating the layer type 
%% together with the ids of all the neuron definition.
%% @end
%%--------------------------------------------------------------------
-spec compile(Coordinade, Definition) -> CompiledLayer when 
    Coordinade     :: float(),
    Definition :: definition(), 
    CompiledLayer  :: compiled().
compile(Coordinade, Definition) ->
    {Units, Prop} = maps:take(units, Definition),
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

