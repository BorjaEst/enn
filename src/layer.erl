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

-type definition() :: #{units       := integer(),
                        activation  := activation:func(),
                        aggregation := aggregation:func(),
                        initializer := initializer:func()}.
-type compiled() :: [Neuron::enn:neuron()].


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
    Properties :: #{activation  => activation:func(),
                    aggregation => aggregation:func(),
                    initializer => initializer:func()},
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
    Units       :: integer(),
    InputsLayer :: definition().
input(Units) -> 
    input(Units, #{}).

-spec input(Units, Properties) -> InputsLayer when 
    Units       :: integer(),
    Properties :: #{activation  => activation:func()},
    InputsLayer :: definition().
input(Units, Prop) ->
    #{
        units       => Units,
        activation  => maps:get(activation, Prop, direct),
        aggregation => direct,
        initializer => ones
    }.

%%--------------------------------------------------------------------
%% @doc Returns the compilation definition for an output layer.
%% @end
%%--------------------------------------------------------------------
-spec output(Units) -> OutputsLayer when 
    Units        :: integer(),
    OutputsLayer :: definition().
output(Units) -> 
    output(Units, #{}).

-spec output(Units, Properties) -> OutputsLayer when 
    Units        :: integer(),
    Properties :: #{activation  => activation:func(),
                    aggregation => aggregation:func(),
                    initializer => initializer:func()},
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
%% Should run inside a mnesia transaction.
%% @end
%%--------------------------------------------------------------------
-spec compile(Key, Definition) -> CompiledLayer when 
    Key           :: term(),
    Definition    :: definition(), 
    CompiledLayer :: compiled().
compile(_Key, Definition) ->
    Units = maps:get(units, Definition),
    [nnode:new(Definition) || _ <- lists:seq(1, Units)].


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

