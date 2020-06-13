%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(layer).

%% API
-export([create_type/1, input/2, output/2, dense/2, elu/2, tanh/2]).
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
%% @doc Creates a function that would return a layer with the desired
%% properties. 
%% @end
%%--------------------------------------------------------------------
-spec create_type(Properties::definition()) -> function().
-define(DEFAULT_PROPERTIES, #{activation  => direct,
                              aggregation => dot_prod,
                              initializer => glorot,
                              bias        => undefined}).
create_type(Properties) -> 
    fun(Units, Connections) -> 
        #{connections => Connections,
          units       => Units,
          data        => maps:merge(?DEFAULT_PROPERTIES, Properties)}
    end.

%%--------------------------------------------------------------------
%% @doc Returns the definition for an input layer.
%% @end
%%--------------------------------------------------------------------
-spec input(Units, Connections) -> model:layer() when 
    Units       :: integer(),
    Connections :: definition().
-define(INPUT_PROPERTIES,   #{activation  => direct,
                              aggregation => direct,
                              initializer => ones,
                              bias        => 0.0}).
input(Units, Connections) ->
        #{connections => Connections,
          units       => Units,
          data        => ?INPUT_PROPERTIES}.

%%--------------------------------------------------------------------
%% @doc Returns the definition for an output layer.
%% @end
%%--------------------------------------------------------------------
-spec output(Units, Connections) -> model:layer() when 
    Units       :: integer(),
    Connections :: definition().
-define(OUTPUT_PROPERTIES,  #{activation  => direct,
                              aggregation => dot_prod,
                              initializer => glorot,
                              bias        => 0.0}).
output(Units, Connections) ->
        #{connections => Connections,
          units       => Units,
          data        => ?OUTPUT_PROPERTIES}.

%%--------------------------------------------------------------------
%% @doc Returns the definition for a dense layer.
%% @end
%%--------------------------------------------------------------------
-spec dense(Units, Connections) -> model:layer() when 
    Units       :: integer(),
    Connections :: definition().
-define(DENSE_PROPERTIES,   #{activation  => direct,
                              aggregation => dot_prod,
                              initializer => glorot,
                              bias        => undefined}).
dense(Units, Connections) ->
        #{connections => Connections,
          units       => Units,
          data        => ?DENSE_PROPERTIES}.

%%--------------------------------------------------------------------
%% @doc Returns the definition for a elu layer.
%% @end
%%--------------------------------------------------------------------
-spec elu(Units, Connections) -> model:layer() when 
    Units       :: integer(),
    Connections :: definition().
-define(ELU_PROPERTIES,     #{activation  => elu,
                              aggregation => dot_prod,
                              initializer => glorot,
                              bias        => undefined}).
elu(Units, Connections) ->
        #{connections => Connections,
          units       => Units,
          data        => ?ELU_PROPERTIES}.

%%--------------------------------------------------------------------
%% @doc Returns the definition for a tanh layer.
%% @end
%%--------------------------------------------------------------------
-spec tanh(Units, Connections) -> model:layer() when 
    Units       :: integer(),
    Connections :: definition().
-define(TANH_PROPERTIES,     #{activation  => tanh,
                              aggregation => dot_prod,
                              initializer => glorot,
                              bias        => undefined}).
tanh(Units, Connections) ->
        #{connections => Connections,
          units       => Units,
          data        => ?TANH_PROPERTIES}.


%%====================================================================
%% Internal functions
%%====================================================================


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

