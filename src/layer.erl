%%%-------------------------------------------------------------------
%%% @author borja
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Aug 2018 14:25
%%%-------------------------------------------------------------------
-module(layer).

-include_lib("eunit/include/eunit.hrl").
-include_lib("nnelements.hrl").

%% API
-export([dense/2, input/2, output/2, compile/2]).
-export_type([type_id/0, specifications/0]).

-type type_id() :: dense | input | output.
-type specifications() :: #{
	type := type_id(),
	units := integer(),
	activation := activation:func(),
	aggregation := aggregation:func(),
	options := []}. %% TODO: create neuron:options

-type properties() :: #{
	activation => activation:func(),
	aggregation => aggregation:func()}.


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
-spec dense(Units :: integer(), Properties :: properties()) ->
	DenseLayer :: specifications().
dense(Units, Properties) ->
	#{
		type        => dense,
		units       => Units,
		activation  => maps:get(activation, Properties, sigmoid),
		aggregation => maps:get(aggregation, Properties, dotprod),
		options     => []
	}.

%%--------------------------------------------------------------------
%% @doc Returns the compilation specifications for an input layer.
%% @end
%%--------------------------------------------------------------------
-spec input(Units :: integer(), Properties :: properties()) ->
	InputsLayer :: specifications().
input(Units, Properties) ->
	#{
		type        => input,
		units       => Units,
		activation  => maps:get(activation, Properties, tanh),
		aggregation => maps:get(aggregation, Properties, direct),
		options     => []
	}.

%%--------------------------------------------------------------------
%% @doc Returns the compilation specifications for an output layer.
%% @end
%%--------------------------------------------------------------------
-spec output(Units :: integer(), Properties :: properties()) -> 
	OutputsLayer :: specifications().
output(Units, Properties) ->
	#{
		type        => output,
		units       => Units,
		activation  => maps:get(activation, Properties, tanh),
		aggregation => maps:get(aggregation, Properties, dotprod),
		options     => []
	}.

%%--------------------------------------------------------------------
%% @doc Compiles a layer. Returns a tuple indicating the layer type 
%% together with the ids of all the neuron specifications.
%% @end
%%--------------------------------------------------------------------
-spec compile(Coordinade :: float(), Spec :: specifications()) ->
	CompiledLayer :: {Type :: type_id(), [NId :: neuron_id()]}.
compile(Coordinade, Spec) ->
	#{
		type        := Type,
		units       := Units,
		activation  := AF,
		aggregation := AggrF,
		options     := Options
	} = Spec,
	{Type, [neuron:new(Coordinade, AF, AggrF, Options) || 
								 _ <- lists:seq(1, Units)]}.


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

