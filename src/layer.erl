%%%-------------------------------------------------------------------
%%% @author borja
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Aug 2018 14:25
%%%-------------------------------------------------------------------
-module(layer).
-compile([export_all, nowarn_export_all]). %% TODO: To delete after build

-include_lib("eunit/include/eunit.hrl").
-include_lib("nnelements.hrl").

-define(MIN_LINK_WEIGHT, 0.05).

%% API
%%-export([]).
-export_type([layer/0]).


-type type_id() :: dense | input | output.
-type layer() :: #{type := type_id(),
				   units := integer(),
				   activation := activation_function:activation_function(),
				   activation_function := activation_function:activation_function(),
				   options := []}. %% TODO: create neuron:options


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
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
% TODO: To make description and specs
dense(Units, Properties) ->
	#{
		type        => dense,
		units       => Units,
		activation  => maps:get(activation, Properties, sigmoid),
		aggregation => maps:get(aggregation, Properties, dotprod),
		options     => []
	}.

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
% TODO: To make description and specs
input(Units, Properties) ->
	#{
		type        => input,
		units       => Units,
		activation  => maps:get(activation, Properties, tanh),
		aggregation => maps:get(aggregation, Properties, direct),
		options     => []
	}.

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
% TODO: To make description and specs
output(Units, Properties) ->
	#{
		type        => output,
		units       => Units,
		activation  => maps:get(activation, Properties, tanh),
		aggregation => maps:get(aggregation, Properties, dotprod),
		options     => []
	}.

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
% TODO: To make description and specs
compile(LayerCoordinate, LayerProperties) ->
	#{
		type        := Type,
		units       := Units,
		activation  := AF,
		aggregation := AggrF,
		options     := Options
	} = LayerProperties,
	{Type, [neuron:new(LayerCoordinate, AF, AggrF, Options) || _ <- lists:seq(1, Units)]}.


%%====================================================================
%% Internal functions
%%====================================================================


%%====================================================================
%% Eunit white box tests
%%====================================================================

% ----------------------------------------------------------------------------------------------------------------------
% TESTS DESCRIPTIONS ---------------------------------------------------------------------------------------------------

% ----------------------------------------------------------------------------------------------------------------------
% SPECIFIC SETUP FUNCTIONS ---------------------------------------------------------------------------------------------
no_setup() ->
	ok.

no_cleanup(_) ->
	ok.

% ----------------------------------------------------------------------------------------------------------------------
% ACTUAL TESTS ---------------------------------------------------------------------------------------------------------

% ----------------------------------------------------------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS --------------------------------------------------------------------------------------------

