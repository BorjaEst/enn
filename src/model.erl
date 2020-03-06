%%%-------------------------------------------------------------------
%%% @author borja
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Aug 2018 14:25
%%%-------------------------------------------------------------------
-module(model).
-compile([export_all, nowarn_export_all]). %% TODO: To delete after build

-include_lib("eunit/include/eunit.hrl").

%% API
%%-export([]).
-export_type([specifications/0]).

-type connections() :: {
	Type :: all, %% TODO: to define more types (half, random, etc)
	LayerId_From :: float(),
	LayerIds_To :: [float()]
}.
-type specifications() :: #{
	name := atom() | string(),
	connections := connections(),
	layers := #{
		LayerId :: float() => Specs :: layer:specifications()
		},
	options := [Option :: term()]  %% TODO: Define options
}.

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
%% @doc Returns the specifications for a sequential model from layers.
%% @end
%%--------------------------------------------------------------------
-spec sequential(Layers :: [layer:specifications()], Name :: atom()) ->
	Model_specifications :: specifications().
sequential(Layers, Name) when length(Layers) > 1 ->
	Sequence = linspace(-1, + 1, length(Layers)),
	#{
		name => Name,
		connections => sequential_connection(Sequence),
		layers => maps:from_list(lists:zip(Sequence, Layers)),
		options => []
	}.

%%--------------------------------------------------------------------
%% @doc Returns the specifications for a recurrent model from layers.
%% RLevel indicates the number of lower layers that will be connected. 
%% @end
%%--------------------------------------------------------------------
-spec recurrent(Layers :: [layer:specifications()], 
			    RLevel :: integer(), Name :: atom()) ->
	Model_specifications :: specifications().
recurrent(Layers, RLevel, Name) when length(Layers) > 1 ->
	Sequence = linspace(-1, + 1, length(Layers)),
	#{
		name => Name,
		connections => sequential_connection(Sequence) ++ recurrent_connection(Sequence, RLevel),
		layers => maps:from_list(lists:zip(Sequence, Layers)),
		options => []
	}.

%%--------------------------------------------------------------------
%% @doc Compiles and stores a model in the DB returning its cortex_id.
%% @end
%%--------------------------------------------------------------------
-spec compile(Model :: specifications()) -> 
	Cortex_id :: cortex:id().
compile(Model) ->
	#{
		name := Name,
		connections := Connections,
		layers := Layers,
		options := Options
	} = Model,
	Compiled_Layers = maps:map(fun layer:compile/2, Layers),
	Cortex_Id = cortex:new(Name, Compiled_Layers, Options),
	ok = connect_layers(Connections, Compiled_Layers),
	Cortex_Id.


%%====================================================================
%% Internal functions
%%====================================================================

%.......................................................................................................................
linspace(From, To, N) ->
	Step = (To - From) / (N - 1),
	[round((From + Step * X) * 100) / 100 || X <- lists:seq(0, N - 1)].

% ......................................................................................................................
sequential_connection([Layer_A, Layer_B | Rest] = _Layers) ->
	[{all, Layer_A, [Layer_B]} | sequential_connection([Layer_B | Rest])];
sequential_connection([_Layer_A] = _Layers) ->
	[].

% ......................................................................................................................
recurrent_connection(Layers, RLevel) ->
	recurrent_connection_aux(lists:reverse(Layers), RLevel).

recurrent_connection_aux([_Last_Layer], _RLevel) ->
	[];
recurrent_connection_aux([Layer_A | Rest], RLevel) when length(Rest) >= RLevel ->
	{ToConnect, _} = lists:split(RLevel, Rest),
	[{all, Layer_A, ToConnect} | recurrent_connection_aux(Rest, RLevel)];
recurrent_connection_aux([Layer_A | Rest], RLevel) ->
	[{all, Layer_A, Rest} | recurrent_connection_aux(Rest, RLevel)].

%.......................................................................................................................
connect_layers([{Type, Layer_A, Layers_To} | Rest], Layers_elements) ->
	ElementsLayer_A = layer_elements(Layer_A, Layers_elements),
	Elements_To = lists:append([layer_elements(L_To, Layers_elements) || L_To <- Layers_To]),
	connect(Type, ElementsLayer_A, Elements_To),
	connect_layers(Rest, Layers_elements);
connect_layers([], _Layers_elements) ->
	ok.

layer_elements(Layer, Layers_elements) ->
	{_Type, ElementsLayer} = maps:get(Layer, Layers_elements),
	ElementsLayer.

%.......................................................................................................................
connect(all, Elements_From, Elements_To) ->
	[mutation:create_link(From, To) || From <- Elements_From, To <- Elements_To].


%%====================================================================
%% Eunit white box tests
%%====================================================================

% ----------------------------------------------------------------------------------------------------------------------
% TESTS DESCRIPTIONS ---------------------------------------------------------------------------------------------------
linspace_test_() ->
	% {setup, Where, Setup, Cleanup, Tests | Instantiator}
	[
		{"The linspace function retunrs a list of N element from 'From' to 'To' ",
		 {setup, local, fun no_setup/0, fun no_cleanup/1, fun test_for_linspace/1}},
		{"The sequential_connection function retunrs a list of connections ",
		 {setup, local, fun no_setup/0, fun no_cleanup/1, fun test_for_sequential_connection/1}},
		{"The recurrent_connection function retunrs a list of connections ",
		 {setup, local, fun no_setup/0, fun no_cleanup/1, fun test_for_recurrent_connection/1}}
	].

% ----------------------------------------------------------------------------------------------------------------------
% SPECIFIC SETUP FUNCTIONS ---------------------------------------------------------------------------------------------
no_setup() ->
	ok.

no_cleanup(_) ->
	ok.

% ----------------------------------------------------------------------------------------------------------------------
% ACTUAL TESTS ---------------------------------------------------------------------------------------------------------
test_for_linspace(_) ->
	[
		?_assertEqual([-1.0, 0.0, 1.0, 2.0], linspace(-1, 2, 4)),
		?_assertEqual([-1.0, 1.0], linspace(-1, 1, 2)),
		?_assertEqual([-1.0, 0.0, 1.0], linspace(-1, 1, 3)),
		?_assertEqual([-1.0, -0.5, 0.0, 0.5, 1.0], linspace(-1, 1, 5))
	].

test_for_sequential_connection(_) ->
	Layers = [-1.0, 0.0, 1.0],
	[
		?_assertEqual([
			              {all, -1.0, [0.0]},
			              {all, 0.0, [1.0]}
		              ], sequential_connection(Layers))
	].

test_for_recurrent_connection(_) ->
	Layers = [-1.0, 0.0, 1.0],
	[
		?_assertEqual([
			              {all, 1.0, []},
			              {all, 0.0, []}
		              ], recurrent_connection(Layers, _RLevel = 0)),
		?_assertEqual([
			              {all, 1.0, [0.0]},
			              {all, 0.0, [-1.0]}
		              ], recurrent_connection(Layers, _RLevel = 1)),
		?_assertEqual([
			              {all, 1.0, [0.0, -1.0]},
			              {all, 0.0, [-1.0]}
		              ], recurrent_connection(Layers, _RLevel = 2))
	].

% ----------------------------------------------------------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS --------------------------------------------------------------------------------------------


