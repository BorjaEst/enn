%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(model).
-compile([export_all, nowarn_export_all]). %% TODO: To delete after build

-include_lib("eunit/include/eunit.hrl").

%% API
%%-export([]).
-export_type([nature/0, definition/0]).

-type nature() :: sequential | recurrent.
-type connections() :: {
    Type :: all, %% TODO: to define more types (half, random, etc)
    LayerId_From :: float(),
    LayerIds_To :: [float()]
}.
-type definition() :: #{
    connections := [connections()],
    layers      := #{
        Coordinade :: float() => Specs :: layer:definition()
    }
}.

-ifdef(debug_mode).
-define(STDCALL_TIMEOUT, infinity).
-else.
-define(STDCALL_TIMEOUT, 5000).
-endif.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns the definition for a sequential model from layers.
%% @end
%%--------------------------------------------------------------------
-spec sequential(Layers :: [layer:definition()]) ->
    Model_definition :: definition().
sequential(Layers) when length(Layers) > 1 ->
    Sequence = linspace(-1, + 1, length(Layers)),
    #{
        connections => sequential_connection(Sequence),
        layers => maps:from_list(lists:zip(Sequence, Layers))
    }.

%%--------------------------------------------------------------------
%% @doc Returns the definition for a recurrent model from layers.
%% RLevel indicates the number of lower layers that will be connected. 
%% @end
%%--------------------------------------------------------------------
-spec recurrent(Layers :: [layer:definition()], 
                RLevel :: integer()) ->
    Model_definition :: definition().
recurrent(Layers, RLevel) when length(Layers) > 1 ->
    Sequence = linspace(-1, + 1, length(Layers)),
    #{
        connections => sequential_connection(Sequence) ++ 
                       recurrent_connection(Sequence, RLevel),
        layers => maps:from_list(lists:zip(Sequence, Layers))
    }.

%%--------------------------------------------------------------------
%% @doc Compiles and stores a model in the DB returning its cortex_id.
%% @end
%%--------------------------------------------------------------------
-spec compile(Model :: definition()) -> 
    Cortex_id :: cortex:id().
compile(Model) ->
    #{connections:=Connections, layers:=Layers} = Model,
    CompiledLayers = maps:map(fun layer:compile/2, Layers),
    cortex:new(
        _NInputs  = enn:inputs(Model),
        _NOutputs = enn:outputs(Model),
        _Graph    = graph(CompiledLayers, Connections)
    ).


%%====================================================================
%% Internal functions
%%====================================================================

% -------------------------------------------------------------------
linspace(From, To, N) ->
    Step = (To - From) / (N - 1),
    [round((From + Step * X) * 100) / 100 || X <- lists:seq(0, N - 1)].

% -------------------------------------------------------------------
sequential_connection([Layer_A, Layer_B | Rest] = _Layers) ->
    [{all, Layer_A, [Layer_B]} | sequential_connection([Layer_B | Rest])];
sequential_connection([_Layer_A] = _Layers) ->
    [].

% -------------------------------------------------------------------
recurrent_connection(Layers, RLevel) ->
    recurrent_connection_aux(lists:reverse(Layers), RLevel).

recurrent_connection_aux([_Last_Layer], _RLevel) ->
    [];
recurrent_connection_aux([Layer_A | Rest], RLevel) when length(Rest) >= RLevel ->
    ToConnect = lists:sublist(Rest, RLevel),
    [{all, Layer_A, ToConnect} | recurrent_connection_aux(Rest, RLevel)];
recurrent_connection_aux([Layer_A | Rest], RLevel) ->
    [{all, Layer_A, Rest} | recurrent_connection_aux(Rest, RLevel)].

% -------------------------------------------------------------------
graph(CompiledLayers, Connections) -> 
    Graph = digraph:new([cyclic]),
    add_vertex(CompiledLayers, Graph),
    add_edges(CompiledLayers, Connections, Graph),
    serialize_graph(Graph).

add_vertex(CompiledLayers, G) -> 
    Neurons = lists:append(maps:values(CompiledLayers)),
    [digraph:add_vertex(G, elements:id(N)) || N <- Neurons].

add_edges(CompiledLayers, [{all,IFrom,IxTo} | Connections], G) -> 
    To     = lists:append([maps:get(X, CompiledLayers) || X <- IxTo]),
    From   = maps:get(IFrom, CompiledLayers),
    [digraph:add_edge(G, V1, V2, uninitialized) || V1<-From, V2<-To],
    add_edges(CompiledLayers, Connections, G);
add_edges(_CompiledLayers, [], Graph) ->
    Graph.

serialize_graph({digraph, V, E, N, B}) ->
    {ets:tab2list(V), ets:tab2list(E), ets:tab2list(N), B}.


%%====================================================================
%% Eunit white box tests
%%====================================================================

% -------------------------------------------------------------------
% TESTS DESCRIPTIONS ------------------------------------------------
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

% -------------------------------------------------------------------
% SPECIFIC SETUP FUNCTIONS ------------------------------------------
no_setup() ->
    ok.

no_cleanup(_) ->
    ok.

% -------------------------------------------------------------------
% ACTUAL TESTS ------------------------------------------------------
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

% -------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS -----------------------------------------


