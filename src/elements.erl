%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(elements).
-compile([export_all, nowarn_export_all]). %% TODO: To delete after build

-include_lib("math_constants.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
%%-export([]).
-export_type([type/0, cortex/0, neuron/0]).

%%% Neuronal networks are composed mostly by 2 types:
-type type()   :: cortex | neuron.
-type id()     :: neuron:id() | cortex:id().
-type weight() :: float() | cortex | undef.

-define(NEW_CORTEX_ID, {make_ref(), cortex}).
-record(cortex, {
    id               :: cortex:id(),
    layers = #{}     :: #{Layer :: float() => [neuron:neuron_id()]},
    outputs_ids = [] :: [neuron:id()], % Output neurons
    inputs_ids  = [] :: [neuron:id()]  % Input neurons
}).
-type cortex() :: #cortex{}.

-define(NEW_NEURON_ID(Coord), {{Coord, make_ref()}, neuron}).
-record(neuron, {
    id = ?NEW_NEURON_ID(0.0) :: neuron:id(),
    activation               :: activation:func(),
    aggregation              :: aggregation:func(),
    initializer              :: initializer:func(),
    outputs_ids     = []     :: [ id()], % Direct outputs & inputs
    inputs_idps     = []     :: [{id(), Weight :: weight()}],
    rcc_outputs_ids = []     :: [ id()], % Recurrent outputs & inputs
    rcc_inputs_idps = []     :: [{id(), Weight :: weight()}],
    bias                     :: float()
}).  
-type neuron() :: #neuron{}.


-ifdef(debug_mode).
-define(STDCALL_TIMEOUT, infinity).
-else.
-define(STDCALL_TIMEOUT, 5000).
-endif.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns the record fields of an element.
%% @end
%%--------------------------------------------------------------------
-spec fields(Atom :: neuron | cortex) -> ListOfFields :: [atom()].
fields(neuron) -> record_info(fields, neuron);
fields(cortex) -> record_info(fields, cortex).

%%--------------------------------------------------------------------
%% @doc Returns a neuron. Defaults are:
%% - Layer: 0.0 (must be between [-1.0, +1.0])
%% - Activation function: direct
%% - Agregation function: dotprod
%% @end
%%--------------------------------------------------------------------
-spec neuron(Coordinade, Properties) -> neuron() when
    Coordinade :: integer(),
    Properties :: neuron:properties().
neuron(Coordinade, Properties) ->
    edit(#neuron{id = ?NEW_NEURON_ID(Coordinade)}, Properties).

%%--------------------------------------------------------------------
%% @doc Evaluates if the input is a neuron.
%% @end
%%--------------------------------------------------------------------
-spec is_neuron(Term :: term()) -> boolean().
is_neuron(Neuron) -> is_record(Neuron, neuron).

%%--------------------------------------------------------------------
%% @doc Creates a cortex.
%%
%% Note that when a cortex is created, all inputs and outputs are 
%% empty. Those are completed during the connection phase and carried
%% on in cortex:new by the mutation library.
%% @end
%%--------------------------------------------------------------------
-spec cortex(CompiledLayers, Properties) -> cortex() when
    CompiledLayers :: #{integer() => layer:compiled()},
    Properties     :: cortex:properties().
cortex(CompiledLayers, Properties) ->
    Cortex = #cortex{id = ?NEW_CORTEX_ID, layers = CompiledLayers},
    edit(Cortex, Properties).

%%--------------------------------------------------------------------
%% @doc Evaluates if the input is a cortex.
%% @end
%%--------------------------------------------------------------------
-spec is_cortex(Term :: term()) -> boolean().
is_cortex(Cortex) -> is_record(Cortex, cortex).

%%--------------------------------------------------------------------
%% @doc Edits an element using some options.
%% @end
%%--------------------------------------------------------------------
-spec edit(Element, Properties) -> EditedElement when
    Element       :: neuron() | cortex(),
    Properties    :: neuron:properties() | cortex:properties(),
    EditedElement :: neuron() | cortex().
edit(Neuron, Properties) when is_record(Neuron, neuron) ->
    edit_neuron(Neuron, maps:to_list(Properties));
edit(Cortex, Properties) when is_record(Cortex, cortex) ->
    edit_cortex(Cortex, maps:to_list(Properties)).

edit_neuron(Neuron, [{id,              Value} | Options]) ->
   edit_neuron(Neuron#neuron{id = Value}, Options);
edit_neuron(Neuron, [{activation,      Value} | Options]) ->
    edit_neuron(Neuron#neuron{activation = Value}, Options);
edit_neuron(Neuron, [{aggregation,     Value} | Options]) ->
    edit_neuron(Neuron#neuron{aggregation = Value}, Options);
edit_neuron(Neuron, [{outputs_ids,     Value} | Options]) ->
    edit_neuron(Neuron#neuron{outputs_ids = Value}, Options);
edit_neuron(Neuron, [{inputs_idps,     Value} | Options]) ->
    edit_neuron(Neuron#neuron{inputs_idps = Value}, Options);
edit_neuron(Neuron, [{rcc_outputs_ids, Value} | Options]) ->
    edit_neuron(Neuron#neuron{rcc_outputs_ids = Value}, Options);
edit_neuron(Neuron, [{rcc_inputs_idps, Value} | Options]) ->
    edit_neuron(Neuron#neuron{rcc_inputs_idps = Value}, Options);
edit_neuron(Neuron, [{bias,            Value} | Options]) ->
    edit_neuron(Neuron#neuron{bias = Value}, Options);
edit_neuron(Neuron, []) -> 
    Neuron.

edit_cortex(Cortex, [{id,          Value} | Options]) -> 
    edit_cortex(Cortex#cortex{id = Value}, Options);
edit_cortex(Cortex, [{layers,      Value} | Options]) ->
    edit_cortex(Cortex#cortex{layers = Value}, Options);
edit_cortex(Cortex, [{outputs_ids, Value} | Options]) ->
    edit_cortex(Cortex#cortex{outputs_ids = Value}, Options);
edit_cortex(Cortex, [{inputs_idps, Value} | Options]) ->
    edit_cortex(Cortex#cortex{inputs_ids = Value}, Options);
edit_cortex(Cortex, []) ->
    Cortex.


%%--------------------------------------------------------------------
%% @doc Returns all the networks of the Neural Network related to the
%% cortex.
%% @end
%%--------------------------------------------------------------------
-spec neurons(Cortex :: cortex()) -> [Neuron_Id :: neuron:id()].
neurons(Cortex) ->
    lists:append(maps:values(Cortex#cortex.layers)).

-spec neurons(Cortex :: cortex(), Layer :: float() | hidden) -> 
    [Neuron_Id :: neuron:id()].
neurons(Cortex, hidden) ->
    [_ | HiddenLayers] = lists:droplast(maps:values(Cortex#cortex.layers)),
    lists:append(HiddenLayers);
neurons(Cortex, Layer) ->
    maps:get(Layer, Cortex#cortex.layers).

%%-------------------------------------------------------------------
%% @doc Returns the layers of the Neural Network related to the
%% cortex.
%% @end
%%--------------------------------------------------------------------
-spec layers(Cortex :: cortex()) -> [Coordinade :: float()].
layers(Cortex) ->
    maps:keys(Cortex#cortex.layers).

%%-------------------------------------------------------------------
%% @doc Returns the inputs links of the specified list of neurons. 
%% The other side of a link might be a neuron wich is not in the 
%% listed but listed neuron is connected to.
%% @end
%%--------------------------------------------------------------------
-spec links(Neurons :: [neuron()]) -> [Links :: link()].
links([Neuron | Neurons]) ->
    [{In, Neuron#neuron.id} || In <- inputs_ids(Neuron)] ++ links(Neurons);
links([]) ->
    [].


%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
% TODO: To make description and specs
size(Cortex) ->
    lists:sum([length(Neurons) || Neurons <- maps:values(Cortex#cortex.layers)]).

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
id(Neuron) when is_record(Neuron, neuron) ->
    Neuron#neuron.id;
id(Cortex) when is_record(Cortex, cortex) ->
    Cortex#cortex.id.

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
coordinade({{LI, _}, neuron}) -> LI;
coordinade({_, cortex})       -> cortex;
coordinade(Element)           -> coordinade(id(Element)).

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
activation(Neuron) ->
    Neuron#neuron.activation.

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
aggregation(Neuron) -> 
    Neuron#neuron.aggregation.

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
bias(Neuron) -> 
    Neuron#neuron.bias.

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
outputs_ids(Neuron) when is_record(Neuron, neuron) ->
    Neuron#neuron.outputs_ids;
outputs_ids(Cortex) when is_record(Cortex, cortex) ->
    Cortex#cortex.outputs_ids.

rcc_outputs_ids(Neuron) when is_record(Neuron, neuron) ->
    Neuron#neuron.rcc_outputs_ids;
rcc_outputs_ids(Cortex) when is_record(Cortex, cortex) ->
    [].

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
inputs_ids(Neuron) when is_record(Neuron, neuron) ->
    [Id || {Id, _W} <- inputs_idps(Neuron)];
inputs_ids(Cortex) when is_record(Cortex, cortex) ->
    Cortex#cortex.inputs_ids.

rcc_inputs_ids(Neuron) when is_record(Neuron, neuron) ->
    [Id || {Id, _W} <- rcc_inputs_idps(Neuron)].

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
inputs_idps(Neuron) when is_record(Neuron, neuron) ->
    Neuron#neuron.inputs_idps.

rcc_inputs_idps(Neuron) when is_record(Neuron, neuron) ->
    Neuron#neuron.rcc_inputs_idps.

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
add_output_id(N, ToId) when is_record(N, neuron) ->
    FromLI = coordinade(N),
    ToLI = coordinade(ToId),
    if
        FromLI < ToLI ->
            N#neuron{    outputs_ids = 
                [ToId | N#neuron.outputs_ids]};
        FromLI >= ToLI ->
            N#neuron{rcc_outputs_ids = 
                [ToId | N#neuron.rcc_outputs_ids]}

    end;
add_output_id(Cortex, ToId) when is_record(Cortex, cortex) ->
    Cortex#cortex{outputs_ids = [ToId | Cortex#cortex.outputs_ids]}.

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
remove_output_id(N, ToId) when is_record(N, neuron) ->
    N#neuron{outputs_ids     = lists:delete(ToId, N#neuron.outputs_ids),
                   rcc_outputs_ids = lists:delete(ToId, N#neuron.rcc_outputs_ids)};
remove_output_id(Cortex, ToId) when is_record(Cortex, cortex) ->
    Cortex#cortex{outputs_ids = lists:delete(ToId, Cortex#cortex.outputs_ids)}.

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
add_input_id(N, FromId) when is_record(N, neuron) ->
    FromLI = coordinade(FromId),
    ToLI = coordinade(N),
    if
        FromLI == cortex ->
            N#neuron{     inputs_idps = 
                [{FromId, cortex} | N#neuron.inputs_idps]};
        FromLI < ToLI ->
            N#neuron{     inputs_idps = 
                [{FromId, undef}  | N#neuron.inputs_idps]};
        FromLI >= ToLI ->
            N#neuron{ rcc_inputs_idps = 
                [{FromId, undef}  | N#neuron.rcc_inputs_idps]}
    end;
add_input_id(Cortex, FromId) when is_record(Cortex, cortex) ->
    Cortex#cortex{inputs_ids = [FromId | Cortex#cortex.inputs_ids]}.

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
remove_input_id(N, FromId) when is_record(N, neuron) ->
    N#neuron{inputs_idps     = lists:keydelete(FromId, 1, N#neuron.inputs_idps),
                   rcc_inputs_idps = lists:keydelete(FromId, 1, N#neuron.rcc_inputs_idps)};
remove_input_id(Cortex, FromId) when is_record(Cortex, cortex) ->
    Cortex#cortex{inputs_ids = lists:keydelete(FromId, 1, Cortex#cortex.inputs_ids)}.

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
edit_input_id(N, FromId, W) when is_record(N, neuron) ->
    N#neuron{inputs_idps     = lists:keyreplace(FromId, 1, N#neuron.inputs_idps, {FromId, W}),
                   rcc_inputs_idps = lists:keyreplace(FromId, 1, N#neuron.rcc_inputs_idps, {FromId, W})}.

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Make specs
clone_cortex(Cortex) ->
    Clone_Id = ?NEW_CORTEX_ID, % Preparation for the clonation
    ConversionETS = ets:new(idsNcloneids, [set, private]),
    ets:insert(ConversionETS, {Cortex#cortex.id, Clone_Id}),
    {Cortex#cortex{
        id          = Clone_Id,
        layers      = map_layers(Cortex#cortex.layers, ConversionETS),
        inputs_ids  = map_inputs_ids( Cortex#cortex.inputs_ids,  ConversionETS),
        outputs_ids = map_outputs_ids(Cortex#cortex.outputs_ids, ConversionETS)
    }, ConversionETS}. % FIRST map the elemenNts (over IdsNCloneIds) and build the new cortex

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Make specs
clone_neuron(Neuron, ConversionETS) ->
    Neuron#neuron{
        id              = ets:lookup_element(ConversionETS, Neuron#neuron.id, 2),
        inputs_idps     = map_inputs_idps(Neuron#neuron.inputs_idps, ConversionETS),
        outputs_ids     = map_outputs_ids(Neuron#neuron.outputs_ids, ConversionETS),
        rcc_inputs_idps = map_inputs_idps(Neuron#neuron.rcc_inputs_idps, ConversionETS),
        rcc_outputs_ids = map_outputs_ids(Neuron#neuron.rcc_outputs_ids, ConversionETS)
    }.

%%--------------------------------------------------------------------
%% @doc Returns a character list that represents the Element formatted
%% in accordance with Format.
%% @end
%%--------------------------------------------------------------------
-spec pformat(Element) -> Chars when 
      Element :: neuron() | cortex() | neuron:id() | cortex:id(),
      Chars :: io_lib:chars().
pformat(Neuron) when is_record(Neuron, neuron) -> 
    pformat(Neuron, record_info(fields, neuron));
pformat(Cortex) when is_record(Cortex, cortex) -> 
    pformat(Cortex, record_info(fields, cortex));
pformat(Id) -> 
    pformat(edb:read(Id)).

pformat(Element, Fields) ->
    [io_lib:format("Element record: ~w ~n", [element(1, Element)]) |
     pformat(Element, Fields, 2)].

pformat(Element, [Field | ListOf_Fields], Index) ->
    [io_lib:format(" --> ~w = ~w ~n", [Field, element(Index, Element)]) |
     pformat(Element, ListOf_Fields, Index + 1)];
pformat(_Element, [], _Index) ->
    [].

%%====================================================================
%% Internal functions
%%====================================================================

%.......................................................................................................................
map_layers(Layers, ConversionETS) ->
    LayersList = maps:to_list(Layers),
    NewLayerList = [{Layer, map_neurons_ids(Neurons_Ids, ConversionETS)} || {Layer, Neurons_Ids} <- LayersList],
    _NewLayer = maps:from_list(NewLayerList).

map_neurons_ids(Neurons_Ids, ConversionETS) ->
    [map_neuron_id(Neuron_Id, ConversionETS) || Neuron_Id <- Neurons_Ids].

map_neuron_id(Id, ConversionETS) ->
    CloneId = ?NEW_NEURON_ID(coordinade(Id)),
    ets:insert(ConversionETS, {Id, CloneId}),
    CloneId.

% ......................................................................................................................
map_inputs_idps(Inputs_IdPs, ConversionETS) ->
    [{ets:lookup_element(ConversionETS, I_Id, 2), W} || {I_Id, W} <- Inputs_IdPs].

% ......................................................................................................................
map_outputs_ids(Outputs_Ids, ConversionETS) ->
    [ets:lookup_element(ConversionETS, O_Id, 2) || O_Id <- Outputs_Ids].


%%====================================================================
%% Eunit white box tests
%%====================================================================

% ----------------------------------------------------------------------------------------------------------------------
% TESTS DESCRIPTIONS ---------------------------------------------------------------------------------------------------
print_elements_test_() ->
    % {setup, Where, Setup, Cleanup, Tests | Instantiator}
    [
        {"It is possible to print a neuron correctly using ?LOG_INFO",
         {setup, local, fun no_setup/0, fun no_cleanup/1, fun test_for_print_neuron/1}}
%%        {"It is possible to print all record types correctly using ?LOG_INFO",
%%         {setup, local, fun no_setup/0, fun no_cleanup/1, fun test_for_print_all/1}}
    ].


% ----------------------------------------------------------------------------------------------------------------------
% SPECIFIC SETUP FUNCTIONS ---------------------------------------------------------------------------------------------
no_setup() ->
    ok.

no_cleanup(_) ->
    ok.

% ----------------------------------------------------------------------------------------------------------------------
% ACTUAL TESTS ---------------------------------------------------------------------------------------------------------
test_for_print_neuron(_) ->
    Neuron = #neuron{bias = 0.0},
    TestListToPrint = lists:append([
                                       "Element record: neuron \n",
                                       " --> id = undefined \n",
                                       " --> af = undefined \n",
                                       " --> aggrf = undefined \n",
                                       " --> bias = 0.0 \n",
                                       " --> inputs_idps = [] \n",
                                       " --> outputs_ids = [] \n",
                                       " --> rcc_inputs_idps = [] \n",
                                       " --> rcc_outputs_ids = [] \n"
                                   ]),
    {inorder, [
        ?_assertEqual(TestListToPrint, lists:flatten(pformat(Neuron)))
    ]}.

%%test_for_print_all(_) ->
%%    EList = [#input{}, #output{}, #neuron{}, #cortex{}],
%%    lazy_for_print_all(EList).

% ----------------------------------------------------------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS --------------------------------------------------------------------------------------------

%%lazy_for_print_all([Element | T]) ->
%%    {generator, fun() -> [?_assert(print_aux(Element)) | lazy_for_print_all(T)] end};
%%lazy_for_print_all([]) ->
%%    [].
