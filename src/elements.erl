%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(elements).
-compile([export_all, nowarn_export_all]). %% TODO: To delete after build
-compile({no_auto_import, [size/1, apply/3]}).

-include_lib("math_constants.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
%%-export([]).
-export_type([type/0, cortex/0, neuron/0]).
-export_type([weight/0, link/0]).

%%% Neuronal networks are composed mostly by 2 types:
-type type()   ::      cortex |      neuron.
-type id()     :: cortex:id() | neuron:id().
-type weight() :: float() | uninitialized.
-type link()   :: {From :: id(), To :: id()}.

-define(NEW_CORTEX_ID,        {         make_ref(),  cortex}).
-define(NEW_NEURON_ID,        {{undef,  make_ref()}, neuron}).
-define(NEW_NEURON_ID(Float), {{Float,  make_ref()}, neuron}).
-define(REFERENCE(Id),              element(2,element(1,Id))).

-record(cortex, {
    id           :: cortex:id(),
    layers = #{} :: #{Coordinade :: float() => [neuron:neuron_id()]},
    outputs_ids = [] :: [neuron:id()], % Output neurons
    inputs_ids  = [] :: [neuron:id()]  % Input neurons
}).
-type cortex() :: #cortex{}.

-record(neuron, {
    id = ?NEW_NEURON_ID  :: neuron:id(),
    activation           :: activation:func(),
    aggregation          :: aggregation:func(),
    initializer          :: initializer:func(),
    outputs_ids = []     :: [ id()  ], 
    inputs_idps = []     :: [{id(), Weight :: weight()}],
    bias = uninitialized :: float() | uninitialized
}).  
-type neuron() :: #neuron{}.


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
%% - Coordinade: 0.0 (must be between [-1.0, +1.0])
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
%% on in cortex:new by the transform library.
%% @end
%%--------------------------------------------------------------------
-spec cortex(CompiledLayers, Properties) -> cortex() when
    CompiledLayers :: #{Coordinade :: float() => layer:compiled()},
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
%% @doc Edits an element using some properties.
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
edit_neuron(Neuron, [{initializer,     Value} | Options]) ->
    edit_neuron(Neuron#neuron{initializer = Value}, Options);
edit_neuron(Neuron, [{outputs_ids,     Value} | Options]) ->
    edit_neuron(Neuron#neuron{outputs_ids = Value}, Options);
edit_neuron(Neuron, [{inputs_idps,     Value} | Options]) ->
    edit_neuron(Neuron#neuron{inputs_idps = Value}, Options);
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


%%-------------------------------------------------------------------
%% @doc Returns the layers of the Neural Network related to the
%% cortex. The return is ordered from lower to higher.
%% @end
%%--------------------------------------------------------------------
-spec layers(Cortex :: cortex()) -> 
    Layers :: #{Coordinade :: float() => [neuron:neuron_id()]}.
layers(Cortex) ->
    Cortex#cortex.layers.

%%--------------------------------------------------------------------
%% @doc Returns the number of neurons inside a network under the scope
%% of the specified cortex.
%% @end
%%--------------------------------------------------------------------
-spec dimensions(CortexOrLayer) -> Dimensions when 
    CortexOrLayer :: cortex() | #{Coordinade::float()=>[neuron:id()]},
    Dimensions    :: #{Coordinade :: float() => Size :: integer()}.
dimensions(Layers) ->
    maps:map(fun(_, Nx) -> length(Nx) end, Layers).

dimensions_test() -> 
    Layers = #{
        -1.0 => lists:seq(1,10), 
         0.0 => lists:seq(1,12), 
         1.0 => lists:seq(1, 6)
    },
    ?assertEqual(#{-1.0=>10,0.0=>12,1.0=>6}, dimensions(Layers)).

%%--------------------------------------------------------------------
%% @doc Returns the number of neurons inside a network under the scope
%% of the specified cortex.
%% @end
%%--------------------------------------------------------------------
-spec size(CortexOrLayer) -> Size when 
    CortexOrLayer :: cortex() | #{Coordinade::float()=>[neuron:id()]},
    Size :: integer().
size(Cortex) when is_record(Cortex, cortex) ->
    size(layers(Cortex));
size(Layers) when is_map(Layers) ->
    lists:sum(maps:values(dimensions(Layers))).

size_test() -> 
    Layers = #{
        -1.0 => lists:seq(1,10), 
         0.0 => lists:seq(1,12), 
         1.0 => lists:seq(1, 6)
    },
    ?assertEqual(10+12+6, size(Layers)).

%%--------------------------------------------------------------------
%% @doc Returns all the networks of the Neural Network related to the
%% cortex.
%% @end
%%--------------------------------------------------------------------
-spec neurons(CortexOrLayer) -> Neurons when 
    CortexOrLayer :: cortex() | #{Coordinade::float()=>[neuron:id()]},
    Neurons       :: [Neuron_Id :: neuron:id()].
neurons(Cortex) when is_record(Cortex, cortex) ->
    neurons(layers(Cortex));
neurons(Layer) when is_map(Layer) -> 
    lists:append(maps:values(Layer)).

-spec neurons(CortexOrLayer, Control) -> Neurons when
    CortexOrLayer :: cortex() | #{Coordinade::float()=>[neuron:id()]},
    Control       :: float() | hidden,
    Neurons       :: [Neuron_Id :: neuron:id()].
neurons(Cortex, Control) when is_record(Cortex, cortex) ->
    neurons(layers(Cortex), Control);
neurons(Layers, hidden) when is_map(Layers) ->
    [_|HiddenLy] = lists:droplast(maps:values(Layers)),
    lists:append(HiddenLy);
neurons(Layers, Coordinade) ->
    maps:get(Coordinade, Layers).

%%-------------------------------------------------------------------
%% @doc Returns the inputs links of the specified list of neurons. 
%% The other side of a link might be a neuron wich is not in the 
%% listed but listed neuron is connected to.
%% @end
%%--------------------------------------------------------------------
-spec links(Neurons :: [neuron()]) -> [Links :: link()].
links([Neuron | Neurons]) ->
    [{In, Neuron#neuron.id} 
        || In <- inputs_ids(Neuron)] ++ links(Neurons);
links([]) ->
    [].

%%--------------------------------------------------------------------
%% @doc Returns the element id.
%% @end
%%--------------------------------------------------------------------
-spec id(Element :: neuron() | cortex()) -> id().
id(Neuron) when is_record(Neuron, neuron) -> Neuron#neuron.id;
id(Cortex) when is_record(Cortex, cortex) -> Cortex#cortex.id.

%%--------------------------------------------------------------------
%% @doc Retruns the coordinade form an element (or its id). The 
%% coordinade of a cortex is 'cortex'. 
%% @end
%%--------------------------------------------------------------------
-spec coordinade(Element :: id() | neuron() | cortex()) -> 
    Coordinade :: float() | cortex.
coordinade({{LI, _}, neuron}) -> LI;
coordinade({_, cortex})       -> cortex;
coordinade(Element)           -> coordinade(id(Element)).

%%-------------------------------------------------------------------
%% @doc Returns the layers coordinades of the Neural Network related 
%% to the cortex. The return is ordered from lower to higher.
%% @end
%%--------------------------------------------------------------------
-spec coordinades(Cortex :: cortex()) -> [Coordinade :: float()].
coordinades(Cortex) -> maps:keys(layers(Cortex)).

%%--------------------------------------------------------------------
%% @doc Returns the activation function of a neuron.
%% @end
%%--------------------------------------------------------------------
-spec activation(Neuron :: neuron()) -> 
    Activation :: activation:func().
activation(Neuron) ->
    Neuron#neuron.activation.

%%--------------------------------------------------------------------
%% @doc Returns the activation fuction of a neuron.
%% @end
%%--------------------------------------------------------------------
-spec aggregation(Neuron :: neuron()) -> 
    Aggregation :: aggregation:func().
aggregation(Neuron) -> 
    Neuron#neuron.aggregation.

%%--------------------------------------------------------------------
%% @doc Returns the initializer fuction of a neuron.
%% @end
%%--------------------------------------------------------------------
-spec initializer(Neuron :: neuron()) -> 
    Initializer :: initializer:func().
initializer(Neuron) -> 
    Neuron#neuron.initializer.

%%--------------------------------------------------------------------
%% @doc Returns a neuron bias.
%% @end
%%--------------------------------------------------------------------
-spec bias(Neuron :: neuron()) -> 
    Bias :: float().
bias(Neuron) -> 
    Neuron#neuron.bias.

%%--------------------------------------------------------------------
%% @doc Edits the neuron bias.
%% @end
%%--------------------------------------------------------------------
-spec edit_bias(Neuron :: neuron(), Weight :: float()) -> 
    EditedNeuron :: neuron().
edit_bias(Neuron, Weight) -> 
    Neuron#neuron{bias = Weight}.

%%--------------------------------------------------------------------
%% @doc Resets the neuron bias.
%% @end
%%--------------------------------------------------------------------
-spec reset_bias(Neuron :: neuron()) -> 
    EditedNeuron :: neuron().
reset_bias(Neuron) -> 
    Neuron#neuron{bias = uninitialized}.

%%--------------------------------------------------------------------
%% @doc Returns the output ids from an element. 
%% @end
%%--------------------------------------------------------------------
-spec outputs_ids(Element :: neuron() | cortex()) -> 
    Outputs_Ids :: [id()].
outputs_ids(Element) -> 
    outputs_ids(Element, all).

-spec outputs_ids(Element, Filter) -> Outputs_Ids when 
    Element :: neuron() | cortex(),
    Filter  :: all | dir | rcc,
    Outputs_Ids :: [id()].
outputs_ids(Neuron, all) when is_record(Neuron, neuron) ->
    Neuron#neuron.outputs_ids;
outputs_ids(Neuron, dir) when is_record(Neuron, neuron) ->
    Filter = fun(X) -> is_dir_output(coordinade(Neuron), X) end,
    lists:filter(Filter, Neuron#neuron.outputs_ids);
outputs_ids(Neuron, rcc) when is_record(Neuron, neuron) ->
    Filter = fun(X) -> not is_dir_output(coordinade(Neuron), X) end,
    lists:filter(Filter, Neuron#neuron.outputs_ids);
outputs_ids(Cortex, all) when is_record(Cortex, cortex) ->
    Cortex#cortex.outputs_ids;
outputs_ids(Cortex, dir) when is_record(Cortex, cortex) ->
    outputs_ids(Cortex, all);
outputs_ids(Cortex, rcc) when is_record(Cortex, cortex) ->
    [].

%%--------------------------------------------------------------------
%% @doc Returns true if the output comes from a higher coordinade. 
%% Note that cortex is considered always in the highest layer for 
%% outputs. 
%% @end
%%--------------------------------------------------------------------
-spec is_dir_output(Coordinade :: float(), ToId :: id()) -> 
    boolean().
is_dir_output(Coordinade, ToId) -> 
    case coordinade(ToId) of 
        cortex                 -> true;
        I when I >  Coordinade -> true;
        I when I =< Coordinade -> false
    end.

%%--------------------------------------------------------------------
%% @doc Return the input ids from an element.
%% @end
%%--------------------------------------------------------------------
-spec inputs_ids(Element :: neuron() | cortex()) -> 
    Inputs_Ids :: [id()].
inputs_ids(Element) -> 
    inputs_ids(Element, all).

-spec inputs_ids(Element, Filter) -> Inputs_Ids when 
    Element :: neuron() | cortex(),
    Filter  :: all | dir | rcc,
    Inputs_Ids :: [id()].
inputs_ids(Neuron, Filter) when is_record(Neuron, neuron) ->
    [Id || {Id, _W} <- inputs_idps(Neuron, Filter)];
inputs_ids(Cortex, all) when is_record(Cortex, cortex) ->
    Cortex#cortex.inputs_ids;
inputs_ids(Cortex, dir) when is_record(Cortex, cortex) ->
    inputs_ids(Cortex, all);
inputs_ids(Cortex, rcc) when is_record(Cortex, cortex) ->
    [].

%%--------------------------------------------------------------------
%% @doc Returns the input ids and weights from a neuron.
%% @end
%%--------------------------------------------------------------------
-spec inputs_idps(Neuron :: neuron()) -> 
    Inputs_Idps :: [id()].
inputs_idps(Neuron) -> 
    inputs_idps(Neuron, all).

-spec inputs_idps(Neuron, Filter) -> Inputs_Idps when 
    Neuron  :: neuron(),
    Filter  :: all | dir | rcc,
    Inputs_Idps :: [id()].
inputs_idps(Neuron, all) when is_record(Neuron, neuron) ->
    Neuron#neuron.inputs_idps;
inputs_idps(Neuron, dir) when is_record(Neuron, neuron) ->
    Filter = fun(X) -> is_dir_input(coordinade(Neuron), X) end,
    lists:filter(Filter, Neuron#neuron.inputs_idps);
inputs_idps(Neuron, rcc) when is_record(Neuron, neuron) ->
    Filter = fun(X) -> not is_dir_input(coordinade(Neuron), X) end,
    lists:filter(Filter, Neuron#neuron.inputs_idps).

%%--------------------------------------------------------------------
%% @doc Returns true if the input comes from a lower coordinade. Note
%% that cortex is considered always in the lowest layer for inputs. 
%% @end
%%--------------------------------------------------------------------
-spec is_dir_input(Coordinade :: float(), ToId :: id()) -> 
    boolean().
is_dir_input(Coordinade, FromId) -> 
    case coordinade(FromId) of 
        cortex                 -> true;
        I when I >= Coordinade -> false;
        I when I <  Coordinade -> true 
    end.

%%--------------------------------------------------------------------
%% @doc Adds an Id as output of the element.
%% @end
%%--------------------------------------------------------------------
-spec add_output(Neuron :: neuron(), ToId :: id()) -> 
    EditedNeuron :: neuron().
add_output(Neuron, ToId) when is_record(Neuron, neuron) ->
    Outputs_Ids = [ToId | Neuron#neuron.outputs_ids],
    Neuron#neuron{outputs_ids = Outputs_Ids};
add_output(Cortex, ToId) when is_record(Cortex, cortex) ->
    Outputs_Ids = [ToId | Cortex#cortex.outputs_ids],
    Cortex#cortex{outputs_ids = Outputs_Ids}.

%%--------------------------------------------------------------------
%% @doc Removes an Id from an element outputs.
%% @end
%%--------------------------------------------------------------------
-spec remove_output(Neuron :: neuron(), ToId :: id()) -> 
    EditedNeuron :: neuron().
remove_output(Neuron, ToId) when is_record(Neuron, neuron) ->
    Outputs_Ids = lists:delete(ToId, Neuron#neuron.outputs_ids),
    Neuron#neuron{outputs_ids = Outputs_Ids};
remove_output(Cortex, ToId) when is_record(Cortex, cortex) ->
    Outputs_Ids = lists:delete(ToId, Cortex#cortex.outputs_ids),
    Cortex#cortex{outputs_ids = Outputs_Ids}.

remove_output_test() -> 
    [remove_output_neuron_test_aux() || _ <- lists:seq(1,99)],
    [remove_output_cortex_test_aux() || _ <- lists:seq(1,99)].

remove_output_neuron_test_aux() -> 
    N1 = #neuron{id = ?NEW_NEURON_ID(rand:uniform())},
    N2 = #neuron{outputs_ids = [id(N1)]},
    ?assertMatch(#neuron{outputs_ids = []}, remove_output(N2, id(N1))).

remove_output_cortex_test_aux() -> 
    N1 = #neuron{id = ?NEW_NEURON_ID(rand:uniform())},
    C1 = #cortex{outputs_ids = [id(N1)]},
    ?assertMatch(#cortex{outputs_ids = []}, remove_output(C1, id(N1))).

%%--------------------------------------------------------------------
%% @doc Adds an Id as input of the element.
%% @end
%%--------------------------------------------------------------------
-spec add_input(Neuron :: neuron(), FromId :: id()) -> 
    EditedNeuron :: neuron().
add_input(Neuron, FromId) when is_record(Neuron, neuron) ->
   Inputs_IdPs = [{FromId,uninitialized} | Neuron#neuron.inputs_idps],
   Neuron#neuron{inputs_idps = Inputs_IdPs};
add_input(Cortex, FromId) when is_record(Cortex, cortex) ->
    Inputs_IdPs = [FromId | Cortex#cortex.inputs_ids],
    Cortex#cortex{inputs_ids = Inputs_IdPs}.

%%--------------------------------------------------------------------
%% @doc Removes an Id from an element inputs.
%% @end
%%--------------------------------------------------------------------
-spec remove_input(Neuron :: neuron(), FromId :: id()) -> 
    EditedNeuron :: neuron().
remove_input(Neuron, FromId) when is_record(Neuron, neuron) ->
    Inputs_IdPs = lists:keydelete(FromId,1,Neuron#neuron.inputs_idps),
    Neuron#neuron{inputs_idps = Inputs_IdPs};
remove_input(Cortex, FromId) when is_record(Cortex, cortex) ->
    Inputs_Ids = lists:delete(FromId, Cortex#cortex.inputs_ids),
    Cortex#cortex{inputs_ids = Inputs_Ids}.

remove_input_test() -> 
    [remove_input_neuron_test_aux() || _ <- lists:seq(1,99)],
    [remove_input_cortex_test_aux() || _ <- lists:seq(1,99)].

remove_input_neuron_test_aux() -> 
    N1 = #neuron{id = ?NEW_NEURON_ID(rand:uniform())},
    N2 = #neuron{inputs_idps = [{id(N1),0.0}]},
    ?assertMatch(#neuron{inputs_idps = []}, remove_input(N2, id(N1))).

remove_input_cortex_test_aux() -> 
    N1 = #neuron{id = ?NEW_NEURON_ID(rand:uniform())},
    C1 = #cortex{inputs_ids = [id(N1)]},
    ?assertMatch(#cortex{inputs_ids = []}, remove_input(C1, id(N1))).

%%--------------------------------------------------------------------
%% @doc Edits the input weight of an input.
%% @end
%%--------------------------------------------------------------------
-spec edit_input(Neuron :: neuron(), FromId :: id(), W :: weight()) ->
    EditedNeuron :: neuron().
edit_input(Neuron, FromId, W) when is_record(Neuron, neuron) ->
    Inputs_IdPs = lists:keyreplace(
        FromId, 1, 
        Neuron#neuron.inputs_idps, {FromId, W}),
    Neuron#neuron{inputs_idps = Inputs_IdPs}.

%%--------------------------------------------------------------------
%% @doc Resets the input weight of an input.
%% @end
%%--------------------------------------------------------------------
-spec reset_input(Neuron :: neuron(), FromId :: id()) -> 
    EditedNeuron :: neuron().
reset_input(Neuron, FromId) when is_record(Neuron, neuron) -> 
    Inputs_IdPs = lists:keyreplace(
        FromId, 1, 
        Neuron#neuron.inputs_idps, {FromId,uninitialized}),
    Neuron#neuron{inputs_idps = Inputs_IdPs}.

%%--------------------------------------------------------------------
%% @doc Clones a cortex and generates a map inside using an ETS table 
%% for the a new id for each neuron and the original neuron id. Note
%% it does not clone any neuron nor modifies the original cortex and
%% neurons.
%% @end
%--------------------------------------------------------------------
-spec clone_cortex(Cortex :: cortex()) -> 
    {Clone :: cortex(), ConversionETS :: ets:tid()}.
clone_cortex(Cortex) ->
    Clone_Id = ?NEW_CORTEX_ID, % Preparation for the clonation
    ConversionETS = ets:new(idsNcloneids, [set, private]),
    ets:insert(ConversionETS, {Cortex#cortex.id, Clone_Id}),
    {Cortex#cortex{
        id          = Clone_Id,
        layers      = map_layers(Cortex#cortex.layers, ConversionETS),
        inputs_ids  = map_inputs_ids(Cortex#cortex.inputs_ids,   ConversionETS),
        outputs_ids = map_outputs_ids(Cortex#cortex.outputs_ids, ConversionETS)
    }, ConversionETS}. % FIRST map the elemenNts (over IdsNCloneIds) and build the new cortex

%%--------------------------------------------------------------------
%% @doc Clones a neuron.
%% @end
%%--------------------------------------------------------------------
-spec clone_neuron(Neuron :: neuron(), ConversionETS :: ets:tid()) -> 
    Clone :: neuron().
clone_neuron(Neuron, ConversionETS) ->
    Neuron#neuron{
        id              = ets:lookup_element(ConversionETS, Neuron#neuron.id, 2),
        inputs_idps     = map_inputs_idps(Neuron#neuron.inputs_idps, ConversionETS),
        outputs_ids     = map_outputs_ids(Neuron#neuron.outputs_ids, ConversionETS)
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
map_outputs_ids(Outputs_Ids, ConversionETS) ->
    [ets:lookup_element(ConversionETS, O_Id, 2) || O_Id <- Outputs_Ids].

% ......................................................................................................................
map_inputs_ids(Inputs_Ids, ConversionETS) ->
    [ets:lookup_element(ConversionETS, I_Id, 2) || I_Id <- Inputs_Ids].

% ......................................................................................................................
map_inputs_idps(Inputs_IdPs, ConversionETS) ->
    [{ets:lookup_element(ConversionETS, I_Id, 2), W} || {I_Id, W} <- Inputs_IdPs].


%%====================================================================
%% Eunit white box tests
%%====================================================================

% ----------------------------------------------------------------------------------------------------------------------
% TESTS DESCRIPTIONS ---------------------------------------------------------------------------------------------------
% print_elements_test_() ->
%     {setup, Where, Setup, Cleanup, Tests | Instantiator}
%     [
%         {"It is possible to print a neuron correctly using ?LOG_INFO",
%          {setup, local, fun no_setup/0, fun no_cleanup/1, fun test_for_print_neuron/1}}
%        {"It is possible to print all record types correctly using ?LOG_INFO",
%         {setup, local, fun no_setup/0, fun no_cleanup/1, fun test_for_print_all/1}}
%     ].


% ----------------------------------------------------------------------------------------------------------------------
% SPECIFIC SETUP FUNCTIONS ---------------------------------------------------------------------------------------------
no_setup() ->
    ok.

no_cleanup(_) ->
    ok.

% ----------------------------------------------------------------------------------------------------------------------
% ACTUAL TESTS ---------------------------------------------------------------------------------------------------------
% test_for_print_neuron(_) ->
%     Neuron = #neuron{bias = 0.0},
%     TestListToPrint = lists:append(
%         [
%             "Element record: neuron \n",
%             " --> id = undefined \n",
%             " --> activation  = undefined \n",
%             " --> aggregation = undefined \n",
%             " --> initializer = undefined \n",
%             " --> outputs_ids = [] \n",
%             " --> inputs_idps = [] \n",
%             " --> bias = 0.0 \n"
%         ]),
%     {inorder, [
%         ?_assertEqual(TestListToPrint, lists:flatten(pformat(Neuron)))
%     ]}.

%%test_for_print_all(_) ->
%%    EList = [#input{}, #output{}, #neuron{}, #cortex{}],
%%    lazy_for_print_all(EList).

% ----------------------------------------------------------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS --------------------------------------------------------------------------------------------

%%lazy_for_print_all([Element | T]) ->
%%    {generator, fun() -> [?_assert(print_aux(Element)) | lazy_for_print_all(T)] end};
%%lazy_for_print_all([]) ->
%%    [].
