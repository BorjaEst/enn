%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(transform).
-compile([export_all, nowarn_export_all]). %%TODO: To delete after build

%% API
%%-export([]).

%%%===================================================================
%%% API - LEVEL 1 transforms
%%%===================================================================
%% Features:
%%      - Frequency: High
%%      - Effects: Only a link is modified

%%-------------------------------------------------------------------
%% @doc Edits the weight of a neuron link.
%% @end
%%-------------------------------------------------------------------
-spec edit_link(From, To, Weight) -> no_return() when 
    From   :: neuron:id() | cortex:id(),
    To     :: neuron:id(),
    Weight :: float().
edit_link(FromId, ToId, Weight) ->
    To     = edb:read(ToId),
    Edited = elements:edit_input(To, FromId, Weight),
    edb:write(Edited).

%%-------------------------------------------------------------------
%% @doc Resets the indicated links of a neuron.
%% @end
%%-------------------------------------------------------------------
-spec reset_links(From, To) -> no_return() when 
    From  :: [neuron:id() | cortex:id()],
    To    :: neuron:id().
reset_links(ToId, FromIds) ->
    Edited = lists:foldl(
        fun(FromId,To) -> elements:reset_input(To, FromId) end,
        edb:read(ToId), FromIds
    ),
    edb:write(Edited).

%%--------------------------------------------------------------------
%% @doc Edits the weight of a neuron bias.
%% @end
%%--------------------------------------------------------------------
-spec edit_bias(Neuron_Id, Weight) -> no_return() when 
    Neuron_Id :: neuron:id(),
    Weight    :: float().
edit_bias(Neuron_Id, Weight) ->
    Neuron = edb:read(Neuron_Id),
    Edited = elements:edit_bias(Neuron, Weight),
    edb:write(Edited).

%%--------------------------------------------------------------------
%% @doc Edits the weight of a neuron bias.
%% @end
%%--------------------------------------------------------------------
-spec reset_bias(Neuron_Id) -> no_return() when 
    Neuron_Id :: neuron:id().
reset_bias(Neuron_Id) ->
    Neuron = edb:read(Neuron_Id),
    Edited = elements:reset_bias(Neuron),
    edb:write(Edited).


%%%===================================================================
%%% API - LEVEL 2 transforms
%%%===================================================================
%% Features:
%%      - Frequency: medium
%%      - Effects: Creates, removes connections and modifies neuron functions

%%--------------------------------------------------------------------
%% @doc
%% % TODO: To make description
%%
%% @end
%%--------------------------------------------------------------------
% TODO: Define specs
create_link(SameElement_Id, SameElement_Id) ->
    SameElement = edb:read(SameElement_Id),
    U1_SameElement = link_only_From(SameElement, SameElement),
    U2_SameElement = link_only_To(U1_SameElement, U1_SameElement),
    edb:write(U2_SameElement);
create_link(FromElement_Id, ToElement_Id) ->
    [FromElement, ToElement] = edb:read([FromElement_Id, ToElement_Id]),
    U_FromElement = link_only_From(FromElement, ToElement),
    U_ToElement   = link_only_To(U_FromElement, ToElement),
    edb:write([U_FromElement, U_ToElement]).

%%--------------------------------------------------------------------
%% @doc
%% % TODO: To make description
%%
%% @end
%%--------------------------------------------------------------------
% TODO: Define specs
remove_link(SameElement_Id, SameElement_Id) ->
    SameElement = edb:read(SameElement_Id),
    U1_SameElement = unlink_only_From(SameElement, SameElement),
    U2_SameElement = unlink_only_To(U1_SameElement, U1_SameElement),
    edb:write(U2_SameElement);
remove_link(FromElement_Id, ToElement_Id) ->
    [FromElement, ToElement] = edb:read([FromElement_Id, ToElement_Id]),
    U_FromElement = unlink_only_From(FromElement, ToElement),
    U_ToElement   = unlink_only_To(U_FromElement, ToElement),
    edb:write([U_FromElement, U_ToElement]).

%%--------------------------------------------------------------------
%% @doc
%% % TODO: To make description
%%
%% @end
%%--------------------------------------------------------------------
% TODO: Define specs
change_activation({_, neuron} = Neuron_Id, NewAFun) ->
    Neuron    = edb:read(Neuron_Id),
    NewNeuron = elements:edit(Neuron, #{activation => NewAFun}),
    edb:write(NewNeuron).

%%--------------------------------------------------------------------
%% @doc
%% % TODO: To make description
%%
%% @end
%%--------------------------------------------------------------------
% TODO: Define specs
change_aggregation({_, neuron} = Neuron_Id, NewAggrFun) ->
    Neuron = edb:read(Neuron_Id),
    NewNeuron = elements:edit(Neuron, #{aggregation => NewAggrFun}),
    edb:write(NewNeuron).


%%%===================================================================
%%% API - LEVEL 3 transforms
%%%===================================================================
%% Features:
%%      - Frequency: Low
%%      - Effects: Changes the architecture

%%--------------------------------------------------------------------
%% @doc
%% % TODO: To make description
%%
%% @end
%%--------------------------------------------------------------------
% TODO: Define specs
insert_neuron(Layer, AFun, AggrFun, Cortex_Id) ->
    Cortex      = edb:read(Cortex_Id),
    Neurons_Ids = maps:get(Layer, elements:layers(Cortex), []),
    Neuron_Id   = neuron:new(Layer, AFun, AggrFun, _Options = []),
    NewLayer    = maps:put(Layer, [Neuron_Id | Neurons_Ids], elements:layers(Cortex)),
    NewCortex   = elements:edit(Cortex, #{layers => NewLayer}),
    edb:write(NewCortex).

%%--------------------------------------------------------------------
%% @doc
%% % TODO: To make description
%%
%% @end
%%--------------------------------------------------------------------
% TODO: Define specs
remove_neuron({{Layer, _}, neuron} = Neuron_Id, Cortex_Id) ->
    Cortex      = edb:read(Cortex_Id),
    Neurons_Ids = maps:get(Layer, elements:layers(Cortex)),
    Neuron      = edb:read(Neuron_Id),
    [remove_link(E_Id, Neuron_Id) || E_Id <- elements:inputs_ids(Neuron)],
    [remove_link(Neuron_Id, E_Id) || E_Id <- elements:outputs_ids(Neuron)],
    NewCortex = case lists:delete(Neuron_Id, Neurons_Ids) of
        [] ->
            NewLayer = maps:remove(Layer, elements:layers(Cortex)),
            elements:edit(Cortex, #{layer => NewLayer});
        Neurons ->
            NewLayer = maps:update(Layer, Neurons, elements:layers(Cortex)),
            elements:edit(Cortex, #{layer => NewLayer})
    end,
    edb:write(NewCortex).


%%====================================================================
%% Internal functions
%%====================================================================

%.......................................................................................................................
link_only_From(From, To) ->
    ToId = elements:id(To),
    case lists:member(ToId, elements:outputs_ids(From)) of
        false ->
            _U_From = elements:add_output(From, ToId);
        true ->
            FromId     = elements:id(From),
            Error_Text = io_lib:format("[cannot add O_Id]: ~w already a member of ~w outputs", [ToId, FromId]),
            error({link_fail, lists:flatten(Error_Text)})
    end.

%.......................................................................................................................
link_only_To(From, To) ->
    FromId = elements:id(From),
    case lists:member(FromId, elements:inputs_ids(To)) of
        false ->
            _U_To = elements:add_input(To, FromId);
        true ->
            ToId       = elements:id(To),
            Error_Text = io_lib:format("[cannot add I_Id]: ~w already a member of ~w inputs", [FromId, ToId]),
            error({link_fail, lists:flatten(Error_Text)})
    end.

%.......................................................................................................................
unlink_only_From(From, To) ->
    ToId = elements:id(To),
    case lists:member(ToId, elements:outputs_ids(From)) of
        true ->
            _U_From = elements:remove_output(From, ToId);
        false ->
            FromId     = elements:id(From),
            Error_Text = io_lib:format("[cannot remove O_Id]: ~w not a member of ~w outputs", [ToId, FromId]),
            error({unlink_fail, lists:flatten(Error_Text)})
    end.

%.......................................................................................................................
unlink_only_To(From, To) ->
    FromId = elements:id(From),
    case lists:member(FromId, elements:inputs_ids(To)) of
        true ->
            _U_To = elements:remove_input(To, FromId);
        false ->
            ToId       = elements:id(To),
            Error_Text = io_lib:format("[cannot remove I_Id]: ~w not a member of ~w intputs", [FromId, ToId]),
            error({unlink_fail, lists:flatten(Error_Text)})
    end.

%.......................................................................................................................
next_of(_Element, [], If_Last)                      -> If_Last;
next_of(Element, [Next | _], _) when Element < Next -> Next;
next_of(Element, [_ | Rest], If_Last)               -> next_of(Element, Rest, If_Last).

%.......................................................................................................................
prev_of(Element, List, If_First) -> prev_of2(Element, lists:reverse(List), If_First).

prev_of2(_Element, [], If_Last)                      -> If_Last;
prev_of2(Element, [Next | _], _) when Element > Next -> Next;
prev_of2(Element, [_ | Rest], If_Last)               -> prev_of2(Element, Rest, If_Last).

%%====================================================================
%% Eunit white box tests
%%====================================================================

% ----------------------------------------------------------------------------------------------------------------------
% TESTS DESCRIPTIONS ---------------------------------------------------------------------------------------------------

% ----------------------------------------------------------------------------------------------------------------------
% SPECIFIC SETUP FUNCTIONS ---------------------------------------------------------------------------------------------

% ----------------------------------------------------------------------------------------------------------------------
% ACTUAL TESTS ---------------------------------------------------------------------------------------------------------

% ----------------------------------------------------------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS --------------------------------------------------------------------------------------------



