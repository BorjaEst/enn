%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc 
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(link).
-compile([export_all, nowarn_export_all]). %% TODO: To delete after buil

-export([]).
-export_type([link/0, weight/0]).

-type link()   :: {From :: neuron:id(), To :: neuron:id()}.
-type id()     :: {link(), link}.
-type weight() :: float() | undefined.
-record(link, {
    id :: id(),
    w  :: weight()
}).


%%%===================================================================
%%% API
%%%===================================================================

%%-------------------------------------------------------------------
%% @doc Record fields from link.  
%% @end
%%-------------------------------------------------------------------
-spec record_fields() -> ListOfFields :: [atom()].
record_fields() -> record_info(fields, link).

%%-------------------------------------------------------------------
%% @doc Returns the links weights.
%% @end
%%-------------------------------------------------------------------
-spec read([{From, To}]) -> [weight()] when 
    From :: neuron:id(),
    To   :: neuron:id().
read(Links) -> 
    [weight(L) || L <- edb:read([id(From,To) || {From,To} <- Links])].

%%-------------------------------------------------------------------
%% @doc Writes the indicated links in mnesia.
%% @end
%%------------------------------------------------------------------
-spec write(Links :: [{From, To}], Weights :: [weight()]) -> ok when
    From :: neuron:id(),
    To   :: neuron:id().
write(Links, W) -> 
    edb:write(links(Links,W)).

%%-------------------------------------------------------------------
%% @doc Deletes the links (the status will be undefined).
%% @end
%%------------------------------------------------------------------
-spec delete(Links :: [{From, To}]) -> ok when
    From :: neuron:id(),
    To   :: neuron:id().
delete(Links) ->
    edb:delete([id(From,To) || {From,To} <- Links]).

%%-------------------------------------------------------------------
%% @doc Clones a link replacing the From and To ids using a map.
%% @end
%%-------------------------------------------------------------------
-spec clone(Links :: [{From, To}], #{Old => New}) -> ok when 
    From :: neuron:id(),
    To   :: neuron:id(),
    Old  :: neuron:id(),
    New  :: neuron:id().
clone(Links, Map) -> 
    write(map(Links, Map), read(Links)).

%%-------------------------------------------------------------------
%% @doc Moves the links using a map.
%% @end
%%-------------------------------------------------------------------
-spec move(Links :: [{From, To}], #{Old => New}) -> ok when 
    From :: neuron:id(),
    To   :: neuron:id(),
    Old  :: neuron:id(),
    New  :: neuron:id().
move(Links, Map) -> 
    W   = read(Links),
    New = map(Links, Map),
    delete(Links),
    write(New, W).

%%-------------------------------------------------------------------
%% @doc Merges the links using a map.
%% @end
%%-------------------------------------------------------------------
-spec merge(Links :: [{From, To}], #{Old => New}) -> ok when 
    From :: neuron:id(),
    To   :: neuron:id(),
    Old  :: neuron:id(),
    New  :: neuron:id().
merge(Links, Map) -> 
    Target = map(Links, Map),
    W1     = read(Links),
    W2     = read(Target),
    delete(Links),
    write(Target, sum(W1,W2)).


%%====================================================================
%% Internal functions
%%====================================================================

% Merges a list of {From,To} with a list of weights -----------------
links([{From,To}|Lx], [W|Wx]) -> [link({From,To},W) | links(Lx,Wx)];
links(            [],     []) -> [].

% Creates a link from a tuple {From,To} and a weight ----------------
link({From,To},W) -> #link{id=id(From,To), w=W}.

% Returns the link id from it From and To ---------------------------
id(From, To) -> {{From,To}, link}.

% Returns the link weight or undefined in any other case ------------
weight(#link{w=W}) -> W;
weight(         _) -> undefined.

% Maps a link from a tuple {From,To} --------------------------------
map({From,To}, Map) -> {maps:get(From,Map,From),maps:get(To,Map,To)};
map(Links, Map) when is_list(Links) -> [map(L,Map) || L <- Links].

% Sums 2 lists of weights -------------------------------------------
sum([undefined|Wx1], [       W2|Wx2]) -> [   W2|sum(Wx1, Wx2)];
sum([       W1|Wx1], [undefined|Wx2]) -> [   W1|sum(Wx1, Wx2)];
sum([       W1|Wx1], [       W2|Wx2]) -> [W1+W2|sum(Wx1, Wx2)];
sum(             [],              []) -> [].

