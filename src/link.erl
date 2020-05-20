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
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec read({From, To}) -> weight() when 
    From :: neuron:id(),
    To   :: neuron:id().
read({From, To}) -> 
    case mnesia:read(link, id(From,To)) of 
        [L] -> weight(L);
        []  -> undefined
    end.

%%-------------------------------------------------------------------
%% @doc Writes the indicated links in mnesia.
%% Should run inside a mnesia transaction.
%% @end
%%------------------------------------------------------------------
-spec write(Link :: {From, To}, Weight :: weight()) -> ok when
    From :: neuron:id(),
    To   :: neuron:id().
write({From, To}, W) -> 
    ok = mnesia:write(#link{id=id(From,To), w=W}).

%%-------------------------------------------------------------------
%% @doc Adds a specific value to the previous weight.
%% Should run inside a mnesia transaction.
%% @end
%%------------------------------------------------------------------
-spec add(Link :: {From, To}, Value :: float()) -> ok when
    From :: neuron:id(),
    To   :: neuron:id().
add({From, To}, V) -> 
    case read({From, To}) of 
        W when is_float(W) -> write({From, To}, W + V);
        undefined          -> write({From, To}, V)
    end.

%%-------------------------------------------------------------------
%% @doc Deletes the links (the status will be undefined).
%% Should run inside a mnesia transaction. 
%% @end
%%------------------------------------------------------------------
-spec delete(Link :: {From, To}) -> ok when
    From :: neuron:id(),
    To   :: neuron:id().
delete({From, To}) ->
    ok = mnesia:delete(link, id(From,To), write).

%%-------------------------------------------------------------------
%% @doc Clones a link replacing the From and To ids using a map.
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec clone(Link :: {From, To}, #{Old => New}) -> ok when 
    From :: neuron:id(),
    To   :: neuron:id(),
    Old  :: neuron:id(),
    New  :: neuron:id().
clone({From, To}, Map) -> 
    case read({From, To}) of 
        W when is_float(W) -> write(map({From, To}, Map), W);
        undefined          -> ok
    end.

%%-------------------------------------------------------------------
%% @doc Moves the links using a map.
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec move(Links :: {From, To}, #{Old => New}) -> ok when 
    From :: neuron:id(),
    To   :: neuron:id(),
    Old  :: neuron:id(),
    New  :: neuron:id().
move({From, To}, Map) -> 
    clone({From, To}, Map),
    delete({From, To}).

%%-------------------------------------------------------------------
%% @doc Merges the links using a map.
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec merge(Links :: {From, To}, #{Old => New}) -> ok when 
    From :: neuron:id(),
    To   :: neuron:id(),
    Old  :: neuron:id(),
    New  :: neuron:id().
merge({From, To}, Map) -> 
    case read({From, To}) of 
        W when is_float(W) -> 
            add(map({From, To}, Map), W),
            delete({From, To});
        undefined          -> ok
    end.

%%-------------------------------------------------------------------
%% @doc Merges the links using a map.
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec divide(Links :: {From, To}, #{Old => New}) -> ok when 
    From :: neuron:id(),
    To   :: neuron:id(),
    Old  :: neuron:id(),
    New  :: neuron:id().
divide({From, To}, Map) -> 
    case read({From, To}) of 
        W when is_float(W) -> 
            add(map({From, To}, Map), W/2.0),
            write({From, To}, W)/2.0;
        undefined          -> ok
    end.


%%====================================================================
%% Internal functions
%%====================================================================

% Returns the link id from it From and To ---------------------------
id(From, To) -> {{From,To}, link}.

% Returns the link weight or undefined in any other case ------------
weight(#link{w=W}) -> W;
weight(         _) -> undefined.

% Maps a link from a tuple {From,To} --------------------------------
map({From,To}, Map) -> {maps:get(From,Map,From),maps:get(To,Map,To)}.

