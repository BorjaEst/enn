%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc 
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(link).
-compile([export_all, nowarn_export_all]). %% TODO: To delete after buil

-export([]).
-export_type([id/0, weight/0, link/0]).

-type id()     :: {{From :: neuron:id(), To :: neuron:id()}, link}.
-type weight() :: float() | uninitialized.
-record(link, {
    id :: id(),
    w  :: weight()
}).
-type link() :: #link{}.
-type properties() :: #{weight := weight()}.


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new link. Some properties can be defined.
%% @end
%%--------------------------------------------------------------------
-spec new(N1, N2) -> link() when
    N1 :: neuron:id(),
    N2 :: neuron:id().
new(N1, N2) -> new(N1, N2, #{}).

-spec new(N1, N2, Properties) -> link() when
    N1 :: neuron:id(),
    N2 :: neuron:id(),
    Properties :: properties().
new(N1, N2, Properties) ->  
    #link{
        id = id(N1, N2),
        w  = maps:get(weight, Properties, uninitialized)
    }.

%%--------------------------------------------------------------------
%% @doc Returns the link id.
%% @end
%%-------------------------------------------------------------------
-spec id(Link :: link()) -> id().
id(Link) -> Link#link.id.

-spec id(N1, N2) -> id() when
    N1 :: neuron:id(),
    N2 :: neuron:id().
id(N1, N2) -> {{N1, N2}, link}.

%%-------------------------------------------------------------------
%% @doc Record fields from link.  
%% @end
%%-------------------------------------------------------------------
-spec record_fields() -> ListOfFields :: [atom()].
record_fields() -> record_info(fields, link).

%%--------------------------------------------------------------------
%% @doc Returns the link issuer.
%% @end
%%-------------------------------------------------------------------
-spec from(Link :: link()) -> neuron:id().
from({{From, _}, link}) -> From.

%%--------------------------------------------------------------------
%% @doc Returns the link receiver.
%% @end
%%-------------------------------------------------------------------
-spec to(Link :: link()) -> neuron:id().
to({{_, To}, link}) -> To.

%%--------------------------------------------------------------------
%% @doc Returns the link weight.
%% @end
%%-------------------------------------------------------------------
-spec weight(Link :: link()) -> weight().
weight(Link) -> Link#link.w.

%%--------------------------------------------------------------------
%% @doc Changes the link weight.
%% @end
%%-------------------------------------------------------------------
-spec edit(Link :: link(), W :: weight()) -> Edited :: link().
edit(Link, W) -> Link#link{w = W}.

