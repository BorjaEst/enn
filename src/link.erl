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


-type id()     :: {From :: neuron:id(), To :: neuron:id(), link}.
-type weight() :: float() | uninitialized.
-record(link, {
    id :: id(),
    w = uninitialized :: weight()
}).
-type link() :: #link{}.




