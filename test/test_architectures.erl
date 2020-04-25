%%%-------------------------------------------------------------------
%%% @author borja
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Feb 2019 11:56
%%%-------------------------------------------------------------------
-module(test_architectures).
-compile([export_all, nowarn_export_all]). %%TODO: To delete after build

-include_lib("layers.hrl").

%% Defined agent species
-export([]).

-ifdef(debug_mode).
-define(LOG(X), io:format("{~p,~p,~p}: ~p~n", [self(), ?MODULE, ?LINE, X])).
-define(STDCALL_TIMEOUT, infinity).
-else.
-define(LOG(X), true).
-define(STDCALL_TIMEOUT, 5000).
-endif.

%%%===================================================================
%%% Defined Neural Architectures
%%%===================================================================

% -------------------------------------------------------------------
% TODO: Define specs and comments
random_dense(Max_Units, Max_Layers) ->
    _Model = model:sequential(
        [?input(rand:uniform(Max_Units))] ++
        [?dense(rand:uniform(Max_Units)) || _ <- lists:seq(1, rand:uniform(Max_Layers - 1))] ++
        [?output(rand:uniform(Max_Units))]
    ).

% -------------------------------------------------------------------
% TODO: Define specs and comments
example() ->
    _Model = model:sequential([
                                ?input(10),
                                ?dense(10, #{activation => sigmoid}),
                                ?output(1)
                            ]).

% -------------------------------------------------------------------
% TODO: Define specs and comments
xor_gate() ->
    _Model = model:sequential([
                                ?input( 2),
                                ?dense( 2, #{activation => elu}),
                                ?output(1, #{activation => elu})
                            ]).

% -------------------------------------------------------------------
% TODO: Define specs and comments
addition() ->
    _Model = model:sequential([
                                ?input(2),
                                ?output(1, #{activation => direct})
                            ]).

% -------------------------------------------------------------------
% TODO: Define specs and comments
multiplication() ->
    _Model = model:sequential([
                                ?input(2),
                                ?output(1, #{
                                    aggregation => product,
                                    activation  => direct})
                            ]).

% -------------------------------------------------------------------
% TODO: Define specs and comments
network_0_weights() ->
    _Model = model:sequential([
                                ?input(1),
                                ?dense(2, #{activation => tanh}),
                                ?output(1)
                            ]).

% -------------------------------------------------------------------
% TODO: Define specs and comments
recurrent() ->
    _Model = model:recurrent([
                               ?input(1, #{activation => sigmoid}),
                               ?dense(2, #{activation => sigmoid}),
                               ?output(1, #{activation => tanh})
                           ], 2).

% -------------------------------------------------------------------
% TODO: Define specs and comments
classification() ->
    _Model = ok.


