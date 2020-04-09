%%%-------------------------------------------------------------------
%%% @author Borja
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. oct 2018 11:40
%%%-------------------------------------------------------------------
-module(debug).
-author("Borja").

%% API
-export([enn_SUITE/0, transforms_SUITE/0]).

-define(LOG_DIR, "./apps/enn/_build/test/logs").
-define(STEP_OPTS, []).
-define(DEFAULT_OPTIONS, [{logdir, ?LOG_DIR}, {step, ?STEP_OPTS}]).

%% TESTS CALLS
enn_SUITE() ->
    ct:run_test([{suite, enn_SUITE} | ?DEFAULT_OPTIONS]).

transforms_SUITE() ->
    ct:run_test([{suite, transforms_SUITE} | ?DEFAULT_OPTIONS]).
