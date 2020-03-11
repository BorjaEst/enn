%%%-------------------------------------------------------------------
%%% @author Borja
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. oct 2018 11:40
%%%-------------------------------------------------------------------
-module(debugger_calls).
-author("Borja").

%% API
-export([enn_SUITE/0, mutations_SUITE/0]).

-define(LOG_DIR, "./apps/enn/_build/test/logs").
-define(STEP_OPTS, []).
-define(DEFAULT_OPTIONS, [{logdir, ?LOG_DIR}, {step, ?STEP_OPTS}]).

%% TESTS CALLS
enn_SUITE() ->
	ct:run_test([{suite, enn_SUITE} | ?DEFAULT_OPTIONS]).

mutations_SUITE() ->
	ct:run_test([{suite, mutations_SUITE} | ?DEFAULT_OPTIONS]).
