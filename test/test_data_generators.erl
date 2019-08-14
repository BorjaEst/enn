%%%-------------------------------------------------------------------
%%% @author borja
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Feb 2019 11:56
%%%-------------------------------------------------------------------
-module(test_data_generators).
-compile([export_all, nowarn_export_all]). %%TODO: To delete after build

-include_lib("layers.hrl").
-include_lib("nnelements.hrl").

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

% ......................................................................................................................
% TODO: Define specs and comments
static_sum_of_inputs(N_Inputs, N_Outputs, N_Loops) ->
	Static_Inputs = [rand:uniform() - 0.5 || _ <- lists:seq(1, N_Inputs)],
	Inputs = [Static_Inputs || _ <- lists:seq(1, N_Loops)],
	Outputs = [lists:duplicate(N_Outputs, lists:sum(InList)) || InList <- Inputs],
	{Inputs, Outputs}.

% ......................................................................................................................
% TODO: Define specs and comments
random_sum_of_inputs(N_Inputs, N_Outputs, N_Loops) ->
	Inputs = [[rand:uniform() - 0.5 || _ <- lists:seq(1, N_Inputs)] || _ <- lists:seq(1, N_Loops)],
	Outputs = [lists:duplicate(N_Outputs, lists:sum(InList)) || InList <- Inputs],
	{Inputs, Outputs}.

% ......................................................................................................................
% TODO: Define specs and comments
static_xor_of_inputs(2, 1, N_Loops) ->
	In = [[-1, -1], [-1, 1], [1, -1], [1, 1]],
	Inputs = lists:append(lists:duplicate(N_Loops div 4, In)) ++ element(1, lists:split(N_Loops rem 4, In)),
	Outputs = [[do_xor(In1, In2)] || [In1, In2] <- Inputs],
	{Inputs, Outputs}.

% ......................................................................................................................
% TODO: Define specs and comments
random_xor_of_inputs(2, 1, N_Loops) ->
	Inputs = [[rand:uniform(2) * 2 - 3, rand:uniform(2) * 2 - 3] || _ <- lists:seq(1, N_Loops)],
	Outputs = [[do_xor(In1, In2)] || [In1, In2] <- Inputs],
	{Inputs, Outputs}.

% ......................................................................................................................
% TODO: Define specs and comments
inputs_always_0(N_Inputs, N_Outputs, N_Loops) ->
	Inputs = [[0.0 || _ <- lists:seq(1, N_Inputs)] || _ <- lists:seq(1, N_Loops)],
	Outputs = [[(rand:uniform() - 0.5) * 2 || _ <- lists:seq(1, N_Outputs)] || _ <- Inputs],
	{Inputs, Outputs}.

% ......................................................................................................................
% TODO: Define specs and comments
sequence_of_1_input(1, 1, N_Loops) ->
	Inputs = [[rand:uniform(10) / 10.0] || _ <- lists:seq(1, N_Loops)],
	Outputs = [[0.0] | lists:droplast(Inputs)],
	{Inputs, Outputs}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

% ......................................................................................................................
do_xor(In1, In2) when (In1 == 1) xor (In2 == 1) -> 1.0;
do_xor(_, _)                                    -> -1.0.


