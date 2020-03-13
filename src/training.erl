%%%-------------------------------------------------------------------
%%% @author borja
%%% @copyright (C) 2018, <COMPANY>
%%% @doc  
%%%
%%% @end
%%% Created : 30. Aug 2018 0:05
%%%-------------------------------------------------------------------
-module(training).
-compile([export_all, nowarn_export_all]). %% TODO: To delete after build

-include_lib("math_constants.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
%%-export([start_link/0]).

%% gen_statem callbacks
-export([init/1, predict/3, fit/3, results/3, terminate/3]).

-record(state, {
	inputsList,
    optimaList
}).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Supervised ANN training function. Fits the Predictions to the 
%% Optima.
%% @end
%%--------------------------------------------------------------------
-spec start_link(Cortex_PId :: pid(), Inputs :: [float()], 
          Optima :: [float()]) ->
	{Loss :: [float()], Predictions :: [float()]}.

start_link(Cortex_PId, Inputs, Optima) ->
	start_link(Cortex_PId, Inputs, Optima, []). 

start_link(Cortex_PId, Inputs, Optima, Options) ->
	spawn_link()
			[
				self(),
			Cortex_PId, 
			Inputs, 
			Optima
		], []).





			


%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
init(Caller, Cortex_PId, InputsList, OptimaList, [Option | Rest]) ->
	case Option of
		loss -> 
			put(calculate_loss, true);
		{log, LogName} ->
			{ok, LogRef} = datalog:new(LogName),
			put(log_ref, LogRef)
	end,
	init(Cortex_PId, InputsList, OptimaList, Rest);
init(Cortex_PId, InputsList, OptimaList, []) -> 
	put(cortex_pid, Cortex_PId),
	predict(enter, init, #state{
		inputsList = InputsList,
    	optimaList = OptimaList
	}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%%--------------------------------------------------------------------
%% predict: Performs a prediction from the input	
%%--------------------------------------------------------------------
predict(enter, _OldState, #state{inputsList = []} = State) ->
	terminate(State);
predict(enter, _OldState, State) -> 
	reset_data(),
	predict(internal, State#state.inputsList, State);
predict(internal, [Inputs | InputsList], State) -> 
	put_inputs(Inputs),
	put_prediction(cortex:predict(get(cortex_pid), Inputs)),
	fit(enter, predict, State#state{inputsList = InputsList}).

%%--------------------------------------------------------------------
%% fit: Trains the model if optima available
%%--------------------------------------------------------------------
fit(enter, _OldState, #state{optimaList = []} = State) ->
	results(enter, fit, State);
fit(enter, _OldState, State) -> 
	fit(internal, State#state.optimaList, State);
fit(internal, [Optima | OptimaList], State) -> 
	put_optima(Optima),
	put_errors(cortex:fit(get(cortex_pid), Optima)),
	results(enter, fit, State#state{optimaList = OptimaList}).

%%--------------------------------------------------------------------
%% results: Applies the result functions (Loss calculation, log, etc.)
%%--------------------------------------------------------------------
results(enter, _OldState, State) ->
	lists:foldl(
		fun(F, X) -> F(X) end, 
		get(data), 
		[
			fun loss/1,
			fun datalog/1
		]),
	predict(enter, results, State). 
%%--------------------------------------------------------------------
%% @end
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
terminate(State, [Option | Rest]) -> 
	case Option of 
		{log, LogRef} ->
			ok = datalog:close(LogRef);
		_ -> nothing
	end,
	terminate(State, Rest);
terminate(_State, []) ->
	get(caller) ! {
		get(prediction_list), 
		get(errorsLists)
	}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

reset_data() -> 
	put(data, #{}).

get_data() -> 
	get(data).

put_inputs(Inputs) -> 
	Data = get(data),
	put(data, Data#{inputs => Inputs}).

put_optima(Optima) -> 
	Data = get(data),
	put(data, Data#{optima => Optima}).
	
put_prediction(Prediction) -> 
	Data = get(data),
	put(data, Data#{prediction => Prediction}),
	PredictionList = get(prediction_list),
	put(prediction_list, [Prediction | PredictionList]).

put_errors(Errors) ->
	Data = get(data),
	put(data, Data#{errors => Errors}),
	ErrorsList = get(errors_list),
	put(errors_list, [Errors | ErrorsList]).


%%%===================================================================
%%% Results functions
%%%===================================================================

% ....................................................................
loss(#{errors := Errors} = Data) ->
	case get(calculate_loss) of
		true -> Data#{loss => ?LOSS(Errors)};
		_ ->  Data
	end;
loss(Data) ->
	Data.

% ....................................................................
datalog(Data) ->
	case get(log_ref) of 
		undefined -> nothing;
		LogRef -> datalog:write(LogRef, Data)
	end,
	Data.


%%====================================================================
%% Eunit white box tests
%%====================================================================

% ----------------------------------------------------------------------------------------------------------------------
% TESTS DESCRIPTIONS ---------------------------------------------------------------------------------------------------
white_test_() ->
	% {setup, Where, Setup, Cleanup, Tests | Instantiator}
	[
		{"Void example",
		 {setup, local, fun no_setup/0, fun no_cleanup/1, fun example/1}}
	].

% ----------------------------------------------------------------------------------------------------------------------
% SPECIFIC SETUP FUNCTIONS ---------------------------------------------------------------------------------------------
no_setup() ->
	ok.

no_cleanup(_) ->
	ok.

% ----------------------------------------------------------------------------------------------------------------------
% ACTUAL TESTS ---------------------------------------------------------------------------------------------------------
example(_) ->
	[
		?_assert(true)
	].

% ----------------------------------------------------------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS --------------------------------------------------------------------------------------------









