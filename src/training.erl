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
-type_export([option/0, result/0]).

%% gen_statem callbacks
-export([init/5, predict/3, fit/3, results/3, terminate/1]).

-type option() :: {return, Returns :: [return()]} 
				| loss
				| log.
-type return() :: prediction
				| errors
				| loss.
-type result() :: [float()]
				| undefined.

-record(state, {
	inputsList = [],
    optimaList = [],
	calculate_loss = false,
	logRef = undefined
}).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Executes a training (or only prediction) session.
%% @end
%%--------------------------------------------------------------------
-spec start_link(Cortex_PId :: pid(), Inputs :: [float()], 
                 Optima :: [float()], Options :: [option()]) ->
	Resutls :: [result()].
start_link(Cortex_PId, Inputs, Optima, Options) ->
	spawn_link(?MODULE,init,
		[self(), Cortex_PId, Inputs, Optima, Options]
	),
	receive {training, Returns} -> Returns end.


%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Executes the preparations for the training cycles. 
%% @end
%%--------------------------------------------------------------------
init(Caller, Cortex_PId, InputsList, OptimaList, Options) -> 
	put(caller_pid, Caller),
	put(cortex_pid, Cortex_PId),
	put(prediction_list, []),
	put(errors_list, []),
	init(Options, #state{
		inputsList = InputsList,
    	optimaList = OptimaList
	}).

init([{return, Returns} | Options], State) ->
	put(return, Returns),
	init(Options, State);
init([loss | Options], State) ->
	put(loss_list, []),
	init(Options, State#state{calculate_loss = true});
init([{log, LogName} | Options], State) ->
	init(Options, State#state{logRef = datalog:new(LogName)});
init([], State) -> 
	predict(enter, init, State).

%%--------------------------------------------------------------------
%% @private
%% @doc Executes the training cycle
%%--------------------------------------------------------------------
%% predict: Performs a prediction from the input	
%%--------------------------------------------------------------------
predict(enter, _OldState, #state{inputsList = []} = State) ->
	terminate(State);
predict(enter, _OldState, State) ->
	[Inputs | InputsList] = State#state.inputsList,
	Data = #{inputs => Inputs},
	predict(internal, Data, State#state{inputsList = InputsList});
predict(internal, Data, State) -> 
    Inputs = maps:get(inputs, Data),
	Cortex = get(cortex_pid),
	Prediction = cortex:predict(Cortex, Inputs),
	put(data, Data#{prediction => Prediction}),
	put(prediction_list, [Prediction | get(prediction_list)]),
	fit(enter, predict, State).

%%--------------------------------------------------------------------
%% fit: Trains the model if optima available
%%--------------------------------------------------------------------
fit(enter, _OldState, #state{optimaList = []} = State) ->
	results(enter, fit, State);
fit(enter, _OldState, State) -> 
	[Optima | OptimaList] = State#state.optimaList,
	Data = maps:put(optima, Optima, get(data)),
	fit(internal, Data, State#state{optimaList = OptimaList});
fit(internal, Data, State) -> 
    Optima = maps:get(optima, Data),
	Cortex = get(cortex_pid),
	Errors = cortex:fit(Cortex, Optima),
	put(data, Data#{errors => Errors}),
	put(errors_list, [Errors | get(errors_list)]),
	results(enter, fit, State).

%%--------------------------------------------------------------------
%% results: Applies the result functions (Loss calculation, log, etc.)
%%--------------------------------------------------------------------

results(enter, _OldState, State) ->
	Data = get(data),
	results(internal, Data, State);
results(internal, Data, #state{calculate_loss = true} = State) 
when not is_map_key(loss, Data) ->
	Loss = ?LOSS(maps:get(errors, Data)),
	put(loss_list, [Loss | get(loss_list)]),
	results(internal, Data#{loss => Loss}, State);
results(internal, Data, #state{logRef = {ok, LogRef}} = State) ->
	datalog:write(LogRef, Data),
	results(internal, Data, State);
results(internal, _Data, State) -> 
	predict(enter, results, State). 
%%--------------------------------------------------------------------
%% @end
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @private
%% @doc Cleans the training and returns the results.
%% @end
%%--------------------------------------------------------------------
terminate(#state{logRef = {ok, LogRef}} = State) -> 
	ok = datalog:close(LogRef),
	terminate(State#state{logRef = closed});
terminate(_State) ->
	get(caller_pid) ! {
		training,
		return(get(return))
	}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

return([prediction | Rest]) -> 
	[get(prediction_list) | return(Rest)];
return([errors | Rest]) -> 
	[get(errors_list) | return(Rest)];
return([loss | Rest]) -> 
	[get(loss_list) | return(Rest)];
return([]) -> 
	[].


%%====================================================================
%% Eunit white box tests
%%====================================================================

% --------------------------------------------------------------------
% SPECIFIC SETUP FUNCTIONS -------------------------------------------
pred_err_lists() ->
	put(prediction_list, [random_list(3) || _ <- lists:seq(1, 2)]),
	put(errors_list, [random_list(3) || _ <- lists:seq(1, 2)]),
	ok.

with_loss_lists() ->
	pred_err_lists(),
	put(loss_list, [random_list(3) || _ <- lists:seq(1, 2)]).

no_cleanup(_) ->
	ok.

% --------------------------------------------------------------------
% TESTS DESCRIPTIONS -------------------------------------------------
white_test_() ->
	% {setup, Where, Setup, Cleanup, Tests | Instantiator}
	[
		{"Tests for basic returns (predictions and errors)",
		 {setup, local, fun pred_err_lists/0, fun no_cleanup/1, 
		  [
			?_assert([get(prediction_list)] == 
				      return([prediction])),
			?_assert([undefined] == 
				      return([loss])),
			?_assert([get(errors_list), get(prediction_list)] ==
					  return([errors, prediction])),
			?_assert([get(prediction_list), get(errors_list)] ==
					  return([prediction, errors]))
		  ]}},
		{"Tests for returns with loss",
		 {setup, local, fun with_loss_lists/0, fun no_cleanup/1, 
		  [
			?_assert([get(prediction_list)] == 
					  return([prediction])),
			?_assert([get(loss_list)] == 
				      return([loss])),
			?_assert([get(errors_list), get(loss_list)] ==
					  return([errors, loss])),
			?_assert([get(loss_list), get(errors_list)] ==
					  return([loss, errors]))
		  ]}}

	].

% --------------------------------------------------------------------
% ACTUAL TESTS -------------------------------------------------------


% --------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS ------------------------------------------

random_list(N) -> 
	[(rand:uniform(20) - 10) /10 || _ <- lists:seq(1, N)].







