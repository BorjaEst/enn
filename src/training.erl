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

-behaviour(gen_statem).

%% API
%%-export([start_link/0]).
-export_type([id/0]).

%% gen_statem callbacks
-export([init/1, format_status/2, handle_event/4, terminate/3, 
     	 code_change/4, callback_mode/0]).
-export([predict/3, fit/3, results/3]).

-record(state, {
	inputsList,
    optimaList
}).


%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc  
%% @end
%%--------------------------------------------------------------------
-spec start_link() ->
	gen_statem:start_ret().
start_link(x) -> 
	ok.

%%--------------------------------------------------------------------
%% @doc Supervised ANN training function. Fits the Predictions to the 
%% Optima.
%% @end
%%--------------------------------------------------------------------
-spec start(Cortex_PId :: pid(), Inputs :: [float()], 
          Optima :: [float()]) ->
	{Loss :: [float()], Predictions :: [float()]}.

start_link(Cortex_PId, Inputs, Optima) ->
	start_link(Cortex_PId, Inputs, Optima, []). 

start_link(Cortex_PId, Inputs, Optima, Options) ->
	gen_statem:start_ret(
		?MODULE, 
		[
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
%% Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {CallbackMode, StateName, State} |
%%                     {CallbackMode, StateName, State, Actions} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init(Cortex_PId, InputsList, OptimaList, [Option | Rest]) ->
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
	{ok, predict, #state{
		inputsList = InputsList,
    	optimaList = OptimaList
	}}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it needs to find out 
%% the callback mode of the callback module.
%%
%% @spec callback_mode() -> atom().
%% @end
%%--------------------------------------------------------------------
callback_mode() ->
	[
		state_functions,
		state_enter
	].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Called (1) whenever sys:get_status/1,2 is called by gen_statem or
%% (2) when gen_statem terminates abnormally.
%% This callback is optional.
%%
%% @spec format_status(Opt, [PDict, StateName, State]) -> term()
%% @end
%%--------------------------------------------------------------------
format_status(_Opt, [_PDict, _StateName, _State]) ->
	Status = some_term,
	Status.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name.  If callback_mode is state_functions, one of these
%% functions is called when gen_statem receives and event from
%% call/2, cast/2, or as a normal process message.
%%
%% @spec state_name(EventType, EventContent, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Actions} |
%%                   {stop, Reason, NewState} |
%%    				 stop |
%%                   {stop, Reason :: term()} |
%%                   {stop, Reason :: term(), NewData :: data()} |
%%                   {stop_and_reply, Reason, Replies} |
%%                   {stop_and_reply, Reason, Replies, NewState} |
%%                   {keep_state, NewData :: data()} |
%%                   {keep_state, NewState, Actions} |
%%                   keep_state_and_data |
%%                   {keep_state_and_data, Actions}
%% 
%%--------------------------------------------------------------------
%% predict: Performs a prediction from the input	
%%--------------------------------------------------------------------
% prediction and training
predict(enter, _OldState, #state{inputsList = []} = State) ->
	{stop, loop_end, State};
predict(enter, _OldState, State) -> 
	reset_data(),
	predict(internal, State#state.inputsList, State);
predict(internal, [Inputs | InputsList], State) -> 
	put_inputs(Inputs),
	put_prediction(cortex:predict(get(cortex_pid), Inputs)),
	{next_state, fit, State#state{
		inputsList = InputsList
	}}.
%%--------------------------------------------------------------------
%% fit: Trains the model if optima available
%%--------------------------------------------------------------------
fit(enter, _OldState, #state{optimaList = []} = State) ->
	{next_state, results, State};
fit(enter, _OldState, State) -> 
	fit(internal, State#state.optimaList, State);
fit(internal, [Optima | OptimaList], State) -> 
	put_optima(Optima),
	put_errors(cortex:fit(get(cortex_pid), Optima)),
	{next_state, results, State#state{
		optimaList = OptimaList
	}}.
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
	{next_state, predict, State}. 
%%--------------------------------------------------------------------
%% @end
%%--------------------------------------------------------------------



%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Actions} |
%%                   {stop, Reason, NewState} |
%%    				 stop |
%%                   {stop, Reason :: term()} |
%%                   {stop, Reason :: term(), NewData :: data()} |
%%                   {stop_and_reply, Reason, Replies} |
%%                   {stop_and_reply, Reason, Replies, NewState} |
%%                   {keep_state, NewData :: data()} |
%%                   {keep_state, NewState, Actions} |
%%                   keep_state_and_data |
%%                   {keep_state_and_data, Actions}
%% @end
%%--------------------------------------------------------------------
handle_event(_EventType, _EventContent, _StateName, State) ->
	NextStateName = the_next_state_name,
	{next_state, NextStateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(loop_end, _StateName, State) -> 
	do_something().


end_fit(ExtIn, OptOut, Loss, Pred, [Option | Rest]) -> 
	case Option of 
		{log, LogRef} ->
			ok = datalog:close(LogRef)
	end,
	end_fit(ExtIn, OptOut, Loss, Pred, Rest);
end_fit(_ExtIn, _OptOut, Loss, Pred, []) ->
	{Loss, Pred}.


terminate(Reason, _StateName, _State) ->
	?LOG_INFO("Cortex_Id ~p terminating with reason ~p", [get(id), Reason]),
	% Cortex = elements:cortex(CompiledLayers, 
	% 	[
	% 		{id, get(id)}
	% 		{outputs_ids, something}
	% 		{inputs_idps, something}
	% 	]),
	% nndb:write(Cortex),
	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
	{ok, StateName, State}.

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









