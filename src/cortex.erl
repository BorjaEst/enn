%%%-------------------------------------------------------------------
%%% @author borja
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Aug 2018 0:05
%%%-------------------------------------------------------------------
-module(cortex).
-compile([export_all, nowarn_export_all]). %% TODO: To delete after build

-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("nnelements.hrl").

-behaviour(gen_statem).

%% API
%%-export([start_link/0]).

%% gen_statem callbacks
-export([init/1, format_status/2, handle_event/4, terminate/3, code_change/4, callback_mode/0]).
-export([inactive/3, on_feedforward/3, on_backpropagation/3]).

-define(DEFAULT_BATCH_SIZE, 08).
-define(MIN_LOSS, 1.0e-2).

-record(input, {pid :: pid(), s :: float(), loss :: float(), lossB :: float(), acc = [] :: [float()]}).
-record(output, {pid :: pid(), s :: float(), error :: float()}).
-record(state, {
	batch :: integer(),
	wait :: [term()],
	from :: gen_statem:from()
}).

-ifdef(debug_mode).
-define(STDCALL_TIMEOUT, infinity).
-else.
-define(STDCALL_TIMEOUT, 1000).
-endif.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
% TODO: To make description and specs
new(Name, Compiled_Layers, Options) ->
	Cortex = nn_elements:create_cortex(Name, maps:map(fun elements/2, Compiled_Layers), Options),
	nndb:write(Cortex),
	[mutation:create_link(Cortex#cortex.id, To) || To <- get_inputs(Compiled_Layers)],
	[mutation:create_link(From, Cortex#cortex.id) || From <- get_outputs(Compiled_Layers)],
	Cortex#cortex.id.

elements(_, LayerInfo) ->
	element(2, LayerInfo).
get_inputs(Compiled_Layers) ->
	lists:append([ElementsLayer || {Type, ElementsLayer} <- maps:values(Compiled_Layers), Type == input]).
get_outputs(Compiled_Layers) ->
	lists:append([ElementsLayer || {Type, ElementsLayer} <- maps:values(Compiled_Layers), Type == output]).

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
% TODO: To make description and specs
start_link(Cortex_Id, Agent_PId) ->
	start_link(Cortex_Id, Agent_PId, ?DEFAULT_BATCH_SIZE).

start_link(Cortex_Id, Agent_PId, Batch_Size) ->
	NNSup_PId = self(),
	TId_IdPIds = ets:new(nn_idpids, [{read_concurrency, true}, public]),
	gen_statem:start_link(?MODULE,
		[
			{id, Cortex_Id},
			{agent, Agent_PId},
			{nn_sup, NNSup_PId},
			{batch_size, Batch_Size},
			{tid_idpids, TId_IdPIds}
		], []).

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
% TODO: To make description and specs
predict(Cortex_PId, ExternalInputs) ->
	_Predictions = gen_statem:call(Cortex_PId, {feedforward, ExternalInputs}, ?STDCALL_TIMEOUT).

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
% TODO: To make description and specs
fit(Cortex_PId, OptimalOutputs) ->
	_Errors = gen_statem:call(Cortex_PId, {backprop, OptimalOutputs}, ?STDCALL_TIMEOUT).

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
% TODO: To make description and specs
nn_id2pid(NNE_Id, TId_IdPIds) ->
	[{NNE_Id, NNE_PId}] = ets:lookup(TId_IdPIds, NNE_Id),
	NNE_PId.

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
% TODO: To make description and specs
nn_pid2id(NNE_PId, TId_IdPIds) ->
	[{NNE_PId, NNE_Id}] = ets:lookup(TId_IdPIds, NNE_PId),
	NNE_Id.


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
init(Arguments) ->
	[put(Key, Value) || {Key, Value} <- Arguments],
	put(perturbed, []),
	Id = get(id),
	ets:insert(get(tid_idpids), [{Id, self()}, {self(), Id}]),
	process_flag(trap_exit, true), % Mandatory to catch supervisor exits
	?LOG_INFO("Cortex_Id:~p initiated", [Id]),
	{ok, inactive, #state{
		batch = get(batch_size)
	},
	 [{next_event, internal, start_nn}]}.


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
%% @end
%%--------------------------------------------------------------------
%TODO: To add specs and description
inactive(enter, _OldState, #state{batch = 0} = State) ->
	put(inputs, [loss_calc(Input) || Input <- get(inputs)]),
	network_perturbation(),
	{keep_state, State#state{
		batch = get(batch_size)
	}};
inactive({call, From}, {feedforward, ExtInputs}, State) ->
	put(outputs, [forward(Output, ExtInput) || {Output, ExtInput} <- lists:zip(get(outputs), ExtInputs)]),
	{next_state, on_feedforward, State#state{
		from = From,
		wait = [Input#input.pid || Input <- get(inputs)]
	}};
inactive({call, From}, {backprop, Optimals}, State) ->
	put(inputs, [backward(Input, Optm) || {Input, Optm} <- lists:zip(get(inputs), Optimals)]),
	{next_state, on_backpropagation, State#state{
		from = From,
		wait = [Output#output.pid || Output <- get(outputs)]
	}};
inactive(internal, start_nn, State) ->
	case catch handle_start_nn() of
		{'EXIT', {broken_nn, _}} ->
			?LOG_NOTICE("Cortex Id: ~p, broken_nn", [get(id)]),
			{stop, normal};
		{'EXIT', Reason} -> {stop, Reason};
		_ -> {next_state, inactive, State}
	end;
inactive(EventType, EventContent, State) ->
	handle_common(EventType, EventContent, State).

%TODO: To add specs and description
on_feedforward(info, {PId, forward, Signal}, State) ->
	Input = lists:keyfind(PId, #input.pid, get(inputs)),
	put(inputs, lists:keyreplace(PId, #input.pid, get(inputs), Input#input{s = Signal})),
	on_feedforward(internal, forward, State#state{
		wait = lists:delete(PId, State#state.wait)
	});
on_feedforward(internal, forward, #state{wait = []} = State) ->
	{next_state, inactive, State,
	 [{reply, State#state.from, [Input#input.s || Input <- get(inputs)]}]};
on_feedforward(EventType, EventContent, State) ->
	handle_common(EventType, EventContent, State).

%TODO: To add specs and description
on_backpropagation(info, {PId, backward, BP_Error}, State) ->
	Output = lists:keyfind(PId, #output.pid, get(outputs)),
	put(outputs, lists:keyreplace(PId, #input.pid, get(outputs), Output#output{error = BP_Error})),
	on_backpropagation(internal, backward, State#state{
		wait = lists:delete(PId, State#state.wait)
	});
on_backpropagation(internal, backward, #state{wait = []} = State) ->
	{next_state, inactive, State#state{batch = State#state.batch - 1},
	 [{reply, State#state.from, [hd(Input#input.acc) || Input <- get(inputs)]}]};
on_backpropagation(EventType, EventContent, State) ->
	handle_common(EventType, EventContent, State).

%TODO: To add specs and description
handle_common(enter, _OldState, State) ->
	{keep_state, State};
handle_common(internal, _EventContent, State) ->
	{keep_state, State};
handle_common(EventType, EventContent, _State) ->
	error({"Cortex received an unknown Event -> ", EventType, EventContent}).


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
terminate(Reason, _StateName, _State) ->
	?LOG_INFO("Cortex_Id ~p terminating with reason ~p", [get(id), Reason]),
%%	ok = state_to_cortex(#state{} = _State, _TId_IdPIds). % TODO: Save changes
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

% ......................................................................................................................
% TODO: Not used for the moment, evaluate if needed with find,all->"state_to_cortex("
state_to_cortex(#state{} = _State, _TId_IdPIds) ->
%%	#cortex{},
	error("state_to_cortex fun not developed").

% ......................................................................................................................
forward(Output, Signal) ->
	Output#output.pid ! {self(), forward, Signal},
	Output#output{s = Signal}.

% ......................................................................................................................
backward(Input, Optm) ->
	Error = Optm - Input#input.s,
	Input#input.pid ! {self(), backward, Error},
	Input#input{acc = [Error | Input#input.acc]}.

% ......................................................................................................................
loss_calc(Input) ->
	Loss = math:sqrt(lists:sum([math:pow(E, 2) || E <- Input#input.acc])),
	Input#input{
		loss  = Loss,
		lossB = min(Loss, Input#input.lossB),
		acc   = []
	}.

% ......................................................................................................................
handle_start_nn() ->
	Cortex = nndb:read(get(id)), NNSup_PId = get(nn_sup), TId_IdPIds = get(tid_idpids),
	Neurons = [{start_nn_element(NNSup_PId, TId_IdPIds, N_Id), nndb:read(N_Id)} || N_Id <- nn_elements:neurons(Cortex)],
	[PId ! {continue_init, TId_IdPIds} || {PId, _} <- Neurons],
	put(neurons, maps:from_list(Neurons)),
	put(inputs, [#input{pid = cortex:nn_id2pid(Id, TId_IdPIds)} || {Id, _} <- Cortex#cortex.inputs_idps]),
	put(outputs, [#output{pid = cortex:nn_id2pid(Id, TId_IdPIds)} || Id <- Cortex#cortex.outputs_ids]).

start_nn_element(NNSup_PId, TId_IdPIds, Neuron_Id) ->
	case nn_sup:start_neuron(NNSup_PId, Neuron_Id) of
		{ok, PId} ->
			ets:insert(TId_IdPIds, [{Neuron_Id, PId}, {PId, Neuron_Id}]),
			PId;
		{error, Reason} ->
			exit(Reason)
	end.

% ......................................................................................................................
network_perturbation() ->
	
	?LOG_NOTICE("LOSS__: ~p", [[{Input#input.loss, Input#input.lossB} || Input <- get(inputs)]]),
	
	[do_network_perturbation(Input) || Input <- get(inputs)],
	ok.


% TODO: Improve setting the perturbation only on neurons related with the error

do_network_perturbation(#input{loss = L1, lossB = L2} = _Input) when L1 < L2, L1 > ?MIN_LOSS ->
	
	?LOG_NOTICE("----> backup & perturb"),
	
	weights_backup(),
	weights_perturb();

do_network_perturbation(#input{loss = L1, lossB = L2} = _Input) when L1 >= L2, L1 > ?MIN_LOSS ->
	
	?LOG_NOTICE("----> restore & perturb"),
	weights_restore(),
	weights_perturb();

do_network_perturbation(_Input) ->
	nothing.


% ......................................................................................................................
weights_backup() ->
	[NPId ! weights_backup || NPId <- maps:keys(get(neurons))],
	put(perturbed, []).


% ......................................................................................................................
weights_perturb() ->
	Neurons = get(neurons),
	MP = 1 / math:sqrt(maps:size(Neurons)),
	Perturbed_NPIds = [NPId || NPId <- maps:keys(Neurons), rand:uniform() < MP],
	[NPId ! weights_perturb || NPId <- Perturbed_NPIds],
	put(perturbed, Perturbed_NPIds).

% ......................................................................................................................
weights_restore() ->
	[NPId ! weights_restore || NPId <- get(perturbed)],
	put(perturbed, []).


%%====================================================================
%% Eunit white box tests
%%====================================================================

% ----------------------------------------------------------------------------------------------------------------------
% TESTS DESCRIPTIONS ---------------------------------------------------------------------------------------------------
cortex_white_test_() ->
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









