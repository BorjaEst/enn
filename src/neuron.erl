%%%-------------------------------------------------------------------
%%% @author borja
%%% @copyright (C) 2018, <COMPANY>
%%% @doc The neuron is a signal processing element. It accepts
%%% signals, accumulates them into an ordered vector, then processes
%%% this input vector to produce an output, and finally passes the
%%% output to other elements it is connected to.
%%%
%%% The neuron waits until it receives all the input signals, 
%%% processes those signals, and then passes the output forward.
%%% @end
%%% Created : 16. Aug 2018 15:20
%%%-------------------------------------------------------------------
-module(neuron).
-compile([export_all, nowarn_export_all]). %% TODO: To delete after build

% TODO: fsm?? with forward or other according to the best performance
% TODO: Optimise network when shutdown (Part can be on the shutdown, another in the clonation¿?)

-include_lib("math_constants.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
%%-export([]).
-export_type([id/0]).

-type id() :: {{LayerCoordinate :: float(), Unique_Id :: reference()}, neuron} | [].

-record(input, {id :: id(), w :: float(), r :: boolean(), s = 0.0 :: float()}).
-record(output, {id :: id(), r :: boolean(), e = 0.0 :: float()}).
-record(tensor, {bias :: float(), in :: #{pid() => {W :: float(), S :: float()}}, soma :: float(), signal :: float()}).
-record(state, {
	id :: id(),
	af :: activation:func(),
	aggrf :: aggregation:func(),
	bias :: float(), % Last bias value
	tensor :: #tensor{},
	inputs :: #{Pid :: id() => #input{}},
	outputs :: #{Pid :: id() => #output{}},
	forward_wait :: [pid()],
	backward_wait :: [pid()],
	error :: float()
}).

% -define(LEARNING_FACTOR, rand:uniform(10)/20).
% -define(MOMENTUM_FACTOR, rand:uniform(10)/100)
-define(LEARNING_FACTOR, 0.20).  
-define(MOMENTUM_FACTOR, 0.00).
-define(SAT_LIMIT, 3.0 * ?PI).
-define(R2(Val), round(Val * 100.0) / 100.0).


-ifdef(debug_mode).
-define(STDCALL_TIMEOUT, infinity).
-define(STDIDLE_TIMEOUT, infinity).
-else.
-define(STDCALL_TIMEOUT, 1000).
-define(STDIDLE_TIMEOUT, 5000).
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
new(Layer, AF, AggrF, Options) ->
	Neuron = elements:neuron(Layer, AF, AggrF, Options),
	edb:write(Neuron),
	elements:id(Neuron).

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
% TODO: To make description and specs
start_link(Neuron_Id) ->
	proc_lib:start_link(?MODULE, init, [Neuron_Id, self()]).


%%%===================================================================
%%% neuron callbacks
%%%===================================================================

% ......................................................................................................................
%TODO: To add specs and description
init(Neuron_Id, Parent) ->
	Neuron = edb:read(Neuron_Id),
	check_neuron(Neuron),
	process_flag(trap_exit, true), % Mandatory to catch supervisor exits
	proc_lib:init_ack(Parent, {ok, self()}),                % Supervisor synchronisation
	TId = receive {continue_init, TableId} -> TableId end,   % Cortex synchronisation
	init2(#state{
		id      = elements:id(Neuron),
		af      = elements:af(Neuron),
		aggrf   = elements:aggrf(Neuron),
		bias    = elements:bias(Neuron),
		inputs  = maps:from_list(
			[{cortex:nn_id2pid(Id, TId), #input{id = Id, w = W, r = false}} || {Id, W} <- elements:inputs_idps(Neuron)] ++
			[{cortex:nn_id2pid(Id, TId), #input{id = Id, w = W, r = true}} || {Id, W} <- elements:rcc_inputs_idps(Neuron)]
		),
		outputs = maps:from_list(
			[{cortex:nn_id2pid(Id, TId), #output{id = Id, r = false}} || Id <- elements:outputs_ids(Neuron)] ++
			[{cortex:nn_id2pid(Id, TId), #output{id = Id, r = true}} || Id <- elements:rcc_outputs_ids(Neuron)]
		),
		error   = 0.0
	}).

init2(State) ->
	Tensor = tensor(State), put(tensor, Tensor),
	B = activation:beta(State#state.af, State#state.error, Tensor#tensor.soma),
	forward(maps:filter(fun is_recurrent/2, State#state.outputs), Tensor#tensor.signal), %Recurrence init
	backward(maps:filter(fun is_recurrent/2, State#state.inputs), B), %Recurrence init
	loop(State#state{
		tensor        = Tensor,
		forward_wait  = [Pid || Pid <- maps:keys(State#state.inputs)],
		backward_wait = [Pid || Pid <- maps:keys(State#state.outputs)]
	}).

% ......................................................................................................................
%TODO: To add specs and description
loop(#state{forward_wait = []} = State) ->
	loop(forward_signal(State));
loop(#state{backward_wait = []} = State) ->
	loop(backward_error(State));
loop(#state{forward_wait = [NextF | RForwardWait], backward_wait = [NextB | RBackwardWait]} = State) ->
	receive
		{NextF, forward, Signal} ->
			#{NextF := Input} = Inputs = State#state.inputs,
			loop(State#state{
				forward_wait = RForwardWait,
				inputs       = Inputs#{NextF := Input#input{s = Signal}}
			});
		{NextB, backward, BP_Error} ->
			#{NextB := Output} = Outputs = State#state.outputs,
			loop(State#state{
				backward_wait = RBackwardWait,
				outputs       = Outputs#{NextB := Output#output{e = BP_Error}},
				error         = State#state.error + BP_Error
			});
		{'EXIT', _Pid, Reason} ->
			terminate(State, Reason)
	after ?STDIDLE_TIMEOUT ->
		#state{id = Neuron_Id} = State,
		?LOG_WARNING("neuron ~p stuck ", [Neuron_Id])
	end.

%TODO: To add specs and description
forward_signal(State) -> %TODO: save on signals_acc the "Tensor" (modifying tensor & aggregation_function)
	Tensor = tensor(State),
	forward(State#state.outputs, Tensor#tensor.signal),
	State#state{
		tensor       = Tensor,
		forward_wait = [Pid || Pid <- maps:keys(State#state.inputs)]
	}.

%TODO: To add specs and description
backward_error(State) ->
	Tensor = State#state.tensor,
	DTensor = d_tensor(Tensor),
	B = activation:beta(State#state.af, State#state.error, Tensor#tensor.soma),
	backward(State#state.inputs, B),
	State#state{
		bias          = adjust_bias(Tensor#tensor.bias, DTensor#tensor.bias, B),
		inputs        = adjust_weights(Tensor#tensor.in, DTensor#tensor.in, State#state.inputs, B),
		backward_wait = [Pid || Pid <- maps:keys(State#state.outputs)],
		error         = 0.0
	}.

%TODO: To add specs and description
terminate(State, Reason) ->
	% TODO: When saving the new state, those links with weights ~= 0, must be deleted (both neurons)
	% TODO: If the neuron has not at least 1 input or 1 output, it must be deleted (and bias forwarded)
	Neuron = elements:edit(
		elements:neuron(),
		[
			{id, State#state.id},
			{af, State#state.af},
			{aggrf, State#state.aggrf},
			{bias, State#state.bias},
			{inputs_idps, [{I#input.id, I#input.w} || {_, #input{r = false} = I} <- maps:to_list(State#state.inputs)]},
			{outputs_ids, [O#output.id || {_, #output{r = false} = O} <- maps:to_list(State#state.outputs)]},
			{rcc_inputs_idps, [{I#input.id, I#input.w} || {_, #input{r = true} = I} <- maps:to_list(State#state.inputs)]},
			{rcc_outputs_ids, [O#output.id || {_, #output{r = true} = O} <- maps:to_list(State#state.outputs)]}
		]),
	edb:write(Neuron),
	exit(Reason).


%%%===================================================================
%%% Internal functions
%%%===================================================================

% ......................................................................................................................
check_neuron(Neuron) ->
	try
		true = elements:is_neuron(Neuron),
		false = [] == elements:inputs_idps(Neuron) ++ elements:rcc_inputs_idps(Neuron),
		false = [] == elements:outputs_ids(Neuron) ++ elements:rcc_outputs_ids(Neuron)
	catch
		error:{badmatch, _} ->
			?LOG_NOTICE("broken network on neuron: ~p", [Neuron]),
			exit(broken_nn)
	end.

% ......................................................................................................................
is_recurrent(_Key, Value) when is_record(Value, input) ->
	Value#input.r;
is_recurrent(_Key, Value) when is_record(Value, output) ->
	Value#output.r.

% ......................................................................................................................
tensor(State) ->
	Tensor_In = maps:from_list(
		[{Pid, {Input#input.w, Input#input.s}} || {Pid, Input} <- maps:to_list(State#state.inputs)]),
	Soma = aggregation:apply(State#state.aggrf, maps:values(Tensor_In), State#state.bias),
	Signal = activation:apply(State#state.af, Soma),
	#tensor{
		bias   = State#state.bias,
		in     = Tensor_In,
		soma   = Soma,
		signal = Signal
	}.

d_tensor(Tensor) ->
	PrevTensor = put(tensor, Tensor),
	#tensor{
		bias   = Tensor#tensor.bias - PrevTensor#tensor.bias,
		in     = d_tensor_in(Tensor#tensor.in, maps:iterator(PrevTensor#tensor.in)),
		soma   = Tensor#tensor.soma - PrevTensor#tensor.soma,
		signal = Tensor#tensor.signal - PrevTensor#tensor.signal
	}.

d_tensor_in(ThisTensor_In, none) ->
	ThisTensor_In;
d_tensor_in(ThisTensor_In, Iterator) ->
	{Pid, {W1, V1}, Next_Iterator} = maps:next(Iterator),
	#{Pid := {W2, V2}} = ThisTensor_In,
	DiffW = case {W2, W1} of
		        {cortex, cortex} -> cortex;
		        _ -> W2 - W1
	        end,
	d_tensor_in(ThisTensor_In#{Pid := {DiffW, V2 - V1}}, Next_Iterator).

% ......................................................................................................................
forward(Outputs, Signal) ->
	[forward(Pid, void, Signal) || {Pid, _Output} <- maps:to_list(Outputs)].

forward(Output_Pid, _Void, Signal) ->
	Output_Pid ! {self(), forward, Signal},
	{Output_Pid, Signal}.

% ......................................................................................................................
backward(Inputs, B) ->
	[backward(Pid, Input#input.w, B) || {Pid, Input} <- maps:to_list(Inputs)].

backward(Input_Pid, cortex, B) ->
	Input_Pid ! {self(), backward, BP_Error = B},
	{Input_Pid, BP_Error};
backward(Input_Pid, Weight, B) ->
	Input_Pid ! {self(), backward, BP_Error = Weight * B},
	{Input_Pid, BP_Error}.

% ......................................................................................................................
adjust_weights(TensorIn, OldTensor_In, Inputs, B) ->
	do_adjust_weights(maps:to_list(TensorIn), OldTensor_In, Inputs, B).

do_adjust_weights([{Pid, {_, Signal}} | Signals_Acc], OldTensor_In, Inputs, B) ->
	case maps:get(Pid, Inputs) of
		#input{w = cortex} = _Input ->
			do_adjust_weights(Signals_Acc, OldTensor_In, Inputs, B);
		#input{w = W} = Input ->
			#{Pid := {D_W, _D_S}} = OldTensor_In,
			New_W = sat(W + (?LEARNING_FACTOR * B * Signal) + (?MOMENTUM_FACTOR * D_W), -?SAT_LIMIT, ?SAT_LIMIT),
			do_adjust_weights(Signals_Acc, OldTensor_In, Inputs#{Pid := Input#input{w = New_W}}, B)
	end;
do_adjust_weights([], _OldTensor_In, Inputs, _B) ->
	Inputs.

% ......................................................................................................................
adjust_bias(Bias, D_Bias, B) ->
	sat(Bias + (?LEARNING_FACTOR * B) + (?MOMENTUM_FACTOR * D_Bias), -?SAT_LIMIT, ?SAT_LIMIT).

% ......................................................................................................................
replace_weight(Pid, #input{w = Weight}, Inputs) ->
	#{Pid := Input} = Inputs,
	maps:update(Pid, Input#input{w = Weight}, Inputs).

% ......................................................................................................................
sat(Val, Min, _) when Val < Min -> Min;
sat(Val, _, Max) when Val > Max -> Max;
sat(Val, _, _)                  -> Val.

	
	
	
