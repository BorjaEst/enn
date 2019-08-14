%%%-------------------------------------------------------------------
%%% @author borja
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Aug 2018 15:20
%%%-------------------------------------------------------------------
-module(neuron).
-compile([export_all, nowarn_export_all]). %% TODO: To delete after build

% TODO: fsm?? with forward or other according to the best performance
% TODO: Optimise network when shutdown (Part can be on the shutdown, another in the clonation¿?)

-include_lib("kernel/include/logger.hrl").
-include_lib("nnelements.hrl").

%% API
%%-export([]).

-record(input, {id :: neuron_id(), w :: float(), r :: boolean(), s = 0.0 :: float()}).
-record(output, {id :: neuron_id(), r :: boolean(), e = 0.0 :: float()}).
-record(tensor, {bias :: float(), in :: #{pid() => {W :: float(), S :: float()}}, soma :: float(), signal :: float()}).
-record(state, {
	id :: neuron_id(),
	af :: function(), % Activation function
	aggrf :: function(), % Name of the aggregation function
	bias :: float(), % Last bias value
	tensor :: #tensor{},
	inputs :: #{PId :: neuron_id() => #input{}},
	outputs :: #{PId :: neuron_id() => #output{}},
	forward_wait :: [pid()],
	backward_wait :: [pid()],
	error :: float()
}).

-define(LEARNING_FACTOR, 0.00).  % TODO: Make it modifiable according to the error and by the eevo module
-define(MOMENTUM_FACTOR, 0.00).
-define(SAT_LIMIT, 1.5 * ?DELTA_MULTIPLIER).

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
	Neuron = nn_elements:create_neuron(Layer, AF, AggrF, Options),
	nndb:write(Neuron),
	Neuron#neuron.id.

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
	Neuron = nndb:read(Neuron_Id),
	check_neuron(Neuron),
	process_flag(trap_exit, true), % Mandatory to catch supervisor exits
	proc_lib:init_ack(Parent, {ok, self()}),                % Supervisor synchronisation
	TId = receive {continue_init, TableId} -> TableId end,   % Cortex synchronisation
	init2(#state{
		id      = Neuron#neuron.id,
		af      = Neuron#neuron.af,
		aggrf   = Neuron#neuron.aggrf,
		bias    = Neuron#neuron.bias,
		inputs  = maps:from_list(
			[{cortex:nn_id2pid(Id, TId), #input{id = Id, w = W, r = false}} || {Id, W} <- Neuron#neuron.inputs_idps] ++
			[{cortex:nn_id2pid(Id, TId), #input{id = Id, w = W, r = true}} || {Id, W} <- Neuron#neuron.rcc_inputs_idps]
		),
		outputs = maps:from_list(
			[{cortex:nn_id2pid(Id, TId), #output{id = Id, r = false}} || Id <- Neuron#neuron.outputs_ids] ++
			[{cortex:nn_id2pid(Id, TId), #output{id = Id, r = true}} || Id <- Neuron#neuron.rcc_outputs_ids]
		),
		error   = 0.0
	}).

init2(State) ->
	Tensor = tensor(State), put(tensor, Tensor),
	B = activation_function:beta(State#state.af, State#state.error, Tensor#tensor.soma),
	forward(maps:filter(fun is_recurrent/2, State#state.outputs), Tensor#tensor.signal), %Recurrence init
	backward(maps:filter(fun is_recurrent/2, State#state.inputs), B), %Recurrence init
	loop(State#state{
		tensor        = Tensor,
		forward_wait  = [PId || PId <- maps:keys(State#state.inputs)],
		backward_wait = [PId || PId <- maps:keys(State#state.outputs)]
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
		weights_backup ->
			
			?LOG_NOTICE("-------- Neuron backup: ~p", [
				[I#input.w || I <- maps:values(State#state.inputs)]]),
			
			
			loop(weights_backup(State));
		weights_perturb ->
			
			?LOG_NOTICE("-------- Neuron perturb prev: ~p", [
				[I#input.w || I <- maps:values(State#state.inputs)]]),
			NewState = weights_perturb(State),
			?LOG_NOTICE("-------- Neuron perturb aftr: ~p", [
				[I#input.w || I <- maps:values(NewState#state.inputs)]]),
			loop(NewState);


%%			loop(weights_perturb(State));
		weights_restore ->
			
			?LOG_NOTICE("-------- Neuron restore prev: ~p", [
				[I#input.w || I <- maps:values(State#state.inputs)]]),
			NewState = weights_restore(State),
			?LOG_NOTICE("-------- Neuron restore aftr: ~p", [
				[I#input.w || I <- maps:values(NewState#state.inputs)]]),
			loop(NewState);


%%			loop(weights_restore(State));
		{'EXIT', _PId, Reason} ->
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
		forward_wait = [PId || PId <- maps:keys(State#state.inputs)]
	}.

%TODO: To add specs and description
backward_error(State) ->
	Tensor = State#state.tensor,
	DTensor = d_tensor(Tensor),
	B = activation_function:beta(State#state.af, State#state.error, Tensor#tensor.soma),
	backward(State#state.inputs, B),
	State#state{
		bias          = adjust_bias(Tensor#tensor.bias, DTensor#tensor.bias, B),
		inputs        = adjust_weights(Tensor#tensor.in, DTensor#tensor.in, State#state.inputs, B),
		backward_wait = [PId || PId <- maps:keys(State#state.outputs)],
		error         = 0.0
	}.

%TODO: To add specs and description
weights_backup(State) ->
	put(weights, #{
		bias   => State#state.bias,
		inputs => State#state.inputs
	}),
	State.

%TODO: To add specs and description
weights_perturb(State) ->
	NewState = weights_backup(State),
	MPf = perturb_f(1 / math:sqrt(maps:size(NewState#state.inputs))),
	NewState#state{
		bias   = MPf(bias, NewState#state.bias),
		inputs = maps:map(MPf, NewState#state.inputs)
	}.

%TODO: To add specs and description
weights_restore(State) ->
	#{
		bias   := OldBias,
		inputs := OldInputs
	} = get(weights),
	State#state{
		bias   = OldBias,
		inputs = maps:fold(fun replace_weight/3, State#state.inputs, OldInputs)
	}.

%TODO: To add specs and description
terminate(State, Reason) ->
	% TODO: When saving the new state, those links with weights ~= 0, must be deleted (both neurons)
	% TODO: If the neuron has not at least 1 input or 1 output, it must be deleted (and bias forwarded)
	nndb:write(#neuron{
		id              = State#state.id,
		af              = State#state.af,
		aggrf           = State#state.aggrf,
		bias            = State#state.bias,
		inputs_idps     = [{I#input.id, I#input.w} || {_, #input{r = false} = I} <- maps:to_list(State#state.inputs)],
		outputs_ids     = [O#output.id || {_, #output{r = false} = O} <- maps:to_list(State#state.outputs)],
		rcc_inputs_idps = [{I#input.id, I#input.w} || {_, #input{r = true} = I} <- maps:to_list(State#state.inputs)],
		rcc_outputs_ids = [O#output.id || {_, #output{r = true} = O} <- maps:to_list(State#state.outputs)]
	}),
	exit(Reason).


%%%===================================================================
%%% Internal functions
%%%===================================================================

% ......................................................................................................................
check_neuron(Neuron) ->
	try
		#neuron{} = Neuron,
		false = [] == nn_elements:inputs_idps(Neuron),
		false = [] == nn_elements:outputs_ids(Neuron)
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
		[{PId, {Input#input.w, Input#input.s}} || {PId, Input} <- maps:to_list(State#state.inputs)]),
	Soma = aggregation_function:apply(State#state.aggrf, maps:values(Tensor_In), State#state.bias),
	Signal = activation_function:apply(State#state.af, Soma),
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
	{PId, {W1, V1}, Next_Iterator} = maps:next(Iterator),
	#{PId := {W2, V2}} = ThisTensor_In,
	DiffW = case {W2, W1} of
		        {cortex, cortex} -> cortex;
		        _ -> W2 - W1
	        end,
	d_tensor_in(ThisTensor_In#{PId := {DiffW, V2 - V1}}, Next_Iterator).

% ......................................................................................................................
forward(Outputs, Signal) ->
	[forward(PId, void, Signal) || {PId, _Output} <- maps:to_list(Outputs)].

forward(Output_PId, _Void, Signal) ->
	Output_PId ! {self(), forward, Signal},
	{Output_PId, Signal}.

% ......................................................................................................................
backward(Inputs, B) ->
	[backward(PId, Input#input.w, B) || {PId, Input} <- maps:to_list(Inputs)].

backward(Input_PId, cortex, B) ->
	Input_PId ! {self(), backward, BP_Error = B},
	{Input_PId, BP_Error};
backward(Input_PId, Weight, B) ->
	Input_PId ! {self(), backward, BP_Error = Weight * B},
	{Input_PId, BP_Error}.

% ......................................................................................................................
adjust_weights(TensorIn, OldTensor_In, Inputs, B) ->
	do_adjust_weights(maps:to_list(TensorIn), OldTensor_In, Inputs, B).

do_adjust_weights([{PId, {_, Signal}} | Signals_Acc], OldTensor_In, Inputs, B) ->
	case maps:get(PId, Inputs) of
		#input{w = cortex} = _Input ->
			do_adjust_weights(Signals_Acc, OldTensor_In, Inputs, B);
		#input{w = W} = Input ->
			#{PId := {D_W, _D_S}} = OldTensor_In,
			New_W = sat(W + (?LEARNING_FACTOR * B * Signal) + (?MOMENTUM_FACTOR * D_W), -?SAT_LIMIT, ?SAT_LIMIT),
			do_adjust_weights(Signals_Acc, OldTensor_In, Inputs#{PId := Input#input{w = New_W}}, B)
	end;
do_adjust_weights([], _OldTensor_In, Inputs, _B) ->
	Inputs.

% ......................................................................................................................
adjust_bias(Bias, D_Bias, B) ->
	sat(Bias + (?LEARNING_FACTOR * B) + (?MOMENTUM_FACTOR * D_Bias), -?SAT_LIMIT, ?SAT_LIMIT).

% ......................................................................................................................
replace_weight(PId, #input{w = Weight}, Inputs) ->
	#{PId := Input} = Inputs,
	maps:update(PId, Input#input{w = Weight}, Inputs).

% ......................................................................................................................
perturb_f(MP) ->
	fun
		(_PId, #input{w = cortex} = Input) -> Input;
		(_PId, #input{w = W} = Input) -> Input#input{w = perturb(MP, rand:uniform(), W)};
		(bias, Bias) -> perturb(MP, rand:uniform(), Bias)
	end.

perturb(MP, Rand, W) when Rand < MP -> sat((rand:uniform() - 0.5) * ?DELTA_MULTIPLIER + W, -?SAT_LIMIT, ?SAT_LIMIT);
perturb(_, _, W)                    -> W.

% ......................................................................................................................
sat(Val, Min, _) when Val < Min -> Min;
sat(Val, _, Max) when Val > Max -> Max;
sat(Val, _, _)                  -> Val.

	
	
	
