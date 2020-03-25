%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc The neuron is a signal processing element. It accepts
%%% signals, accumulates them into an ordered vector, then processes
%%% this input vector to produce an output, and finally passes the
%%% output to other elements it is connected to.
%%%
%%% The neuron waits until it receives all the input signals, 
%%% processes those signals, and then passes the output forward.
%%% @end
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
-export_type([id/0, property/0, properties/0]).

-type id()         :: {{Coordinate :: float(), reference()}, neuron}.
-type property()   :: id | activation | aggregation | initializer.
-type properties() :: #{
    OptionalProperty :: property() => Value :: term()
}.

-define(PID(Id), cortex:nn_id2pid(Id, get(tid))).
-record(input,  {
    id      :: id(),        % Neuron id (input)
    r       :: boolean(),   % Is_recurrent
    w       :: float(),     % Weight
    m       :: float(),     % Momentum
    s = 0.0 :: float()      % Signal value (Xi)
}).
-record(output, {
    id      :: id(),        % Neuron id (output)
    r       :: boolean(),   % Is_recurrent
    e = 0.0 :: float()      % Output error (Ei)
}).

-record(state, {
    forward_wait  :: [pid()],
    backward_wait :: [pid()]
}).

-define(LEARNING_FACTOR, 0.01).  
-define(MOMENTUM_FACTOR, 0.00).
-define(INITIAL_ERROR, 0.0).


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
%% @doc Creates a new neuron from, stores it on the database and 
%% returns its id.
%% @end
%%--------------------------------------------------------------------
-spec new(Coordinade, Properties) -> id() when
    Coordinade :: float(),
    Properties :: #{Property => Value :: term()},
    Property   :: elements:neuron_property().
new(Coordinade, Properties) ->
    Neuron = elements:neuron(Coordinade, Properties),
    edb:write(Neuron),
    elements:id(Neuron).

%%--------------------------------------------------------------------
%% @doc Neuron id start function for supervisor. 
%% @end
%%--------------------------------------------------------------------
-spec start_link(Id :: id(), Cortex :: pid()) -> 
    gen_statem:start_ret().
start_link(Id, Cortex) ->
    Supervisor = self(),
    proc_lib:start_link(?MODULE, init, [Id, Cortex, Supervisor]).


%%%===================================================================
%%% neuron callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc The neuron initialization.  
%% @end
%%--------------------------------------------------------------------
init(Id, Cortex, Supervisor) ->
    Neuron = edb:read(Id),
    [_|_] = elements:inputs_idps(Neuron),
    [_|_] = elements:outputs_ids(Neuron),
    process_flag(trap_exit, true),           % Catch supervisor exits
    proc_lib:init_ack(Supervisor, {ok, self()}),   % Supervisor synch
    receive {continue_init, TId} -> put(tid, TId) end, % Resume synch
    put(cortex,      Cortex),
    put(id,          elements:id(Neuron)),
    put(activation,  elements:activation(Neuron)),
    put(aggregation, elements:aggregation(Neuron)),
    put(initializer, elements:initializer(Neuron)),
    put(bias,        elements:bias(Neuron)),
    put(outputs,     init_outputs(Neuron)),
    put(inputs,      init_inputs(Neuron)),
    put(error,       ?INITIAL_ERROR),
    calculate_tensor(),
    calculate_soma(), 
    calculate_signal(),
    calculate_beta(),
    propagate_recurrent(), 
    loop(internal, #state{
        forward_wait  = [Pid || Pid <- maps:keys(get(inputs ))],
        backward_wait = [Pid || Pid <- maps:keys(get(outputs))]
    }).

%%--------------------------------------------------------------------
%% @doc The neuron loop. It collects all the signals until fowrward or
%% backward is completed. Then starts a forwad or back propagation.  
%%
%%--------------------------------------------------------------------
% Receives a forward signal so updates its value in inputs
loop({N,forward,S},  #state{forward_wait  = [N|Nx]} = State) ->
    #{N := Input} = Inputs = get(inputs),
    put(inputs, Inputs#{N := Input#input{s = S}}),
    loop(internal, State#state{forward_wait  = Nx});
% Receives a backward signal so updates its error in outputs
loop({N,backward,E}, #state{backward_wait = [N|Nx]} = State) -> 
    #{N := Output} = Outputs = get(outputs),
    put(outputs, Outputs#{N := Output#output{e = E}}),
    loop(internal, State#state{backward_wait = Nx});

% If all forward signals have been received, state change to forward
loop(internal, #state{forward_wait = []} = State) -> 
    forward_prop(internal, State);
% If all backward signals have been received, state change to backward
loop(internal, #state{backward_wait = []} = State) ->
    backward_prop(internal, State);
% If there are signals in any waiting buffer collects the next signal
loop(internal,                              State) -> 
    receive_next(internal, State);

% If receives an 'exit' signal, state change to terminate
loop({'EXIT', _Pid, Reason}, State) -> 
    terminate(Reason, State).
%%--------------------------------------------------------------------
%% @doc Forwards the signal to the connected neurons. 
%%
%%--------------------------------------------------------------------
forward_prop(internal, State) -> 
    calculate_tensor(),
    calculate_soma(),
    calculate_signal(),
    [forward(Output) || Output <- maps:to_list(get(outputs))],
    loop(internal, State#state{
        forward_wait = [Pid || Pid <- maps:keys(get(inputs))]
    }).
%%--------------------------------------------------------------------
%% @doc Backwards the errror to the connected inputs.
%% 
%%--------------------------------------------------------------------
backward_prop(internal, State) ->
    calculate_beta(),
    calculate_weights(),
    calculate_bias(),
    [backward(Input) || Input <- maps:to_list(get(inputs))],
   loop(internal, State#state{
        backward_wait = [Pid || Pid <- maps:keys(get(outputs))]
    }).
%%--------------------------------------------------------------------
%% @doc Receives the next messages and sends it to loop. 
%%
%%--------------------------------------------------------------------
receive_next(internal, State) ->
    receive Message        -> loop(Message, State)
    after ?STDIDLE_TIMEOUT ->
        ?LOG_WARNING("neuron ~p stuck", [get(id)]),
        terminate(unknown_message, State)
    end.
%%--------------------------------------------------------------------
%% @end  
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc Terminates the neuron and saves in edb the new weiights.
%% @end
%%--------------------------------------------------------------------
terminate(Reason, _State) ->
    % TODO: When saving the new state, those links with weights ~= 0, must be deleted (both neurons)
    % TODO: If the neuron has not at least 1 input or 1 output, it must be deleted (and bias forwarded)
    Neuron = edb:read(get(id)),
    edb:write(elements:edit(Neuron,
        #{
            outputs_ids => [O#output.id             
                            || {_, O} <- maps:to_list(get(outputs))],
            inputs_idps => [{I#input.id, I#input.w} 
                            || {_, I} <- maps:to_list(get(inputs ))],
            bias        => get(bias)
        })),
    exit(Reason).


%%%===================================================================
%%% Initialization functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Neuron outputs initialization.  
%% @end
%%--------------------------------------------------------------------
init_outputs(Neuron) -> 
    Coordinade = elements:coordinade(Neuron),
    Outputs    = elements:outputs_ids(Neuron),
    init_outputs(Coordinade, Outputs).

init_outputs(Coordinade, [Id|Idx]) ->
    Output = #output{
        id = Id, 
        r  = not elements:is_dir_output(Coordinade, Id)
    },
    [{?PID(Id), Output} | init_outputs(Coordinade, Idx)];
init_outputs(_, []) -> 
    [].

%%--------------------------------------------------------------------
%% @doc Neuron inputs initialization. If a weith is not initialized
%% a synchronised request is sent to the cortex to receive a value.  
%% @end
%%--------------------------------------------------------------------
init_inputs(Neuron) -> 
    Coordinade = elements:coordinade(Neuron),
    Inputs     = elements:inputs_idps(Neuron),
    init_inputs(Coordinade, Inputs).

init_inputs(Coordinade, [{Id,undefined}|IdWx]) -> 
    W = cortex:initializer(get(cortex), get(initializer)),
    init_inputs(Coordinade, [{Id,W}|IdWx]);
init_inputs(Coordinade, [{Id,W}|IdWx]) -> 
    Input = #input{
        id = Id, 
        w  = W, 
        r  = not elements:is_dir_input(Coordinade, Id)
    },
    [{?PID(Id), Input} | init_inputs(Coordinade, IdWx)];
init_inputs(_, []) -> 
    [].

%%--------------------------------------------------------------------
%% @doc Propagates a signal to the recurrent connections to avoid 
%% deadlock.  
%% @end
%%--------------------------------------------------------------------
propagate_recurrent() -> 
    OutputsList = maps:to_list(get(outputs)),
    InputsList  = maps:to_list(get(outputs)),
    [forward(Output) || {_,#input{r = true}} = Output <- OutputsList],
    [backward(Input) || {_,#input{r = true}} = Input  <- InputsList ].


%%%===================================================================
%%% Calculation functions
%%%==================================================================

%%--------------------------------------------------------------------
%% @doc Calculates the tensor.
%% @end
%%--------------------------------------------------------------------
calculate_tensor() ->
    TensorsList = [{Pid, {Input#input.w, Input#input.s}} 
                      || {Pid, Input} <- maps:to_list(get(inputs))],
    put(tensor, TensorsList).

%%--------------------------------------------------------------------
%% @doc Calculates the value of soma.
%% @end
%%--------------------------------------------------------------------
calculate_soma() -> 
    put(soma, aggregation:apply(get(aggregation), 
                                get(tensor     ), 
                                get(bias       )
    )).

%%--------------------------------------------------------------------
%% @doc Calculates the value of soma.
%% @end
%%--------------------------------------------------------------------
calculate_signal() -> 
    put(signal, activation:apply(get(activation), 
                                 get(soma      )
    )).

%%--------------------------------------------------------------------
%% @doc Calculates the value of beta for back propagation).
%% @end
%%--------------------------------------------------------------------
calculate_beta() -> 
    put(beta, activation:beta(get(activation), 
                              get(error     ), 
                              get(soma      ) 
    )).

%%--------------------------------------------------------------------
%% @doc Calculates the new weights from back propagation of the error.
%% @end
%%--------------------------------------------------------------------
calculate_weights() -> 
    put(inputs, calculate_weights(get(tensor), get(inputs))).

calculate_weights([{Pid, {Wi,Xi}} | Tx], Inputs) -> 
    I = maps:get(Pid, Inputs),
    calculate_weights(Tx, Inputs#{Pid := I#input{w = Wi + dw(Xi)}});
calculate_weights([], Inputs) -> 
    Inputs.

%%--------------------------------------------------------------------
%% @doc Calculates the new bias from back propagation of the error.
%% @end
%%--------------------------------------------------------------------
calculate_bias() -> 
    put(bias, get(bias) + dw(1.0)).


%%%===================================================================
%%% Internal functions
%%%===================================================================

% % Weight variation calculation with momentum .........................
% m <- ?MOMENTUM_FACTOR*m - ?LEARNING_FACTOR*get(beta) * Xi,
% w <- W - m,
% dw(Xi, Mi) ->
%     NewMi = ?MOMENTUM_FACTOR * Mi - ?LEARNING_FACTOR * get(beta) * Xi,
%     NewWi = Wi - NewMi.

% Weight variation calculation .......................................
dw(Xi) -> ?LEARNING_FACTOR * get(beta) * Xi.


%%%===================================================================
%%% Message functions
%%%===================================================================

% ....................................................................
forward({Pid, _Output}) ->
    Pid ! {self(), forward, get(signal)}.

% ....................................................................
backward({Pid, Input}) ->
    Pid ! {self(), backward, Input#input.w * get(beta)}.


    
    