%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc The neuron is a signal processing element. It accepts
%%% signals, accumulates them into an ordered vector, then processes
%%% this input vector to produce an output, and finally passes the
%%% output to other elements it is connected to.
%%%
%%% The neuron waits until it receives all the input signals, 
%%% processes those signals, and then passes the output forward.
%%%
%%%
%%% TODO: Optimise network when shutdown (Part can be on the shutdown, 
%%%       another in the clonation¿?)
%%% TODO: fsm? with forward or other according to the best performance
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(neuron).
-compile([export_all, nowarn_export_all]). %% TODO: To delete after build

-include_lib("math_constants.hrl").
-include_lib("enn_logger.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
%%-export([]).
-export_type([id/0, neuron/0, properties/0]).

-type id() :: {{Coordinate :: float(), reference()}, neuron}.
-record(neuron, {
    id = {make_ref(), neuron} :: id(),
    activation  :: activation:func(),
    aggregation :: aggregation:func(),
    initializer :: initializer:func(),
    bias        :: link:weight()
}).  
-type neuron()     :: #neuron{}.
-type properties() :: #{activation  := activation:func(),
                        aggregation := aggregation:func(),
                        initializer := initializer:func(),
                        bias        := link:weight()
}.

-define(PID(Id), nn_pool:pid(Id, get(tid))).
-record(input,  {
    id      :: id(),        % Neuron id (input)
    s = 0.0 :: float(),     % Signal value (Xi)
    m       :: float(),     % Momentum
    w       :: link:weight() % Weight
}).
-type input() :: #input{}.

-record(output, {
    id      :: id(),        % Neuron id (output)
    e = 0.0 :: float()      % Output error (Ei)
}).
-type output() :: #output{}.

-record(state, {
    neuron  :: neuron(),
    inputs  :: [input()],
    outputs :: [output()],
    forward_wait  :: [pid()],
    backward_wait :: [pid()]
}).
-define( NEURON(State),  State#state.neuron).
-define( INPUTS(State),  State#state.inputs).
-define(OUTPUTS(State), State#state.outputs).

% Learning parameters (to be moved to a module optimizer in a future)
-define(LEARNING_FACTOR, 0.01).  
-define(MOMENTUM_FACTOR, 0.00).
-define(INITIAL_ERROR,   0.00).

% Configuration parameters
-define(STDIDLE_TIMEOUT, 1000).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new neuron. Some properties can be defined.
%% @end
%%--------------------------------------------------------------------
-spec new() -> neuron().
new() -> new(#{}).

-spec new(Properties :: properties()) -> neuron().
new(Properties) ->  
    #neuron{
        activation  = maps:get( activation, Properties,   direct),
        aggregation = maps:get(aggregation, Properties, dot_prod),
        initializer = maps:get(initializer, Properties,   glorot),
        bias        = maps:get(bias, Properties, uninitialized)
    }.

%%--------------------------------------------------------------------
%% @doc Returns the neuron id.
%% @end
%%-------------------------------------------------------------------
-spec id(Neuron :: neuron()) -> id().
id(Neuron) -> Neuron#neuron.id.

%%-------------------------------------------------------------------
%% @doc Record fields from neuron.  
%% @end
%%-------------------------------------------------------------------
-spec record_fields() -> ListOfFields :: [atom()].
record_fields() -> record_info(fields, neuron).

%%--------------------------------------------------------------------
%% @doc Neuron id start function for supervisor. 
%% @end
%%--------------------------------------------------------------------
-spec start_link(Id :: id()) -> 
    gen_statem:start_ret().
start_link(Id) ->
    Supervisor = self(),
    proc_lib:start_link(?MODULE, init, [Id, Supervisor]).

%%--------------------------------------------------------------------
%% @doc Gives the go to the neuron run after the network is mounted. 
%%
%% TODO: Improve info passing by having all on the NN_Pool ets.
%% @end
%%--------------------------------------------------------------------
-spec go(Pid, Connections, NN_Pool) -> NonRelevant :: term() when 
    Pid :: pid(),
    Connections :: nn_node:connections(),
    NN_Pool     :: nn_pool:pool().
go(Pid, Connections, NN_Pool) ->
    Pid ! {continue_init, Connections, NN_Pool}.


%%%===================================================================
%%% neuron callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc The neuron initialization.  
%% @end
%%--------------------------------------------------------------------
init(Id, Supervisor) ->
    proc_lib:init_ack(Supervisor, {ok, self()}), % Supervisor synch
    {Connections, NN_Pool} = wait_for_go(),      % Cortex synch
    propagate_recurrent(Connections, NN_Pool), 
    process_flag(trap_exit, true), % Catch supervisor exits
    ?LOG_NEURON_STARTED,
    loop(internal, #state{
        neuron  = edb:read(Id),
        inputs  = load_inputs(Id, Connections, NN_Pool),
        outputs = load_outputs(Connections, NN_Pool),
        forward_wait  = [Pid || Pid <- maps:keys(get(inputs ))],
        backward_wait = [Pid || Pid <- maps:keys(get(outputs))]
    }).

%%--------------------------------------------------------------------
%% @doc The neuron loop. It collects all the signals until fowrward or
%% backward is completed. Then starts a forwad or back propagation.  
%%
%%--------------------------------------------------------------------
% Receives a forward signal so updates its value in inputs
loop({Pid,forward,S},  #state{forward_wait  = [Pid|Nx]} = State) ->
    ?LOG_FORWARD_MESSAGE_RECEIVED(Pid, S),
    #{Pid := Input} = Inputs = get(inputs),
    put(inputs, Inputs#{Pid := Input#input{s = S}}),
    loop(internal, State#state{forward_wait  = Nx});
% Receives a backward signal so updates its error in outputs
loop({Pid,backward,E}, #state{backward_wait = [Pid|Nx]} = State) ->
    ?LOG_BACKWARD_MESSAGE_RECEIVED(Pid, E),
    #{Pid := Output} = Outputs = get(outputs),
    put(outputs, Outputs#{Pid := Output#output{e = E}}),
    loop(internal, State#state{backward_wait = Nx});

% If all forward signals have been received, state change to forward
loop(internal, #state{forward_wait  = []} = State) -> 
    ?LOG_WAITING_NEURONS(State),
    forward_prop(internal, State);
% If all backward signals have been received, state change to backward
loop(internal, #state{backward_wait = []} = State) ->
    ?LOG_WAITING_NEURONS(State),
    backward_prop(internal, State);
% If there are signals in any waiting buffer collects the next signal
loop(internal,                              State) -> 
    receive_next(internal, State).
%%--------------------------------------------------------------------
%% @doc Forwards the signal to the connected neurons. 
%%
%%--------------------------------------------------------------------
forward_prop(internal, State) -> 
    Tensor = calculate_tensor(get(inputs)),
    Soma   = calculate_soma(Tensor),
    Signal = calculate_signal(Soma),
    Sent = [forward(P,O,Signal)|| {P,O}<-maps:to_list(get(outputs))],
    ?LOG_FORWARD_PROPAGATION(Sent),
    loop(internal, State#state{
        forward_wait = [Pid || Pid <- maps:keys(get(inputs))]
    }).
%%--------------------------------------------------------------------
%% @doc Backwards the errror to the connected inputs.
%% 
%%--------------------------------------------------------------------
backward_prop(internal, State) ->
    Error = calculate_error(get(outputs)),
    Beta  = calculate_beta(Error),
    calculate_weights(Beta),
    calculate_bias(Beta),
    Sent = [backward(P,I,Beta)|| {P,I}<-maps:to_list(get(inputs))],
    ?LOG_BACKWARD_PROPAGATION(Sent),
    loop(internal, State#state{
        backward_wait = [Pid || Pid <- maps:keys(get(outputs))]
    }).
%%--------------------------------------------------------------------
%% @doc Receives the next messages and sends it to loop. It only 
%% passes the messages that would be matched, the rest are stored on 
%% the inbox (as they will be used later). 
%%
%%--------------------------------------------------------------------
receive_next(internal, State) ->
    ?LOG_WAITING_NEURONS(State),
    #state{forward_wait=[Nf|_] , backward_wait = [Nb|_]} = State,
    receive {N,forward, _}=Msg when N==Nf -> loop(Msg, State);
            {N,backward,_}=Msg when N==Nb -> loop(Msg, State);
            {'EXIT',_,Reason}             -> terminate(Reason, State)
    after ?STDIDLE_TIMEOUT                ->
        ?LOG_NEURON_IDLE(State),
        receive_next(internal, State)
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
    Id     = get(id),
    Neuron = edb:read(Id),
    edb:write(elements:edit(Neuron,
        #{
            outputs_ids => [O#output.id             
                            || {_, O} <- maps:to_list(get(outputs))],
            inputs_idps => [{I#input.id, I#input.w} 
                            || {_, I} <- maps:to_list(get(inputs ))],
            bias        => get(bias)
        })),
    ?LOG_NEURON_TERMINATING,
    exit(Reason).


%%%===================================================================
%%% Initialization functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Waits the 'continue_init' signal with the required information
%% to start (connections and the nn_pool).  
%% @end
%%--------------------------------------------------------------------
wait_for_go() -> 
    receive 
        {continue_init, Connections, NN_Pool} -> 
            {Connections, NN_Pool}
    end.

%%--------------------------------------------------------------------
%% @doc Propagates a signal to the recurrent connections to avoid 
%% deadlock.  
%% @end
%%--------------------------------------------------------------------
propagate_recurrent(Connections, NN_Pool) -> 
    Empty_Output = #output{},    % Do not gen a new output each iter
    SentO = [forward(nn_pool:pid(NN_Pool, Id), Empty_Output, 0.0) 
        || Id <- nn_node:out_neighbours(Connections, recurrent)],
    ?LOG_FORWARD_PROPAGATION_RECURRENT_OUTPUTS(SentO),
    Empty_Input = #input{w=0.0}, % Do not gen a new input each iter
    SentI = [backward(nn_pool:pid(NN_Pool, Id), Empty_Input, 0.0)
        || Id <- nn_node:in_neighbours(Connections, recurrent)],
    ?LOG_BACKWARD_PROPAGATION_RECURRENT_INPUTS(SentI).

%%--------------------------------------------------------------------
%% @doc Neuron outputs initialization.  
%% @end
%%--------------------------------------------------------------------
init_outputs(Neuron) -> 
    Coordinade = elements:coordinade(Neuron),
    Outputs    = elements:outputs_ids(Neuron),
    maps:from_list(init_outputs(Coordinade, Outputs)).

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
init_inputs(Neuron, NN_id) -> 
    Coordinade = elements:coordinade(Neuron),
    Inputs     = elements:inputs_idps(Neuron),
    maps:from_list(init_inputs(Coordinade, Inputs, NN_id)).

init_inputs(Coordinade, [{Id,uninitialized}|IdWix], NN_id)        -> 
    Wi = calculate_winit(Coordinade, NN_id),
    init_inputs(Coordinade, [{Id,Wi}|IdWix], NN_id);
init_inputs(Coordinade, [{Id,Wi}|IdWix], NN_id) when is_float(Wi) -> 
    Input = #input{
        id = Id, 
        w  = Wi, 
        r  = not elements:is_dir_input(Coordinade, Id)
    },
    [{?PID(Id), Input} | init_inputs(Coordinade, IdWix, NN_id)];
init_inputs(_, [], _) -> 
    [].

%%--------------------------------------------------------------------
%% @doc Neuron bias initialization. If a the bias is not initialized
%% a synchronised request is sent to the cortex to receive a value.  
%% @end
%%--------------------------------------------------------------------
init_bias(Neuron, NN_id) ->
    Coordinade = elements:coordinade(Neuron),
    case elements:bias(Neuron) of 
        uninitialized          -> calculate_winit(Coordinade, NN_id);
        Val when is_float(Val) -> Val
    end.


%%%===================================================================
%%% Calculation functions
%%%==================================================================

%%--------------------------------------------------------------------
%% @doc Calculates the tensor.
%% @end
%%--------------------------------------------------------------------
calculate_tensor(Inputs) ->
    Tensors = [{Pid, {Input#input.w, Input#input.s}} 
                   || {Pid, Input} <- maps:to_list(Inputs)],
    put(tensor, Tensors), % Saves tensor for weights calculation
    [WiXix || {_,WiXix} <- Tensors].

%%--------------------------------------------------------------------
%% @doc Calculates the value of soma.
%% @end
%%--------------------------------------------------------------------
calculate_soma(WiXix) -> 
    Soma = aggregation:func(get(aggregation), WiXix, get(bias)),
    put(soma, Soma),   % Saves the soma value for the beta calculation
    Soma.

%%--------------------------------------------------------------------
%% @doc Calculates the value of soma.
%% @end
%%--------------------------------------------------------------------
calculate_signal(Soma) -> 
    activation:func(get(activation), Soma).

%%--------------------------------------------------------------------
%% @doc Calculates the error from propagation.
%% @end
%%--------------------------------------------------------------------
calculate_error(Outputs) ->
    lists:sum([O#output.e || {_, O} <- maps:to_list(Outputs)]).

%%--------------------------------------------------------------------
%% @doc Calculates the value of beta for back propagation).
%% @end
%%--------------------------------------------------------------------
calculate_beta(Error) -> 
    activation:beta(get(activation), Error, get(soma)).

%%--------------------------------------------------------------------
%% @doc Calculates the new weights from back propagation of the error.
%% @end
%%--------------------------------------------------------------------
calculate_weights(Beta) -> 
    put(inputs, calculate_weights(get(tensor), get(inputs), Beta)).

calculate_weights([{Pid, {Wi,Xi}} | Tx], Inputs, B) -> 
    I = maps:get(Pid, Inputs),
    calculate_weights(Tx, Inputs#{Pid:=I#input{w=Wi+dw(Xi,B)}}, B);
calculate_weights([], Inputs, _) -> 
    Inputs.

%%--------------------------------------------------------------------
%% @doc Calculates the new bias from back propagation of the error.
%% @end
%%--------------------------------------------------------------------
calculate_bias(Beta) -> 
    put(bias, get(bias) + dw(1.0, Beta)).

%%--------------------------------------------------------------------
%% @doc Calculates the new bias from back propagation of the error.
%% @end
%%--------------------------------------------------------------------
calculate_winit(Coordinade, NN_id) -> 
    {Fan_In, Fan_Out} = cortex:fan_inout(NN_id, Coordinade),
    initializer:value(get(initializer), #{
        fan_in  => Fan_In,
        fan_out => Fan_Out
    }).


%%%===================================================================
%%% Internal functions
%%%===================================================================

% % Weight variation calculation with momentum .........................
% m <- ?MOMENTUM_FACTOR*m - ?LEARNING_FACTOR*get(beta) * Xi,
% w <- W - m,
% dw(Xi, Mi) ->
%     NewMi = ?MOMENTUM_FACTOR * Mi - ?LEARNING_FACTOR * get(beta) * Xi,
%     NewWi = Wi - NewMi.

% TODO: Add saturation and error deviation not proportinal to X 
%       (Saturation and protection)

% Weight variation calculation .......................................
dw(Xi, Beta) -> ?LEARNING_FACTOR * Beta * Xi.


%%%===================================================================
%%% Message functions
%%%===================================================================

% -------------------------------------------------------------------
forward(Pid, _Output, Signal) ->
    Pid ! {self(), forward, Signal}, 
    {Pid, Signal}.

% -------------------------------------------------------------------
backward(Pid, Input, Beta) ->
    BP_Error = Input#input.w * Beta,
    Pid ! {self(), backward, BP_Error},
    {Pid, BP_Error}.


%%====================================================================
%% Eunit white box tests
%%====================================================================

% --------------------------------------------------------------------
% TESTS DESCRIPTIONS -------------------------------------------------

% --------------------------------------------------------------------
% SPECIFIC SETUP FUNCTIONS -------------------------------------------

% --------------------------------------------------------------------
% ACTUAL TESTS -------------------------------------------------------

% --------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS ------------------------------------------



