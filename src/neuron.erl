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
-define(NEW_ID, {make_ref(), neuron}).
-record(neuron, {
    id = ?NEW_ID :: id(),
    activation   :: activation:func(),
    aggregation  :: aggregation:func(),
    initializer  :: initializer:func(),
    bias         :: link:weight()
}).  
-type neuron()     :: #neuron{}.
-type properties() :: #{activation  := activation:func(),
                        aggregation := aggregation:func(),
                        initializer := initializer:func(),
                        bias        := link:weight()
}.

-record(input,  {
    w = 0.0 :: link:weight(), % Input weight 
    x = 0.0 :: float()        % Signal value (Xi)
}).
-define(X(Input), element(#input.x, Input)).
-define(W(Input), element(#input.w, Input)).

-record(output, {
    e = 0.0 :: float() % Output error (Ei)
}).
-define(E(Output), Output#output.e).

-define(PID(Id), nn_pool:pid(get(nn_pool), Id)).
-define(ID(Pid), nn_pool:id(get(nn_pool), Pid)).

-record(state, {
    forward_wait   :: [id()],
    backward_wait  :: [id()]
}).

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
        initializer = maps:get(initializer, Properties,   glorot)
    }.

%%--------------------------------------------------------------------
%% @doc Clones a neuron from its id.
%% @end
%%--------------------------------------------------------------------
-spec clone(Neuron :: neuron()) -> neuron().
clone(Neuron) -> Neuron#neuron{id = ?NEW_ID}.

%%--------------------------------------------------------------------
%% @doc Returns the neuron id.
%% @end
%%-------------------------------------------------------------------
-spec id(Neuron :: neuron()) -> id().
id(Neuron) -> Neuron#neuron.id.

%%--------------------------------------------------------------------
%% @doc Returns the neuron activation.
%% @end
%%-------------------------------------------------------------------
-spec activation(Neuron :: neuron()) -> activation:func().
activation(Neuron) -> Neuron#neuron.activation.

-spec activation(Neuron :: neuron(), activation:func()) -> neuron().
activation(Neuron, Func) -> Neuron#neuron{activation = Func}.

%%--------------------------------------------------------------------
%% @doc Returns the neuron aggregation.
%% @end
%%-------------------------------------------------------------------
-spec aggregation(Neuron :: neuron()) -> aggregation:func().
aggregation(Neuron) -> Neuron#neuron.aggregation.

%%--------------------------------------------------------------------
%% @doc Returns the neuron initializer.
%% @end
%%-------------------------------------------------------------------
-spec initializer(Neuron :: neuron()) -> initializer:func().
initializer(Neuron) -> Neuron#neuron.initializer.

%%--------------------------------------------------------------------
%% @doc Returns the neuron bias. If a second parameter is included, it
%% will return a neuron witht the modified bias value.
%% @end
%%-------------------------------------------------------------------
-spec bias(Neuron :: neuron()) -> link:weight().
bias(Neuron) -> Neuron#neuron.bias.

-spec bias(Neuron :: neuron(), Bias :: float()) -> neuron().
bias(Neuron, Bias) -> Neuron#neuron{bias = Bias}.

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
    Connections :: network:connections(),
    NN_Pool :: nn_pool:pool().
go(Pid, Connections, NN_Pool) ->
    Pid ! {continue_init, Connections, NN_Pool}.


%%%===================================================================
%%% Initialization functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc The neuron initialization.  
%% @end
%%--------------------------------------------------------------------
init(Id, Supervisor) ->
    proc_lib:init_ack(Supervisor, {ok, self()}), % Supervisor synch
    {InConn, OutConn, NNPool} = wait_for_go(),   % Cortex synch
    process_flag(trap_exit, true), % Catch supervisor exits
    % Load specific keys in dictionary
    put(nn_pool, NNPool),
    % Load neuron related dictionary
    [Neuron] = mnesia:dirty_read(neuron, Id),
    put(         id,          Neuron#neuron.id),
    put(       bias,        Neuron#neuron.bias),
    put( activation,  Neuron#neuron.activation),    
    put(aggregation, Neuron#neuron.aggregation),    
    put(initializer, Neuron#neuron.initializer), 
    % Load inputs and outputs in dictionary
    put(outputs, #{}),
    add_outputs(maps:to_list(OutConn)),
    put( inputs, #{}),
    add_inputs( maps:to_list( InConn)),
    % First propagation for rcc links
    calculate_bias(0.0),
    forward_prop(),   
    backward_prop(),  
    % Neuron start log and initialization of waits
    ?LOG_NEURON_STARTED,
    loop(#state{ 
        forward_wait  = maps:keys(get( inputs)), 
        backward_wait = maps:keys(get(outputs))
    }).

wait_for_go() -> 
    receive 
        {continue_init, #{in:=InConn, out:=OutConn}, NN_Pool} -> 
            {InConn, OutConn, NN_Pool}
    end.


%%%===================================================================
%%% neuron callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc The neuron loop. Receives the next messages and triggers the
%% specific action. 
%%
%%--------------------------------------------------------------------
% If all forward signals have been received, state change to forward
loop(#state{forward_wait  = []} = State) -> 
    ?LOG_WAITING_NEURONS(State),
    add_inputs(inbox_forwards()),
    forward_prop(),
    loop(State#state{forward_wait = maps:keys(get(inputs))});
% If all backward signals have been received, state change to backward
loop(#state{backward_wait = []} = State) ->
    ?LOG_WAITING_NEURONS(State),
    backward_prop(),
    loop(State#state{backward_wait = maps:keys(get(outputs))});
% If there are signals in any waiting buffer collects the next signal
loop(State) -> 
    ?LOG_WAITING_NEURONS(State),
    receive_next(State).

%%--------------------------------------------------------------------
%% @doc The neuron loop. Receives the next messages and triggers the
%% specific action. 
%%
%%--------------------------------------------------------------------
receive_next(State) ->
    #state{forward_wait=[Nf|Rf] , backward_wait=[Nb|Rb]} = State,
    receive 
        {Nf,forward, X}   -> 
             forward_in(Nf, X, State#state{ forward_wait=Rf});
        {Nb,backward,E}   -> 
            backward_in(Nb, E, State#state{backward_wait=Rb});
        {'EXIT',_,Reason} -> 
            terminate(Reason, State)
    after   
        ?STDIDLE_TIMEOUT  -> 
            idle(State)
    end.
% Receives a forward signal so updates its value in inputs ----------
forward_in(Pid, Xi, State)  ->
    #{Pid := I} = Inputs = get(inputs),
    put(inputs, Inputs#{Pid := I#input{x = Xi}}),
    loop(State).

% Receives a backward error so updates its value in outputs ---------
backward_in(Pid, Ei, State)  ->
    #{Pid := O} = Outputs = get(outputs),
    put(outputs, Outputs#{Pid := O#output{e = Ei}}),
    loop(State).

% Neuron has been idle for a long time ------------------------------
idle(State) -> 
    ?LOG_NEURON_IDLE(State),
    receive_next(State).

%%--------------------------------------------------------------------
%% @doc Terminates the neuron and saves in edb the new weights.
%%
%%--------------------------------------------------------------------
terminate(Reason, _State) ->
    % TODO: When saving the new state, those links with weights ~= 0, must be deleted (both neurons)
    % TODO: If the neuron has not at least 1 input or 1 output, it must be deleted (and bias forwarded)
    {atomic, _} = mnesia:transaction( 
        fun() -> write_links(), write_neuron() end
    ),
    ?LOG_NEURON_TERMINATING,
    exit(Reason).

%%--------------------------------------------------------------------
%% @end  
%%--------------------------------------------------------------------


%%%===================================================================
%%% Message functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Forwards the signal to the connected neurons. 
%%
%%--------------------------------------------------------------------
forward_prop() -> 
    Tensor = calculate_tensor(get(inputs)),
    Soma   = calculate_soma(Tensor),
    Signal = calculate_signal(Soma),
    Sent = [forward(P,O,Signal) || {P,O}<-maps:to_list(get(outputs))],
    ?LOG_FORWARD_PROPAGATION(Sent).

forward(Pid, _Output, Signal) ->
    Pid ! {self(), forward, Signal}, 
    {Pid, Signal}.

%%--------------------------------------------------------------------
%% @doc Backwards the errror to the connected inputs.
%% 
%%--------------------------------------------------------------------
backward_prop() ->
    Error = calculate_error(get(outputs)),
    Beta  = calculate_beta(Error),
    Sent = [backward(P,I,Beta) || {P,I}<-maps:to_list(get(inputs))],
    ?LOG_BACKWARD_PROPAGATION(Sent),
    calculate_bias(Beta),
    calculate_weights(Beta).

backward(Pid, Input, Beta) ->
    BP_Error = ?W(Input) * Beta,
    Pid ! {self(), backward, BP_Error},
    {Pid, BP_Error}.


%%%===================================================================
%%% Calculation functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Calculates the tensor.
%% @end
%%--------------------------------------------------------------------
calculate_tensor(Inputs) ->
    Tensors = [{Pid,?W(I),?X(I)} || {Pid,I} <- maps:to_list(Inputs)],
    put(prev_signals, Tensors), % Save for weights calculation
    Tensors.

%%--------------------------------------------------------------------
%% @doc Calculates the value of soma.
%% @end
%%--------------------------------------------------------------------
calculate_soma(Tensors) -> 
    WiXi = [{Wi,Xi} || {_,Wi,Xi} <- Tensors],
    Soma = aggregation:func(get(aggregation), WiXi, get(bias)),
    put(soma, Soma),  % Save for the beta calculation
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
    lists:sum([O#output.e || {_,O} <- maps:to_list(Outputs)]).

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
    Inputs = calculate_weights(get(prev_signals), get(inputs), Beta),
    put(inputs, Inputs).

calculate_weights([{Pid,_,Xi}|Tx], Inputs, B) -> 
    I = maps:get(Pid, Inputs),
    calculate_weights(Tx, Inputs#{Pid:=updt_input(I,Xi,B)}, B);
calculate_weights([], Inputs, _) -> 
    Inputs.

updt_input(I, Xi, Beta) -> 
    I#input{w=updt_weight(?W(I),Xi,Beta)}.

%%--------------------------------------------------------------------
%% @doc Calculates the new bias from back propagation of the error.
%% @end
%%--------------------------------------------------------------------
calculate_bias(Beta) -> 
    put(bias, updt_weight(get(bias), 1.0, Beta)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Adds the Pids and their signals to the inputs.
%% @end
%%--------------------------------------------------------------------
add_inputs(      []) -> ok;
add_inputs(Requests) -> 
    {[], Inputs} = add_inputs(Requests, get(id), [], get(inputs)),
    put(inputs, Inputs).

add_inputs([{Id1,link}|Rx], Id2, LAcc, Ix1) -> % Using Id
    Pid = ?PID(Id1),
    {[Wi|Wx], Ix2} = add_inputs(Rx, Id2, [{Id1,Id2}|LAcc], 
                                Ix1#{Pid=>#input{}}),
    {Wx, Ix2#{Pid:=#input{w=Wi}}};
add_inputs([{Pid,Xi}|Rx], Id2, LAcc, Ix1)   -> % Using Pid
    Id1 = ?ID(Pid), % Note: Id is a tuple {Ref,neuron}
    {[Wi|Wx], Ix2} = add_inputs(Rx, Id2, [{Id1,Id2}|LAcc], 
                                Ix1#{Pid=>#input{}}),
    {Wx, Ix2#{Pid:=#input{w=Wi, x=Xi}}};
add_inputs([], _, LAcc, Inputs) -> 
    put(inputs, Inputs), % Needed for correct winit fan_in/out
    {atomic, Weights} = mnesia:transaction(
        fun() -> [link:read({From,To}) || {From,To} <- LAcc] end
    ),
    {[updt_weight(W,0.0,0.0) || W <- Weights], Inputs}.

%%--------------------------------------------------------------------
%% @doc Adds the Pids and their signals to the inputs.
%% @end
%%--------------------------------------------------------------------
add_outputs(      []) -> ok;
add_outputs(Requests) -> 
    Outputs = add_outputs(Requests, get(outputs)),
    put(outputs, Outputs).

add_outputs([{Id,link}|Rx], Outputs) ->
    Pid = ?PID(Id),
    add_outputs(Rx, Outputs#{Pid => #output{}});
add_outputs([], Outputs) -> 
    Outputs.

%%--------------------------------------------------------------------
%% @doc Updates the weight value. If it is uninitialized, a new value
%% is generated.
%% @end
%%--------------------------------------------------------------------
updt_weight(Wi,Xi,B) when is_number(Wi) -> Wi+dw(Xi,B);
updt_weight(undefined, _, _)            -> winit().

%%--------------------------------------------------------------------
%% @doc Initialises a weight.
%% @end
%%--------------------------------------------------------------------
winit() -> 
    initializer:value(get(initializer), #{
        fan_in  => maps:size(get( inputs)),
        fan_out => maps:size(get(outputs))
    }).

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

% -------------------------------------------------------------------
inbox_forwards() -> 
    receive {Pid,forward,Xi} -> [{Pid,Xi}|inbox_forwards()]
    after 0                  -> []
    end.

% -------------------------------------------------------------------
inbox_backwards() -> 
    receive {Pid,backward,Xi} -> [{Pid,Xi}|inbox_backwards()]
    after 0                   -> []
    end.

% -------------------------------------------------------------------
write_links() -> 
    Id     = get(id),
    NNPool = get(nn_pool),
    write_links(maps:to_list(get(inputs)), Id, NNPool).

write_links([{Pid,I}|Ix], To, NNPool) -> 
    ok = link:write({nn_pool:id(NNPool,Pid), To}, ?W(I)),
    write_links(Ix, To, NNPool);
write_links([], _, _) -> 
    ok.

% -------------------------------------------------------------------
write_neuron() -> 
    ok = mnesia:write(#neuron{
        id          = get(id),
        activation  = get(activation),
        aggregation = get(aggregation),
        initializer = get(initializer),
        bias        = get(bias)
    }).


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

