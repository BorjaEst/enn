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

-record(input,  {
    id      :: neuron:id(),
    w = 0.0 :: link:weight(), % Input weight 
    s = 0.0 :: float()        % Signal value (Xi)
}).
-define(S(Input), element(#input.s, Input)).
-define(W(Input), element(#input.w, Input)).

-record(output, {
    id      :: neuron:id(),
    e = 0.0 :: float() % Output error (Ei)
}).
-define(E(Output), Output#output.e).

-define(PID(Id), nn_pool:pid(get(nn_pool)), Id).
-define(ID(Pid), nn_pool:id(get(nn_pool)), Pid).

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
%% @doc Clones a neuron.
%% @end
%%--------------------------------------------------------------------
-spec clone(Neuron :: neuron()) -> neuron().
clone(Neuron) -> Neuron#neuron{id = {make_ref(), neuron}}.

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
-spec go(Pid, Outputs, NN_Pool) -> NonRelevant :: term() when 
    Pid :: pid(),
    Outputs :: [neuron:id()],
    NN_Pool :: nn_pool:pool().
go(Pid, Outputs, NN_Pool) ->
    Pid ! {continue_init, Outputs, NN_Pool}.


%%%===================================================================
%%% Initialization functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc The neuron initialization.  
%% @end
%%--------------------------------------------------------------------
init(Id, Supervisor) ->
    proc_lib:init_ack(Supervisor, {ok, self()}), % Supervisor synch
    {OutRequests, NNPool} = wait_for_go(),       % Cortex synch
    process_flag(trap_exit, true), % Catch supervisor exits
    Neuron = edb:read(Id),
    put(nn_pool, NNPool),
    put(outputs,    #{}), 
    put( inputs,    #{}), 
    put(         id,          Neuron#neuron.id),
    put(       bias,        Neuron#neuron.bias),
    put( activation,  Neuron#neuron.activation),    
    put(aggregation, Neuron#neuron.aggregation),    
    put(initializer, Neuron#neuron.initializer), 
    add_outputs(OutRequests),
    loop(#state{ % wait starts at [] to trigger rcc links
        forward_wait  = [], 
        backward_wait = []
    }).

wait_for_go() -> 
    receive 
        {continue_init, Connections, NN_Pool} -> 
            {Connections, NN_Pool}
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
    put(inputs, Inputs#{Pid := I#input{s = Xi}}),
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
    save_links(),
    edb:write(#neuron{
        id          = get(id),
        activation  = get(activation),
        aggregation = get(aggregation),
        initializer = get(initializer),
        bias        = get(bias)
    }),
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
    Inputs = get(inputs), Outputs = get(outputs),
    Tensor = calculate_tensor(Inputs),
    Soma   = calculate_soma(Tensor),
    Signal = calculate_signal(Soma),
    Sent = [forward(P,O,Signal) || {P,O} <- maps:to_list(Outputs)],
    ?LOG_FORWARD_PROPAGATION(Sent).

forward(Pid, _Output, Signal) ->
    Pid ! {self(), forward, Signal}, 
    {Pid, Signal}.

%%--------------------------------------------------------------------
%% @doc Backwards the errror to the connected inputs.
%% 
%%--------------------------------------------------------------------
backward_prop() ->
    Inputs = get(inputs), Outputs = get(outputs),
    Error = calculate_error(Outputs),
    Beta  = calculate_beta(Error),
    Sent = [backward(P,I,Beta) || {P,I} <- maps:to_list(Inputs)],
    ?LOG_BACKWARD_PROPAGATION(Sent),
    calculate_bias(Beta),
    calculate_weights(Inputs, Beta).

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
    Tensors = [{Pid,?W(I),?S(I)} || {Pid,I} <- maps:to_list(Inputs)],
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
    activation:beta(get(activationn), Error, get(soma)).

%%--------------------------------------------------------------------
%% @doc Calculates the new weights from back propagation of the error.
%% @end
%%--------------------------------------------------------------------
calculate_weights(Inputs, Beta) -> 
    calculate_weights(get(prev_signals), Inputs, Beta).

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
add_inputs(Requests) -> 
    {[], Inputs} = add_inputs(Requests, get(id), []),
    put(inputs, Inputs).

add_inputs([{Pid,Xi}|Rx], Id2, LAcc) -> 
    Id1 = ?ID(Pid),
    {[Wi|Wx], Inputs} = add_inputs(Rx, Id2, [{Id1,Id2}|LAcc]),
    {Wx, Inputs#{Pid => #input{id=Id1, w=Wi, s=Xi}}};
add_inputs([], _, LAcc) -> 
    {link:read(LAcc), get(inputs)}.

%%--------------------------------------------------------------------
%% @doc Adds the Pids and their signals to the inputs.
%% @end
%%--------------------------------------------------------------------
add_outputs(Ids) -> 
    Outputs = add_outputs(Ids, get(outputs)),
    put(outputs, Outputs).

add_outputs([Id|Rx], Outputs) ->
    Pid = ?PID(Id),
    add_outputs([Id|Rx], Outputs#{Pid => #output{id=Id}});
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
        fan_in  => length(get( inputs)),
        fan_out => length(get(outputs))
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
save_links() ->
    Id     = get(id),
    Inputs = get(inputs),
    {Links,Weights} = links(maps:values(Inputs), Id, [],[]),
    link:write(Links, Weights).

links([I|Ix], To, LAcc, WAcc) -> 
    From = I#input.id,
    links(Ix, To, [{From,To}|LAcc], [I#input.w|WAcc]);
links([], _, LAcc, WAcc) -> 
    {LAcc, WAcc}.


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



