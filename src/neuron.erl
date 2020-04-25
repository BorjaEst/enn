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
    link    :: id(),   % Input link 
    s = 0.0 :: float() % Signal value (Xi)
}).
-type input() :: #input{}.
-define(S(Input), element(#input.s,    Input)).
-define(L(Input), element(#input.link, Input)).
-define(W(Input), link:weight(?L(Input))).

-record(output, {
    e = 0.0 :: float() % Output error (Ei)
}).
-type output() :: #output{}.
-define(E(Output), Output#output.e).

-record(state, { %Order is relevant: outputs -> inputs -> neuron
    outputs :: [output()],
    inputs  :: [input()],
    neuron  :: neuron(),
    forward_wait  :: [pid()],
    backward_wait :: [pid()]
}).
-define(     NEURON(State), element( #state.neuron, State)).
-define(     INPUTS(State), element( #state.inputs, State)).
-define(    OUTPUTS(State), element(#state.outputs, State)).
-define( ACTIVATION(State),  activation(?NEURON(State))).
-define(AGGREGATION(State), aggregation(?NEURON(State))).
-define(       BIAS(State),        bias(?NEURON(State))).

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
%% @doc Returns the neuron bias.
%% @end
%%-------------------------------------------------------------------
-spec bias(Neuron :: neuron()) -> link:weight().
bias(Neuron) -> Neuron#neuron.bias.

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
    Neuron = edb:read(Id),
    put(initializer, Neuron#neuron.initializer), % Save for winit()
    load_wait(#state{
        outputs = load_outputs(Connections, NN_Pool),
        inputs  = load_inputs(Id, Connections, NN_Pool),
        neuron  = load_neuron(Neuron)
    }).

load_wait(State) -> 
    ?LOG_NEURON_STARTED(State),
    loop(internal, State#state{
        forward_wait  = [Pid || Pid <- maps:keys( ?INPUTS(State))],
        backward_wait = [Pid || Pid <- maps:keys(?OUTPUTS(State))]
    }).

%%--------------------------------------------------------------------
%% @doc The neuron loop. It collects all the signals until fowrward or
%% backward is completed. Then starts a forwad or back propagation.  
%%
%%--------------------------------------------------------------------
% Receives a forward signal so updates its value in inputs
loop({Pid,forward,S},  #state{forward_wait  = [Pid|Nx]} = State) ->
    ?LOG_FORWARD_MESSAGE_RECEIVED(Pid, S),
    #{Pid := I} = Inputs = State#state.inputs,
    loop(internal, State#state{
        inputs       = Inputs#{Pid := I#input{s = S}},
        forward_wait = Nx
    });
% Receives a backward signal so updates its error in outputs
loop({Pid,backward,E}, #state{backward_wait = [Pid|Nx]} = State) ->
    ?LOG_BACKWARD_MESSAGE_RECEIVED(Pid, E),
    #{Pid := O} = Outputs = State#state.outputs,
    loop(internal, State#state{
        outputs       = Outputs#{Pid := O#output{e = E}},
        backward_wait = Nx
    });
% If all forward signals have been received, state change to forward
loop(internal, #state{forward_wait  = []} = State) -> 
    ?LOG_WAITING_NEURONS(State),
    forward_prop(internal, State);
% If all backward signals have been received, state change to backward
loop(internal, #state{backward_wait = []} = State) ->
    ?LOG_WAITING_NEURONS(State),
    backward_prop(internal, State);
% If there are signals in any waiting buffer collects the next signal
loop(internal, State) -> 
    receive_next(internal, State).
%%--------------------------------------------------------------------
%% @doc Forwards the signal to the connected neurons. 
%%
%%--------------------------------------------------------------------
forward_prop(internal, State) -> 
    #state{inputs=Inputs, outputs=Outputs} = State,
    Tensor = calculate_tensor(Inputs),
    Soma   = calculate_soma(?NEURON(State), Tensor),
    Signal = calculate_signal(?NEURON(State), Soma),
    Sent = [forward(P,O,Signal)|| {P,O} <- maps:to_list(Outputs)],
    ?LOG_FORWARD_PROPAGATION(Sent),
    loop(internal, State#state{
        forward_wait = [Pid || Pid <- maps:keys(Inputs)]
    }).
%%--------------------------------------------------------------------
%% @doc Backwards the errror to the connected inputs.
%% 
%%--------------------------------------------------------------------
backward_prop(internal, State) ->
    #state{inputs=Inputs, outputs=Outputs} = State,
    Error = calculate_error(Outputs),
    Beta  = calculate_beta(?NEURON(State), Error),
    Sent = [backward(P,I,Beta)|| {P,I} <- maps:to_list(Inputs)],
    ?LOG_BACKWARD_PROPAGATION(Sent),
    loop(internal, State#state{
        neuron = calculate_bias(?NEURON(State), Beta),
        inputs = calculate_weights(Inputs, Beta),
        backward_wait = [Pid || Pid <- maps:keys(Outputs)]
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
terminate(Reason, State) ->
    % TODO: When saving the new state, those links with weights ~= 0, must be deleted (both neurons)
    % TODO: If the neuron has not at least 1 input or 1 output, it must be deleted (and bias forwarded)
    edb:write([?NEURON(State)|[L||#input{link=L}<-?INPUTS(State)]]),
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
    Empty_Output = #output{}, % Do not gen a new each iteration
    Empty_Input  = #input{link=link:new(n1,n2,#{weight=>0.0})}, 
    SentO = [forward(nn_pool:pid(NN_Pool, Id), Empty_Output, 0.0) 
        || Id <- nn_node:out_neighbours(Connections, recurrent)],
    ?LOG_FORWARD_PROPAGATION_RECURRENT_OUTPUTS(SentO),
    SentI = [backward(nn_pool:pid(NN_Pool, Id), Empty_Input, 0.0)
        || Id <- nn_node:in_neighbours(Connections, recurrent)],
    ?LOG_BACKWARD_PROPAGATION_RECURRENT_INPUTS(SentI).

%%--------------------------------------------------------------------
%% @doc Here goes the internal dictionary savings which requires 
%% global module access for simplification.
%% @end
%%--------------------------------------------------------------------
load_neuron(#neuron{} = Neuron) -> 
    put(id, id(Neuron)),
    calculate_bias(Neuron, 0.0).

%%--------------------------------------------------------------------
%% @doc Neuron outputs loading.  
%% @end
%%--------------------------------------------------------------------
load_outputs(Connections, NN_Pool) -> 
    Out = nn_node:out_neighbours(Connections),
    put(fan_out, length(Out)), % Save for winit()
    maps:from_list([{nn_pool:pid(NN_Pool,X),#output{}} || X<-Out]).

%%--------------------------------------------------------------------
%% @doc Neuron inputs loading. If a link is not retreived from edb, a
%% new link is created.  
%% @end
%%--------------------------------------------------------------------
load_inputs(Id, Connections, NN_Pool) -> 
    In = [link:id(X,Id) || X <- nn_node:in_neighbours(Connections)],
    put(fan_in, length(In)), % Save for winit()
    Inputs = maps:from_list(load_links(edb:read(In),In,NN_Pool)),
    put(prev_signals, [{P,0.0,0.0} || P <- maps:keys(Inputs)]),
    calculate_weights(Inputs, 0.0). % Weights initialisation

load_links([undefined|Lx], [Id|Ids], NN_Pool) -> 
    L = link:new(link:from(Id), link:to(Id)),
    load_links([L|Lx], [Id|Ids], NN_Pool);
load_links([L|Lx], [Id|Ids], NN_Pool) -> 
    Pid = nn_pool:pid(NN_Pool, link:from(Id)),
    [{Pid,#input{link=L}} | load_links(Lx, Ids, NN_Pool)];
load_links([], [], _) -> 
    [].


%%%===================================================================
%%% Calculation functions
%%%==================================================================

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
calculate_soma(Neuron, Tensors) -> 
    WiXi = [{Wi,Xi} || {_,Wi,Xi} <- Tensors],
    Soma = aggregation:func(aggregation(Neuron),WiXi,?BIAS(Neuron)),
    put(soma, Soma),  % Save for the beta calculation
    Soma.

%%--------------------------------------------------------------------
%% @doc Calculates the value of soma.
%% @end
%%--------------------------------------------------------------------
calculate_signal(Neuron, Soma) -> 
    activation:func(activation(Neuron), Soma).

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
calculate_beta(Neuron, Error) -> 
    activation:beta(activation(Neuron), Error, get(soma)).

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
    I#input{link=link:edit(?L(I), updt_weight(?W(I),Xi,Beta))}.

%%--------------------------------------------------------------------
%% @doc Calculates the new bias from back propagation of the error.
%% @end
%%--------------------------------------------------------------------
calculate_bias(Neuron, Beta) -> 
    Neuron#neuron{bias = updt_weight(bias(Neuron), 1.0, Beta)}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Updates the weight value. If it is uninitialized, a new value
%% is generated.
%% @end
%%--------------------------------------------------------------------
updt_weight(Wi,Xi,B) when is_number(Wi) -> Wi+dw(Xi,B);
updt_weight(uninitialized, _, _)        -> winit().

%%--------------------------------------------------------------------
%% @doc Initialises a weight.
%% @end
%%--------------------------------------------------------------------
winit() -> 
    initializer:value(get(initializer), #{
        fan_in  => get( fan_in),
        fan_out => get(fan_out)
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


%%%===================================================================
%%% Message functions
%%%===================================================================

% -------------------------------------------------------------------
forward(Pid, _Output, Signal) ->
    Pid ! {self(), forward, Signal}, 
    {Pid, Signal}.

% -------------------------------------------------------------------
backward(Pid, Input, Beta) ->
    BP_Error = ?W(Input) * Beta,
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



