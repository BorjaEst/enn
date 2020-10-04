%%%-------------------------------------------------------------------
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
-export_type([id/0]).

-type id() :: nn_node:neuron().
-record(input,  {
    w       :: link:weight(), % Input weight 
    x = 0.0 :: float()        % Signal value (Xi)
}).
-define(X(Input), element(#input.x, Input)).
-define(W(Input), element(#input.w, Input)).

-record(output, {
    type    :: seq | rcc,
    e = 0.0 :: float() % Output error (Ei)
}).
-define(E(Output), Output#output.e).

-define(PID(Id), nn_pool:pid(get(nn_pool), Id)).
-define(ID(Pid), nn_pool:id(get(nn_pool), Pid)).

-record(state, {
    forward_wait   :: [id()],
    backward_wait  :: [id()]
}).
-define(DATA, element(#state.data, State)).

% Learning parameters (to be moved to a module optimizer in a future)
-define( INTEGRATION_FACTOR, 0.40).  
-define(PROPORTIONAL_FACTOR, 0.02).  
-define(  DERIVATIVE_FACTOR, 0.01).  
-define(      INITIAL_ERROR, 0.00).

% Configuration parameters
-define(INIT_SYNCH_TIMEOUT,   10).
-define(STDIDLE_TIMEOUT,    1000).
-define(SAT(Val), max(   -1.0e6, 
                  min(    1.0e6,
                  Val))).

%%%===================================================================
%%% API
%%%===================================================================

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
%% @end
%%--------------------------------------------------------------------
-spec cortex_synch(Pid, NN_Pool) -> NonRelevant when 
    Pid         :: pid(),
    NN_Pool     :: nn_pool:pool(),
    NonRelevant :: term().
cortex_synch(Pid, NN_Pool) ->
    Pid ! {continue_init, NN_Pool}.

cortex_synch() -> 
    receive {continue_init, NN_Pool} -> NN_Pool end.


%%%===================================================================
%%% Initialization functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc The neuron initialization.  
%% 
%%--------------------------------------------------------------------
init(Id, Supervisor) ->
    proc_lib:init_ack(Supervisor, {ok, self()}), % Supervisor synch
    process_flag(trap_exit, true), % Catch supervisor exits
    {atomic, Info} = mnesia:transaction(
        fun() -> #{inw  => [{L,nnet:rlink(L)} || L <- nnet:in(Id)],
                   seq  => nnet:out_seq(Id),
                   rcc  => nnet:out_rcc(Id),
                   data => nnet:rnode(Id)}
        end),
    NNPool = cortex_synch(),
    ok = init_dictionary(Id, NNPool, Info),
    ok = init_inputs(Info),
    ok = init_outputs(Info),
    ok = forward_synch(),
    ok = backward_synch(),
    ok = init_weights(),
    ?LOG_NEURON_STARTED, % Start log and initialization of weights
    loop(#state{ 
        forward_wait  = maps:keys(get(inputs)),
        backward_wait = [P || {P,O} <- maps:to_list(get(outputs)),
                               O#output.type == seq]
    }).
%%--------------------------------------------------------------------
%% @doc Initialisation of neuron internal dictionary.  
%%
%%--------------------------------------------------------------------
init_dictionary(Id, NNPool, #{data:=Data}) -> 
    put(    id,      Id), % Used for logs and final write update
    put(   data,   Data), % Used to calculate activation signals
    put(nn_pool, NNPool), % Used to convert Pid<->Id 
    put( inputs,    #{}), % Used for forward/bakward propagation
    put(outputs,    #{}), % Used for forward/bakward propagation
    ok.
%%--------------------------------------------------------------------
%% @doc Initialisation of neuron state.  
%%
%%--------------------------------------------------------------------
init_inputs(#{inw:=InW}) -> 
    set_inputs(InW),
    ok.

init_outputs(#{seq:=Seq, rcc:=Rcc}) -> 
    set_outputs(Rcc, rcc), 
    set_outputs(Seq, seq),
    ok.
%%--------------------------------------------------------------------
%% @doc Synchronisation functions at init.  
%%
%%--------------------------------------------------------------------
forward_synch() -> 
    receive {From, forward, synch} -> 
        [Pid ! {self(), forward, synch} || Pid <- maps:keys(get(outputs))],
        self() ! {From, forward, synch}
    after ?INIT_SYNCH_TIMEOUT -> 
        [Pid ! {self(), forward, discn} || Pid <- maps:keys(get(outputs))],
        ?LOG_NEURON_EXIT("Neuron forward disconnected"),
        exit(normal)
    end,
    synchronise(forward, maps:keys(get(inputs))).

backward_synch() -> 
    receive {From, backward, synch} -> 
        [Pid ! {self(), backward, synch} || Pid <- maps:keys(get(inputs))],
        self() ! {From, backward, synch}
    after 2*?INIT_SYNCH_TIMEOUT ->
        [Pid ! {self(), backward, discn} || Pid <- maps:keys(get(inputs))],
        ?LOG_NEURON_EXIT("Neuron backward disconnected"),
        exit(normal)
    end,
    synchronise(backward, maps:keys(get(outputs))).

synchronise(Session, [From|Fx]) ->
    receive 
        {From, Session,  synch} -> do_nothing; 
        {From, forward,  discn} -> remove_input(From); 
        {From, backward, discn} -> remove_output(From)
    end,
    synchronise(Session, Fx);
synchronise(_Session, []) ->
    ok.
%%--------------------------------------------------------------------
%% @doc Initial weights calculation.  
%%
%%--------------------------------------------------------------------
init_weights() -> 
    calculate_tensor(get(inputs)),
    recalculate_weights(0.0),
    Sent = [forward(P,O,0.0) || {P,O} <- maps:to_list(get(outputs)),
                                 O#output.type == rcc],
    ?LOG_FORWARD_PROPAGATION(Sent), % Rcc propagation
    ok.
%%--------------------------------------------------------------------
%% @end
%%--------------------------------------------------------------------

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
    ?LOG_NEURON_EXIT(Reason),
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
    #{aggregation:=Aggr, activation:=Actv, bias:=Bias} = get(data),
    Tensor = calculate_tensor(get(inputs)),
    Soma   = calculate_soma(Aggr, Tensor, Bias),
    Signal = calculate_signal(Actv, Soma),
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
    #{activation:=Actv} = get(data),
    Error = calculate_error(get(outputs)),
    Beta  = calculate_beta(Actv, Error),
    Sent = [backward(P,I,Beta) || {P,I}<-maps:to_list(get(inputs))],
    ?LOG_BACKWARD_PROPAGATION(Sent),
    recalculate_weights(Beta).

backward(Pid, Input, Beta) ->
    BP_Error = ?W(Input) * Beta,
    Pid ! {self(), backward, BP_Error},
    {Pid, BP_Error}.

recalculate_weights(Beta) -> 
    #{initializer:=Init, bias:=Bias} = Data = get(data),
    AdaptW = fun(X,Ix) -> adapt_weights(X, Ix, Beta, Init) end,
    put(data, Data#{bias:=adapt_Bias(Bias, Beta)}),
    put(inputs, lists:foldl(AdaptW, get(inputs), get(prev_tensor))).


%%%===================================================================
%%% Calculation functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Calculates the tensor.
%% @end
%%--------------------------------------------------------------------
calculate_tensor(Inputs) ->
    Tensors = [{Pid,?W(I),?X(I)} || {Pid,I} <- maps:to_list(Inputs)],
    put(prev_tensor, Tensors), % Save for weights calculation
    Tensors.

%%--------------------------------------------------------------------
%% @doc Calculates the value of soma.
%% @end
%%--------------------------------------------------------------------
calculate_soma(Aggregation, Tensors, Bias) -> 
    WiXi = [{Wi,Xi} || {_,Wi,Xi} <- Tensors],
    Soma = aggregation:func(Aggregation, WiXi, Bias),
    put(prev_soma, Soma),  % Save for the beta calculation
    Soma.

%%--------------------------------------------------------------------
%% @doc Calculates the value of soma.
%% @end
%%--------------------------------------------------------------------
calculate_signal(Activation, Soma) -> 
    ?SAT(activation:func(Activation, Soma)).

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
calculate_beta(Activation, Error) -> 
    ?SAT(activation:beta(Activation, Error, get(prev_soma))).

%%--------------------------------------------------------------------
%% @doc Calculates the new weights from back propagation of the error.
%% @end
%%--------------------------------------------------------------------
adapt_weights({Pid,_,Xi}, Inputs, Beta, Init) -> 
    I = maps:get(Pid, Inputs),
    Inputs#{Pid:=updt_input(Pid,I,Xi,Beta,Init)}.

updt_input(Pid, I, Xi, Beta, Init) -> 
    I#input{w=updt_weight(Pid,?W(I),Xi,Beta,Init)}.

adapt_Bias(Bias, Beta) ->
    case is_number(Bias) of 
        true  -> ?SAT(Bias + ?INTEGRATION_FACTOR * Beta);
        false -> ?SAT( 0.0 + ?INTEGRATION_FACTOR * Beta)
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Adds the Pids and their signals to the inputs.
%% @end
%%--------------------------------------------------------------------
set_inputs(LinkWs) ->
    set_inputs(LinkWs, get(inputs)).

set_inputs([{{From,_},W}|LinkWs], Ix) ->
    Pid = ?PID(From),
    set_inputs(LinkWs, Ix#{Pid=>#input{w=W}});
set_inputs([], Inputs) ->
    put(inputs, Inputs).

%%--------------------------------------------------------------------
%% @doc Adds the Pids and their signals to the inputs.
%% @end
%%--------------------------------------------------------------------
set_outputs(Links, Type) ->
    set_outputs(Links, get(outputs), Type).

set_outputs([{_,To}|Links], Ox, Type) ->
    Pid = ?PID(To),
    set_outputs(Links, Ox#{Pid=>#output{type=Type}}, Type);
set_outputs([], Outputs, _Type) ->
    put(outputs, Outputs).

%%--------------------------------------------------------------------
%% @doc Remove the input identified by pid.
%% @end
%%--------------------------------------------------------------------
remove_input(Pid) ->
    Inputs = get(inputs),
    put(inputs, maps:remove(Pid, Inputs)).

%%--------------------------------------------------------------------
%% @doc Remove the output identified by pid..
%% @end
%%--------------------------------------------------------------------
remove_output(Pid) ->
    Outputs = get(outputs),
    put(outputs, maps:remove(Pid, Outputs)).

%%--------------------------------------------------------------------
%% @doc Updates the weight value. If it is uninitialized, a new value
%% is generated.
%% @end
%%--------------------------------------------------------------------
updt_weight(Pid,Wi,Xi,B,_) when is_number(Wi) -> ?SAT(Wi+dw(Pid,Xi,B));
updt_weight(Pid,not_init,Xi,_,Init)           -> winit(Pid,Xi,Init).

%%--------------------------------------------------------------------
%% @doc Initialises a weight.
%% @end
%%--------------------------------------------------------------------
winit(Pid, Xi, Init) -> 
    put(Pid, Xi),
    initializer:value(Init, #{
        fan_in  => maps:size(get( inputs)),
        fan_out => maps:size(get(outputs))
    }).

% % Weight variation calculation with momentum .........................
% m <- ?MOMENTUM_FACTOR*m - ?LEARNING_FACTOR*get(beta) * Xi,
% w <- W - m,
% dw(Xi, Mi) ->
%     NewMi = ?MOMENTUM_FACTOR * Mi - ?LEARNING_FACTOR * get(beta) * Xi,
%     NewWi = Wi - NewMi.


% Weight variation calculation .......................................
dw(Pid, Xi, Beta) -> 
    dpw(Pid, Xi, Beta) + ddw(Pid, Xi, Beta).

% Proportional weigth calculation ...................................
dpw(_Pid, Xi, Beta) -> 
    Dpw = ?PROPORTIONAL_FACTOR * Beta * Xi,
    Dpw.

% Derivative weigth calculation .....................................
ddw(Pid, Xi, Beta) -> 
    Ddw = ?DERIVATIVE_FACTOR * Beta * (Xi - get(Pid)),
    put(Pid, Xi),
    Ddw.

% -------------------------------------------------------------------
write_links() -> 
    maps:fold(fun write_link/3, {get(id),get(nn_pool)}, get(inputs)).

write_link(Pid, Input, {Id,NNPool}) -> 
    Link = {nn_pool:id(NNPool, Pid), Id},
    ok = nnet:wlink(Link, Input#input.w),
    {Id, NNPool}.

% -------------------------------------------------------------------
write_neuron() -> 
    ok = nnet:wnode(get(id), get(data)).


%%====================================================================
%% Eunit white box tests
%%====================================================================

% --------------------------------------------------------------------
% TESTS DESCRIPTIONS -------------------------------------------------

% Tests the neuron receive messages ---------------------------------
neurons_communication_test_() ->
    [{"A neuron can receive multiple forward/backward messages",
      {foreach, local, fun start_neuron/0, fun stop_neuron/1,
       [
            fun forward_echo/1,
            fun backward_echo/1,
            fun backandforward_echo/1
       ]
     }}
    ].


% --------------------------------------------------------------------
% SPECIFIC SETUP FUNCTIONS -------------------------------------------

% Creates a network and returns the input/output nodes --------------
-define(MOK_INPUTS_IDS,  [join( "in", N) || N <- lists:seq(1,20)]).
-define(MOK_OUTPUTS_IDS, [join("out", N) || N <- lists:seq(1,20)]).
start_neuron() -> 
    spawn_link(?MODULE, mok_init, [
        maps:from_list([{X,#input{w=not_init}} || X <- ?MOK_INPUTS_IDS ]),
        maps:from_list([{X,#output{type=seq}}  || X <- ?MOK_OUTPUTS_IDS]),
        #{activation  => direct, aggregation => dot_prod,
          initializer => glorot, bias        => not_init}]).

% Destroys the neuron -----------------------------------------------
stop_neuron(Neuron) ->
    unlink(Neuron),
    exit(Neuron, shutdown).


% --------------------------------------------------------------------
% TEST INSTANTIATORS -------------------------------------------------

% Sends a forward and receives a forward ----------------------------
forward_echo(Pid) -> 
    {inparallel, 
        [?_assert(send_forward(Id, Pid)) || Id <- ?MOK_INPUTS_IDS] ++ 
        [?_assert(wait_forward(Id, Pid)) || Id <- ?MOK_OUTPUTS_IDS]
    }. 

% Sends a backward and receives a backward --------------------------
backward_echo(Pid) -> 
    {inparallel, 
        [?_assert(wait_backward(Id, Pid)) || Id <- ?MOK_INPUTS_IDS] ++
        [?_assert(send_backward(Id, Pid)) || Id <- ?MOK_OUTPUTS_IDS] 
    }. 

% Sends a forward/backward and receives a forward/backward ----------
backandforward_echo(Pid) -> 
    {inparallel, 
        [?_assert(send_forward(Id, Pid)) || Id <- ?MOK_INPUTS_IDS] ++ 
        [?_assert(wait_forward(Id, Pid)) || Id <- ?MOK_OUTPUTS_IDS] ++
        [?_assert(wait_backward(Id, Pid)) || Id <- ?MOK_INPUTS_IDS] ++
        [?_assert(send_backward(Id, Pid)) || Id <- ?MOK_OUTPUTS_IDS] 
    }. 


% --------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS ------------------------------------------

% Joins a string and an integer (only for tests!!!) -----------------
join(String, Integer) -> 
    list_to_atom(String ++ erlang:integer_to_list(Integer)).

% Moking initialization for neuron ----------------------------------
mok_init(Inputs, Outputs, Data) -> 
    % process_flag(trap_exit, true), Do not catch exits
    put(    id, make_ref()), 
    put(  data,       Data), 
    put(inputs,     Inputs),
    put(outputs,   Outputs),
    ok = init_weights(),
    loop(#state{ 
        forward_wait  = maps:keys(get(inputs)),
        backward_wait = [P || {P,O} <- maps:to_list(get(outputs)),
                               O#output.type == seq]
    }). 

% Sends a forward message -------------------------------------------
send_forward(Input_id, Pid) -> 
    timer:sleep(1),  % Give some time to the other process to register
    Pid ! {Input_id, forward, 0.0},
    true. 

% Expects a forward message -----------------------------------------
wait_forward(Output_id, Pid) -> 
    register(Output_id, self()),
    receive {Pid, forward, _} -> true
    after    10               -> error(no_message)
    end. 

% Sends a backward messages -----------------------------------------
send_backward(Output_id, Pid) -> 
    timer:sleep(1),  % Give some time to the other process to register
    Pid ! {Output_id, backward, 0.0},
    true. 

% Expects a backward message ----------------------------------------
wait_backward(Input_id, Pid) -> 
    register(Input_id, self()),
    receive {Pid, backward, _} -> true
    after    10                -> error(no_message)
    end. 

