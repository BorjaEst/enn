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
-define(LEARNING_FACTOR, 0.01).  
-define(MOMENTUM_FACTOR, 0.00).
-define(INITIAL_ERROR,   0.00).

% Configuration parameters
-define(STDIDLE_TIMEOUT, 1000).

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
%%
%% TODO: Improve info passing by having all on the NN_Pool ets.
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
%% @end
%%--------------------------------------------------------------------
init(Id, Supervisor) ->
    proc_lib:init_ack(Supervisor, {ok, self()}), % Supervisor synch
    process_flag(trap_exit, true), % Catch supervisor exits
    NNPool = cortex_synch(),
    {atomic, #{in:=InW, seq:=Seq, rcc:=Rcc, data:=Data}} = 
    mnesia:transaction(
        fun() -> #{in => [{L,nnet:rlink(L)} || L<-nnet:in(Id)],
                   seq  => nnet:out_seq(Id),
                   rcc  => nnet:out_rcc(Id),
                   data => nnet:rnode(Id)}
        end),
    put(    id,      Id), % Used for logs and final write update
    put(   data,   Data), % Used to calculate activation signals
    put(nn_pool, NNPool), % Used to convert Pid<->Id 
    put( inputs,    #{}), % Used for forward/bakward propagation
    put(outputs,    #{}), % Used for forward/bakward propagation
    ?LOG_NEURON_STARTED, % Start log and initialization of waits
    set_outputs(Rcc), 
    Sent = [forward(P,O,0.0) || {P,O}<-maps:to_list(get(outputs))],
    ?LOG_FORWARD_PROPAGATION(Sent), % Rcc propagation
    loop(#state{ 
        backward_wait = set_outputs(Seq),
        forward_wait  = set_inputs(InW)
    }).


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
    put(data, Data#{bias:=adapt_Bias(Bias, Beta, Init)}),
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
    activation:func(Activation, Soma).

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
    activation:beta(Activation, Error, get(prev_soma)).

%%--------------------------------------------------------------------
%% @doc Calculates the new weights from back propagation of the error.
%% @end
%%--------------------------------------------------------------------
adapt_weights({Pid,_,Xi}, Inputs, Beta, Init) -> 
    I = maps:get(Pid, Inputs),
    Inputs#{Pid:=updt_input(I,Xi,Beta,Init)}.

updt_input(I, Xi, Beta, Init) -> 
    I#input{w=updt_weight(?W(I),Xi,Beta,Init)}.

adapt_Bias(Bias, Beta, Init) ->
    updt_weight(Bias, 1.0, Beta, Init).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Adds the Pids and their signals to the inputs.
%% @end
%%--------------------------------------------------------------------
set_inputs(LinkWs) ->
    set_inputs(LinkWs, get(inputs), []).

set_inputs([{{From,_},W}|LinkWs], Ix, Pids) ->
    Pid = ?PID(From),
    set_inputs(LinkWs, Ix#{Pid=>#input{w=W}}, [Pid|Pids]);
set_inputs([], Inputs, Pids) ->
    put(inputs, Inputs),
    calculate_tensor(Inputs),
    recalculate_weights(0.0),
    Pids.

%%--------------------------------------------------------------------
%% @doc Adds the Pids and their signals to the inputs.
%% @end
%%--------------------------------------------------------------------
set_outputs(Links) ->
    set_outputs(Links, get(outputs), []).

set_outputs([{_,To}|Links], Ox, Pids) ->
    Pid = ?PID(To),
    set_outputs(Links, Ox#{Pid=>#output{}}, [Pid|Pids]);
set_outputs([], Outputs, Pids) ->
    put(outputs, Outputs),
    Pids.

%%--------------------------------------------------------------------
%% @doc Updates the weight value. If it is uninitialized, a new value
%% is generated.
%% @end
%%--------------------------------------------------------------------
updt_weight(Wi,Xi,B,_) when is_number(Wi) -> Wi+dw(Xi,B);
updt_weight(not_init,_,_,Init)            -> winit(Init).

%%--------------------------------------------------------------------
%% @doc Initialises a weight.
%% @end
%%--------------------------------------------------------------------
winit(Init) -> 
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

% TODO: Add saturation and error deviation not proportinal to X 
%       (Saturation and protection)

% Weight variation calculation .......................................
dw(Xi, Beta) -> ?LEARNING_FACTOR * Beta * Xi.

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

% --------------------------------------------------------------------
% SPECIFIC SETUP FUNCTIONS -------------------------------------------

% --------------------------------------------------------------------
% ACTUAL TESTS -------------------------------------------------------

% --------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS ------------------------------------------

