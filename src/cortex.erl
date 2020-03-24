%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc The cortex is a NN synchronizing element. It needs to know 
%%% the Pid of every neural network element, so that it will know when
%%% all the outputs have received their control inputs, and that it’s
%%% time for the inputs to again gather and fanout input data to the
%%% neurons in the input layer. 
%%% @end
%%%-------------------------------------------------------------------
-module(cortex).
-compile([export_all, nowarn_export_all]). %% TODO: To delete after build

-include_lib("math_constants.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_statem).

%% API
%%-export([start_link/0]).
-export_type([id/0, property/0, properties/0]).

%% gen_statem callbacks
-export([init/1, format_status/2, handle_event/4, terminate/3, 
         code_change/4, callback_mode/0]).
-export([inactive/3, on_feedforward/3, on_backpropagation/3]).

-type id() :: {Ref :: reference(), cortex}.
-type property()   :: id | layers | outputs_ids | inputs_idps.
-type properties() :: #{
    OptionalProperty :: property() => Value :: term()
}.

-record(output, {pid :: pid(), s :: float(), error    :: float()}).
-record(input,  {pid :: pid(), s :: float(), acc = [] :: [float()]}).
-record(state, {
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
%% @doc Creates a new cortex from the compiled layers, stores it on 
%% the database and returns its cortex id.
%% @end
%%--------------------------------------------------------------------
-spec new(CompiledLayers, Properties) -> id() when
    CompiledLayers :: #{integer() => layer:compiled()},
    Properties     :: properties().
new(CompiledLayers, Properties) ->
    Cortex = elements:cortex(CompiledLayers, Properties),
    edb:write(Cortex), % Saved before mutations to avoid overwriting
    [mutation:create_link(  elements:id(Cortex), To) 
        ||   To <- get_inputs( CompiledLayers)],
    [mutation:create_link(From, elements:id(Cortex)) 
        || From <- get_outputs(CompiledLayers)],
    elements:id(Cortex).

get_inputs(CompiledLayers) ->
    maps:get(-1.0, CompiledLayers).
get_outputs(CompiledLayers) ->
    maps:get(+1.0, CompiledLayers).

%%--------------------------------------------------------------------
%% @doc Cortex id start function for supervisor. 
%% @end
%%--------------------------------------------------------------------
-spec start_link(Cortex_Id :: id()) -> gen_statem:start_ret().
start_link(Cortex_Id) -> 
    NNSup_Pid = self(),
    TId_IdPids = ets:new(nn_idpids, [{read_concurrency, true}, public]),
    gen_statem:start_link(?MODULE,
        [
            {id, Cortex_Id},
            {nn_sup, NNSup_Pid},
            {tid_idpids, TId_IdPids} 
        ], []).

%%--------------------------------------------------------------------
%% @doc Makes a prediction using the external inputs.
%% @end
%%--------------------------------------------------------------------
-spec predict(Cortex_Pid :: pid(), ExternalInputs :: [float()]) ->
    Predictions :: [float()].
predict(Cortex_Pid, ExternalInputs) ->
    gen_statem:call(Cortex_Pid, {feedforward, ExternalInputs}, ?STDCALL_TIMEOUT).

%%--------------------------------------------------------------------
%% @doc Performs a single NN training using back propagation.
%% @end
%%--------------------------------------------------------------------
-spec fit(Cortex_Pid :: pid(), OptimalOutputs :: [float()]) ->
    Errors :: [float()].
fit(Cortex_Pid, OptimalOutputs) ->
    gen_statem:call(Cortex_Pid, {backprop, OptimalOutputs}, ?STDCALL_TIMEOUT).

%%--------------------------------------------------------------------
%% @doc Returns the pid of a neuron from its id.
%% @end
%%--------------------------------------------------------------------
-spec nn_id2pid(Neuron_Id :: neuron:id(), TId_IdPids :: ets:tid()) ->
    Neuron_Pid :: pid().
nn_id2pid(Neuron_Id, TId_IdPids) ->
    [{Neuron_Id, Neuron_Pid}] = ets:lookup(TId_IdPids, Neuron_Id),
    Neuron_Pid.

%%--------------------------------------------------------------------
%% @doc Returns the id of a neuron from its pid.
%% @end
%%--------------------------------------------------------------------
-spec nn_pid2id(Neuron_Pid :: pid(), TId_IdPids :: ets:tid()) ->
    Neuron_Id :: neuron:id().
nn_pid2id(Neuron_Pid, TId_IdPids) ->
    [{Neuron_Pid, Neuron_Id}] = ets:lookup(TId_IdPids, Neuron_Pid),
    Neuron_Id.


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
init([Option | Rest]) ->
    case Option of
        {id, Cortex_Id} ->
            put(id, Cortex_Id);
        {nn_sup, NNSup_Pid} ->
             put(nn_sup, NNSup_Pid);
        {tid_idpids, TId_IdPids} ->
             put(tid_idpids, TId_IdPids)
    end,
    init(Rest);
init([]) -> 
    Id = get(id),
    ets:insert(get(tid_idpids), [{Id, self()}, {self(), Id}]),
    process_flag(trap_exit, true), % Mandatory to catch supervisor exits
    ?LOG_INFO("Cortex_Id:~p initiated", [Id]),
    {ok, inactive, #state{},
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
%%                     stop |
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
%% @doc inactive: The cortex is idle waiting to start a feedforward or 
%% backpropagation wave.  
%% @end
%%--------------------------------------------------------------------
inactive({call, From}, {feedforward, ExtInputs}, State) ->
    UpdatedOutputs = trigger_forward(get(outputs), ExtInputs),
    put(outputs, UpdatedOutputs),
    {next_state, on_feedforward, State#state{
        from = From,
        wait = [Input#input.pid || Input <- get(inputs)]
    }};
inactive({call, From}, {backprop, Optimals}, State) ->
    UpdatedInputs = trigger_backward(get(inputs), Optimals),
    put(inputs, UpdatedInputs),
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
%%--------------------------------------------------------------------
%% @doc on_feedforward: The cortex is collecting the results of and
%% signals of the forward wave. When all neurons have propagated their
%% values, the cortex returns the predictions to the requester.
%%--------------------------------------------------------------------
on_feedforward(info, {Pid, forward, Signal}, State) ->
    Input = lists:keyfind(Pid, #input.pid, get(inputs)),
    UpdatedInputs = lists:keyreplace(Pid, #input.pid, get(inputs), 
                                     Input#input{s = Signal}),
    put(inputs, UpdatedInputs),
    on_feedforward(internal, forward, State#state{
        wait = lists:delete(Pid, State#state.wait)
    });
on_feedforward(internal, forward, #state{wait = []} = State) ->
    {next_state, inactive, State,
     [{reply, State#state.from, 
       [Input#input.s || Input <- get(inputs)]}]};
on_feedforward(EventType, EventContent, State) ->
    handle_common(EventType, EventContent, State).
%%--------------------------------------------------------------------
%% @doc on_backpropagation: The cortex is collecting the results of 
%% and signals of the backward wave. When all neurons have propagated 
%% the values, the cortex returns the last correction values from the
%% inputs to the requester.
%%--------------------------------------------------------------------
on_backpropagation(info, {Pid, backward, BP_Err}, State) ->
    Output = lists:keyfind(Pid, #output.pid, get(outputs)),
    UpdatedOutputs = lists:keyreplace(Pid, #input.pid, get(outputs), 
                                      Output#output{error = BP_Err}),
    put(outputs, UpdatedOutputs),
    on_backpropagation(internal, backward, State#state{
        wait = lists:delete(Pid, State#state.wait)
    });
on_backpropagation(internal, backward, #state{wait = []} = State) ->
    {next_state, inactive, State,
     [{reply, State#state.from, 
       [hd(Input#input.acc) || Input <- get(inputs)]}]};
on_backpropagation(EventType, EventContent, State) ->
    handle_common(EventType, EventContent, State).
%%--------------------------------------------------------------------
%% @doc This function hanldes all the common events and raises an 
%% exception if the event/call is unknown.
%%--------------------------------------------------------------------
handle_common(enter, _OldState, State) ->
    {keep_state, State};
handle_common(internal, _EventContent, State) ->
    {keep_state, State};
handle_common(EventType, EventContent, _State) ->
    error({"Unknown event", EventType, EventContent}).


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
%%                     stop |
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

% ....................................................................
trigger_forward(Outputs, ExtInputs) ->
    [forward(Output, ExtInput) || 
            {Output, ExtInput} <- lists:zip(Outputs, ExtInputs)].

forward(Output, Signal) ->
    Output#output.pid ! {self(), forward, Signal},
    Output#output{s = Signal}.

% ....................................................................
trigger_backward(Inputs, Optimals) -> 
    [backward(Input, Optm) || 
             {Input, Optm} <- lists:zip(Inputs, Optimals)].

backward(Input, Optm) ->
    Error = Optm - Input#input.s,
    Input#input.pid ! {self(), backward, Error},
    Input#input{acc = [Error | Input#input.acc]}.

% ....................................................................
handle_start_nn() ->
    Cortex = edb:read(get(id)), 
    NNSup_Pid = get(nn_sup), 
    TId_IdPids = get(tid_idpids),
    Neurons = [{start_nn_element(NNSup_Pid, TId_IdPids, N_Id), edb:read(N_Id)} 
                  || N_Id <- elements:neurons(Cortex)],
    [Pid ! {continue_init, TId_IdPids} || {Pid, _} <- Neurons],
    put(neurons, maps:from_list(Neurons)),
    put(inputs,  [ #input{pid = cortex:nn_id2pid(Id, TId_IdPids)} 
        || Id <- elements:inputs_ids( Cortex)]),
    put(outputs, [#output{pid = cortex:nn_id2pid(Id, TId_IdPids)} 
        || Id <- elements:outputs_ids(Cortex)]).

start_nn_element(NNSup_Pid, TId_IdPids, Neuron_Id) ->
    case nn_sup:start_neuron(NNSup_Pid, Neuron_Id) of
        {ok, Pid} ->
            ets:insert(TId_IdPids, [{Neuron_Id, Pid}, {Pid, Neuron_Id}]),
            Pid;
        {error, Reason} ->
            exit(Reason)
    end.


%%====================================================================
%% Eunit white box tests
%%====================================================================

% --------------------------------------------------------------------
% TESTS DESCRIPTIONS -------------------------------------------------
cortex_white_test_() ->
    % {setup, Where, Setup, Cleanup, Tests | Instantiator}
    [
        {"Void example",
         {setup, local, fun no_setup/0, fun no_cleanup/1, fun example/1}}
    ].

% --------------------------------------------------------------------
% SPECIFIC SETUP FUNCTIONS -------------------------------------------
no_setup() ->
    ok.

no_cleanup(_) ->
    ok.

% --------------------------------------------------------------------
% ACTUAL TESTS -------------------------------------------------------
example(_) ->
    [
        ?_assert(true)
    ].

% --------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS -----------------------------------------







