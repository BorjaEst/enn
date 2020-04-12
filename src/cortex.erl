%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc The cortex is a NN synchronizing element. It needs to know 
%%% the Pid of every neural network element, so that it will know when
%%% all the outputs have received their control inputs, and that it’s
%%% time for the inputs to again gather and fanout input data to the
%%% neurons in the input layer. 
%%
%%
%%  TODO: Try to move the ets table to the nn_sup 
%%  TODO: probably the fan_in fan_out function is not correct
%%% @end
%%%-------------------------------------------------------------------
-module(cortex).
-compile([export_all, nowarn_export_all]). %% TODO: To delete after build

-include_lib("network.hrl").
-include_lib("cortex.hrl").
-include_lib("math_constants.hrl").
-include_lib("enn_logger.hrl").
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
-type serialized_graph() :: term().

-record(input,  {
    id      :: id(),        % Neuron id (input)
    s = 0.0 :: float()      % Signal value (Xi)
}).
-record(output, {
    id      :: id(),        % Neuron id (output)
    e = 0.0 :: float()      % Output error (Ei)
}).

-record(state, {
    wait = [] :: [term()]
}).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new cortex from the compiled layers, stores it on 
%% the database and returns its cortex id.
%% @end
%%--------------------------------------------------------------------
-spec new(NInputs, NOutputs, SerialisedGraph) -> cortex_id() when 
    NInputs         :: integer(),
    NOutputs        :: integer(),
    SerialisedGraph :: serialized_graph().
new(NInputs, NOutputs, SerialisedGraph) ->
    Cortex = #cortex{
        inputs           = NInputs,
        outputs          = NOutputs,
        serialised_graph = SerialisedGraph
    },
    edb:write(Cortex),
    Cortex#cortex.id.

%%--------------------------------------------------------------------
%% @doc Cortex id start function for supervisor. 
%% @end
%%--------------------------------------------------------------------
-spec start_link(Cortex_Id :: id()) -> gen_statem:start_ret().
start_link(Cortex_Id) -> 
    gen_statem:start_link(?MODULE, [Cortex_Id], []).

%%--------------------------------------------------------------------
%% @doc Makes a prediction using the external inputs.
%% @end
%%--------------------------------------------------------------------
-spec predict(Cortex_Pid :: pid(), ExternalInputs :: [float()]) ->
    Predictions :: [float()].
predict(Cortex_Pid, ExternalInputs) ->
    gen_statem:call(Cortex_Pid, {feedforward, ExternalInputs}).

%%--------------------------------------------------------------------
%% @doc Performs a single NN training using back propagation.
%% @end
%%--------------------------------------------------------------------
-spec fit(Cortex_Pid :: pid(), Errors :: [float()]) ->
    BP_Errors :: [float()].
fit(Cortex_Pid, Errors) ->
    gen_statem:call(Cortex_Pid, {backprop, Errors}).

%%--------------------------------------------------------------------
%% @doc Returns the pid of a neuron from its id.
%% @end
%%--------------------------------------------------------------------
-spec nn_id2pid(Id, Network) -> pid() when 
    Id      :: neuron:id() | cortex:id(),
    Network :: network().
nn_id2pid(Id, Network) ->
    {Id, Pid} = digraph:vertex(Network#network.graph, Id),
    Pid.

%%--------------------------------------------------------------------
%% @doc Returns the id of a neuron from its pid.
%% @end
%%--------------------------------------------------------------------
-spec nn_pid2id(Pid, Network) -> Id when 
    Pid     :: pid(),
    Network :: network(),
    Id      :: neuron:id() | cortex:id().
nn_pid2id(Pid, Network) ->
    [{Pid, Id}] = ets:lookup(Network#network.pid_pool, Pid),
    Id.


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
init([Id = Network_Id]) -> 
    Graph = enn:mount_graph(Network_Id),
    IdPT  = ets:lookup_element(?NN_POOL,Network_Id,#network.pid_pool),
    true  = ets:insert(IdPT, [{self(), Id}]),
    true  = digraph:add_vertex(Graph, Id, self()),
    true  = ets:update_element(?NN_POOL,Network_Id,
        [{#network.cortex, self()}, {#network.graph, Graph}]
    ),
    process_flag(trap_exit, true), % To catch supervisor 'EXIT'
    put(id, Id),
    ?LOG_STATE_CHANGE(undefined),
    {ok, inactive, #state{wait = []},
        {next_event, internal, {start_nn, Network_Id}}}.

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
%%
%%--------------------------------------------------------------------
inactive(enter, OldState, State) ->
    ?LOG_STATE_CHANGE(OldState),
    {keep_state, State};
inactive({call, From}, {feedforward, Inputs}, State) ->
    ?LOG_EVENT_FEEDFORWARD(Inputs),
    Sent = [forward(P,S)||{{P,_},S}<-lists:zip(get(outputs),Inputs)],
    ?LOG_FORWARD_PROPAGATION(Sent),
    put(from, From),
    {next_state, on_feedforward, State#state{
        wait = [Pid || {Pid, _} <- get(inputs)]
    }};
inactive({call, From}, {backprop, Errors}, State) ->
    ?LOG_EVENT_BACKFORWARD(Errors),
    Sent = [backward(P,E)||{{P,_},E}<-lists:zip(get(inputs),Errors)],
    ?LOG_BACKWARD_PROPAGATION(Sent),
    put(from, From),
    {next_state, on_backpropagation, State#state{
        wait = [Pid || {Pid, _}  <- get(outputs)]
    }};
inactive(internal, {start_nn, NN}, State) ->
    ?LOG_EVENT_START_NEURONS_NETWORK,
    handle_start(NN),
    {keep_state, State};
inactive(EventType, EventContent, State) ->
    handle_common(EventType, EventContent, State).
%%--------------------------------------------------------------------
%% @doc on_feedforward: The cortex is collecting the results of and
%% signals of the forward wave. When all neurons have propagated their
%% values, the cortex returns the predictions to the requester.
%%
%%--------------------------------------------------------------------
on_feedforward(enter, OldState, State) ->
    ?LOG_STATE_CHANGE(OldState),
    {keep_state, State};
on_feedforward(info, {Pid, forward, Signal}, State) ->
    ?LOG_FORWARD_MESSAGE_RECEIVED(Pid, Signal),
    update_input_signal(Pid, Signal),
    on_feedforward(internal, forward, State#state{
        wait = lists:delete(Pid, State#state.wait)
    });
on_feedforward(internal, forward, #state{wait = []} = State) ->
    Signals = [I#input.s || {_, I} <- get(inputs)],
    {next_state, inactive, State, {reply, get(from), Signals}};
on_feedforward(EventType, EventContent, State) ->
    handle_common(EventType, EventContent, State).
%%--------------------------------------------------------------------
%% @doc on_backpropagation: The cortex is collecting the results of 
%% and signals of the backward wave. When all neurons have propagated 
%% the values, the cortex returns the last correction values from the
%% inputs to the requester.
%%
%%--------------------------------------------------------------------
on_backpropagation(enter, OldState, State) ->
    ?LOG_STATE_CHANGE(OldState),
    {keep_state, State};
on_backpropagation(info, {Pid, backward, BP_Err}, State) ->
    ?LOG_BACKWARD_MESSAGE_RECEIVED(Pid, BP_Err),
    update_output_error(Pid, BP_Err),
    on_backpropagation(internal, backward, State#state{
        wait = lists:delete(Pid, State#state.wait)
    });
on_backpropagation(internal, backward, #state{wait = []} = State) ->
    BP_Err = [O#output.e || {_, O} <- get(outputs)],
    {next_state, inactive, State, {reply, get(from), BP_Err}};
on_backpropagation(EventType, EventContent, State) ->
    handle_common(EventType, EventContent, State).
%%--------------------------------------------------------------------
%% @doc This function hanldes all the common events and raises an 
%% exception if the event/call is unknown.
%%
%%--------------------------------------------------------------------
handle_common(internal, _EventContent, State) ->
    {keep_state, State};
handle_common(EventType, EventContent, _State) ->
    error({"Unknown event", EventType, EventContent}).
%%--------------------------------------------------------------------
%% @end
%%-------------------------------------------------------------------

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
terminate(_Reason, OldState, _State) ->
    ?LOG_STATE_CHANGE(OldState),
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

% -------------------------------------------------------------------
update_input_signal(Pid, Signal) -> 
    {value, {_, I}} = lists:keysearch(Pid, 1, get(inputs)),
    Inputs = lists:keyreplace(Pid, 1, get(inputs), 
                             {Pid, I#input{s=Signal}}),
    put(inputs, Inputs).

% -------------------------------------------------------------------
update_output_error(Pid, Error) -> 
    {value, {_, O}} = lists:keysearch(Pid, 1, get(outputs)),
    Outputs = lists:keyreplace(Pid, 1, get(outputs),
                              {Pid, O#output{e=Error}}),
    put(outputs, Outputs).

% -------------------------------------------------------------------
forward(Output_Pid, Signal) ->
    Output_Pid ! {self(), forward, Signal},
    {Output_Pid, Signal}.

% -------------------------------------------------------------------
backward(Input_Pid, Error) ->
    Input_Pid ! {self(), backward, Error},
    {Input_Pid, Error}.

% -------------------------------------------------------------------
handle_start(Network_Id) ->
    NN = enn:network(Network_Id),
    Neurons = [start_neuron(  Id, NN) || Id  <- enn:neurons(NN)],
              [resume_neuron(Pid, NN) || Pid <- Neurons],
    Inputs  = [{nn_id2pid(Id,NN),#output{}} || Id <-enn:inputs( NN)],
    Outputs = [{nn_id2pid(Id,NN),#input{ }} || Id <-enn:outputs(NN)],
    put( inputs, Outputs), % System outputs are the cortex inputs
    put(outputs,  Inputs). % System inputs are the cortex outputs

start_neuron(Id, NN) ->
    Pid  = nn_sup:start_neuron(Id, NN),
    true = ets:insert(enn:table
    , Arg2)

resume_neuron(Pid, NN) -> 
    Pid ! {continue_init, NN#network.pid_pool}.


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







