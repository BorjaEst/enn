%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc This logger enables certain logs dependign on what is desired
%%% to debug. This module is done to reduce the performance cost of 
%%% logging the whole system. The current tags allowed are:
%%%     - debug_enn_all: Enable all debugs.
%%%     - debug_activation: Activation function requests and results.
%%%     - debug_aggregation: Aggregation function req. and results.
%%%     - debug_initialization: Initialization values.
%%%     - debug_cortex_states: Log cortex status changes.
%%%     - debug_cortex_requests: Log cortex requests.
%%%     - debug_propagation: Debug propagation messages.
%%%     - debug_neurons_status: Debug the status of every neruon.
%%% @end
%%%-------------------------------------------------------------------

%% Based on the kernel logger
-include_lib("kernel/include/logger.hrl").

-ifdef(debug_enn_all).
-define(     debug_activation, true).
-define(    debug_aggregation, true).
-define( debug_initialization, true).
-define(  debug_cortex_states, true).
-define(debug_cortex_requests, true).
-define(    debug_propagation, true).
-define( debug_neurons_status, true).
-endif.


%%--------------------------------------------------------------------
%% @doc Enables logs on activation functions.
%% @end
%%--------------------------------------------------------------------
-ifdef(debug_activation).
-define(LOG_ACTIVATION_FUNCTION_REQUEST(Function, Soma), 
    ?LOG_DEBUG(#{what => "Activation function calculation request",
                 pid=>self(), id => get(id),
                 details => #{func => Function, soma => Soma}},
               #{logger_formatter=>#{title=>"ACTIVATION FUNCTION"}}) 
).
-define(LOG_ACTIVATION_FUNCTION_RESULT(Function, Result), 
    ?LOG_DEBUG(#{what => "Activation calculation result",
                 pid=>self(), id => get(id),
                 details => #{func => Function, result => Result}},
               #{logger_formatter=>#{title=>"ACTIVATION FUNCTION"}}) 
).
-define(LOG_ACTIVATION_DERIVADE_REQUEST(Function, Soma), 
    ?LOG_DEBUG(#{what => "Derivade of activation function request",
                 pid=>self(), id => get(id),
                 details => #{func => Function, soma => Soma}},
               #{logger_formatter=>#{title=>"ACTIVATION FUNCTION"}}) 
).
-define(LOG_ACTIVATION_DERIVADE_RESULT(Function, Result), 
    ?LOG_DEBUG(#{what => "Derivade calculation result",
                 pid=>self(), id => get(id),
                 details => #{func => Function, result => Result}},
               #{logger_formatter=>#{title=>"ACTIVATION FUNCTION"}}) 
).
-define(LOG_ACTIVATION_BETA_REQUEST(Function, Error, Soma), 
    ?LOG_DEBUG(#{what => "Beta function calculation request",
                 pid=>self(), id => get(id), details => #{
                     func => Function, error => Error, soma => Soma}},
               #{logger_formatter=>#{title=>"ACTIVATION FUNCTION"}}) 
).
-define(LOG_ACTIVATION_BETA_RESULT(Function, Result), 
    ?LOG_DEBUG(#{what => "Beta calculation result",
                 pid=>self(), id => get(id),
                 details => #{func => Function, result => Result}},
               #{logger_formatter=>#{title=>"ACTIVATION FUNCTION"}}) 
).
-else.
-define(LOG_ACTIVATION_FUNCTION_REQUEST(F, V), F,  V).
-define(LOG_ACTIVATION_FUNCTION_RESULT(F, V),  F,  V).
-define(LOG_ACTIVATION_DERIVADE_REQUEST(F, V), F,  V).
-define(LOG_ACTIVATION_DERIVADE_RESULT(F, V),  F,  V).
-define(LOG_ACTIVATION_BETA_REQUEST(F, E, V),  F,E,V).
-define(LOG_ACTIVATION_BETA_RESULT(F, V),      F,  V).
-endif.

%%--------------------------------------------------------------------
%% @doc Enables logs on aggregation functions.
%% @end
%%--------------------------------------------------------------------
-ifdef(debug_aggregation).
-define(LOG_AGGREGATION_FUNCTION_REQUEST(Function, Tensor, Bias),
    ?LOG_DEBUG(#{what => "Aggregation function calculation request",
                 pid  => self(), id => get(id), details => #{
                     func=>Function, tensor=>Tensor, bias=>Bias}},
               #{logger_formatter=>#{title=>"AGGREGATION FUNCTION"}})
).
-define(LOG_AGGREGATION_FUNCTION_RESULT(Function, Result), 
    ?LOG_DEBUG(#{what => "Aggregation calculation result",
                 pid  => self(), id => get(id),
                 details => #{func => Function, result => Result}},
               #{logger_formatter=>#{title=>"AGGREGATION FUNCTION"}})
).
-else.
-define(LOG_AGGREGATION_FUNCTION_REQUEST(F, T, V), F,T,V).
-define(LOG_AGGREGATION_FUNCTION_RESULT(F, V),     F,  V).
-endif.

%%--------------------------------------------------------------------
%% @doc Enables logs on initialization functions.
%% @end
%%--------------------------------------------------------------------
-ifdef(debug_initialization).
-define(LOG_INITIALIZATION_REQUEST(Function, Arg),
    ?LOG_DEBUG(#{what => "Initialization value calculation request",
                 pid  => self(), id => get(id), 
                 details => #{func => Function, arg => Arg}},
            #{logger_formatter=>#{title=>"INITIALIZATION FUNCTION"}})
).
-define(LOG_INITIALIZATION_RESULT(Function, Result), 
    ?LOG_DEBUG(#{what => "Initialization value result",
                 pid  => self(), id => get(id),
                 details => #{func => Function, result => Result}},
            #{logger_formatter=>#{title=>"INITIALIZATION FUNCTION"}})
).
-else.
-define(LOG_INITIALIZATION_REQUEST(F, V), F,V).
-define(LOG_INITIALIZATION_RESULT(F, V),  F,V).
-endif.

%%--------------------------------------------------------------------
%% @doc Enables logs on cortex state changes.
%% @end
%%--------------------------------------------------------------------
-ifdef(debug_cortex_states).
-define(LOG_STATE_CHANGE(OldState),
    ?LOG_INFO(#{what => "Cortex state has changed", 
                pid=>self(), id => get(id), details => #{
                    state_new=>?FUNCTION_NAME, old_state=>OldState}},
              #{logger_formatter=>#{title=>"CORTEX STATE"}})
).
-else.
-define(LOG_STATE_CHANGE(OldState), OldState).
-endif.

%%--------------------------------------------------------------------
%% @doc Enables logs on cortex requests.
%% @end
%%--------------------------------------------------------------------
-ifdef(debug_cortex_requests).
-define(LOG_EVENT_FEEDFORWARD(ExtInputs),
    ?LOG_DEBUG(#{what => "Feedforward request",
                 pid => self(), id => get(id), details => #{
                    ext_inputs=>ExtInputs, state=>?FUNCTION_NAME}},
               #{logger_formatter=>#{title=>"CORTEX REQUEST"}})
).
-define(LOG_EVENT_BACKWARD(Errors),
    ?LOG_DEBUG(#{what => "Backward request",
                 pid => self(), id => get(id), details => #{
                    ext_errors=>Errors,    state=>?FUNCTION_NAME}},
               #{logger_formatter=>#{title=>"CORTEX REQUEST"}})
).
-else.
-define(LOG_EVENT_FEEDFORWARD(ExtInputs), ExtInputs).
-define(LOG_EVENT_BACKWARD(Errors), Errors).
-endif.

%%--------------------------------------------------------------------
%% @doc Enables logs on propagations.
%% @end
%%--------------------------------------------------------------------
-ifdef(debug_propagation).
-define(LOG_FORWARD_MESSAGE_RECEIVED(From, Signal),
    ?LOG_DEBUG(#{what => "Forward message received", pid => self(),
                 id => get(id), details => {From, Signal}},
               #{logger_formatter=>#{title=>"PROPGATION DEBUG"}})
).
-define(LOG_BACKWARD_MESSAGE_RECEIVED(From, Error),
    ?LOG_DEBUG(#{what => "Backward message received",  pid => self(),
                 id => get(id), details => {From, Error}},
               #{logger_formatter=>#{title=>"PROPAGATION DEBUG"}})
).
-define(LOG_FORWARD_PROPAGATION(Sent),
    ?LOG_DEBUG(#{what => "Forward propagation trigger", pid=>self(), 
                 id => get(id), details => Sent},
               #{logger_formatter=>#{title=>"PROPAGATION DEBUG"}})
).
-define(LOG_FORWARD_PROPAGATION_RECURRENT_OUTPUTS(Sent),
    ?LOG_DEBUG(#{what => "Forward prop trigger (recurrent outputs)", 
                 pid=>self(), id => get(id), details => Sent},
               #{logger_formatter=>#{title=>"PROPAGATION DEBUG"}})
).
-define(LOG_BACKWARD_PROPAGATION(Sent),
    ?LOG_DEBUG(#{what => "Backward propagation trigger", pid=>self(),
                 id => get(id), details => Sent},
               #{logger_formatter=>#{title=>"PROPAGATION DEBUG"}})
).
-define(LOG_BACKWARD_PROPAGATION_RECURRENT_INPUTS(Sent),
    ?LOG_DEBUG(#{what => "Backward prop trigger (recurrent inputs)", 
                 pid=>self(), id => get(id), details => Sent},
               #{logger_formatter=>#{title=>"PROPAGATION DEBUG"}})
).
-else.
-define(LOG_FORWARD_MESSAGE_RECEIVED(From, Signal), From, Signal).
-define(LOG_BACKWARD_MESSAGE_RECEIVED(From, Error), From, Error).
-define(LOG_FORWARD_PROPAGATION(Sent), Sent).
-define(LOG_FORWARD_PROPAGATION_RECURRENT_OUTPUTS(Sent), Sent).
-define(LOG_BACKWARD_PROPAGATION(Sent), Sent).
-define(LOG_BACKWARD_PROPAGATION_RECURRENT_INPUTS(Sent), Sent).
-endif.

%%--------------------------------------------------------------------
%% @doc Enables logs on neurons states.
%% @end
%%--------------------------------------------------------------------
-ifdef(debug_neurons_status).
-define(LOG_NEURON_STARTED,
    ?LOG_DEBUG(#{what => "Neuron started", pid =>self(), id=>get(id), 
                 details => #{}},
               #{logger_formatter=>#{title=>"NEURON STATUS"}})
).
-define(LOG_WAITING_NEURONS(State),
    ?LOG_DEBUG(#{what => "Neuron waiting for these signals", 
                 details => #{forward  => State#state.forward_wait,
                              backward => State#state.backward_wait},
                 pid  => self(), id => get(id)},
               #{logger_formatter=>#{title=>"NEURON STATUS"}})
).
-define(LOG_NEURON_IDLE(State),
    ?LOG_WARNING(#{what => "Neuron idle or stock", 
                   details=>#{forward  => State#state.forward_wait,
                              backward => State#state.backward_wait},
                   pid  => self(), id => get(id)},
                 #{logger_formatter=>#{title=>"NEURON STATUS"}})
).
-define(LOG_NEURON_TERMINATING,
    ?LOG_DEBUG(#{what => "Neuron terminating", 
                 pid  => self(), id => get(id), details => []},
               #{logger_formatter=>#{title=>"NEURON STATUS"}})
).
-else.
-define(LOG_NEURON_STARTED,            ok).
-define(LOG_WAITING_NEURONS(State), State).
-define(LOG_NEURON_IDLE(State),     State).
-define(LOG_NEURON_TERMINATING,        ok).
-endif.

