%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc This logger enables certain logs dependign on what is desired
%%% to debug. This module is done to reduce the performance cost of 
%%% logging the whole system. The current tags allowed are:
%%%     - debug_enn_all: Enable all debugs.
%%%     - debug_activation: Activation function requests and results.
%%%     - debug_cortex_states: Log cortex status changes.
%%%     - debug_cortex_requests: Log cortex requests.
%%%     - debug_propagation: Debug propagation messages.
%%%     - debug_neurons_status: Debug the status of every neruon.
%%% @end
%%%-------------------------------------------------------------------

%% Based on the kernel logger
-include_lib("kernel/include/logger.hrl").

-ifdef(debug_enn_all).
-define(debug_activation, true).
-define(debug_cortex_states, true).
-define(debug_cortex_requests, true).
-define(debug_propagation, true).
-define(debug_neurons_status, true).
-endif.


%%--------------------------------------------------------------------
%% @doc Enables logs on activation functions.
%% @end
%%--------------------------------------------------------------------
-ifdef(debug_activation).
-define(LOG_ACTIVATION_FUNCTION_REQUEST(Function, Soma), 
    ?LOG_DEBUG(#{desc => "Activation function calculation request",
                 func => Function, soma => Soma, pid  => self()},
               #{logger_formatter=>#{title=>"ACTIVATION FUNCTION"}}) 
).
-define(LOG_ACTIVATION_FUNCTION_RESULT(Function, Result), 
    ?LOG_DEBUG(#{desc => "Activation function calculation result",
                 func => Function, result => Result, pid  => self()},
               #{logger_formatter=>#{title=>"ACTIVATION FUNCTION"}}) 
).
-define(LOG_ACTIVATION_DERIVADE_REQUEST(Function, Soma), 
    ?LOG_DEBUG(#{desc => "Derivade of activation function request",
                 func => Function, soma => Soma, pid  => self()},
               #{logger_formatter=>#{title=>"ACTIVATION FUNCTION"}}) 
).
-define(LOG_ACTIVATION_DERIVADE_RESULT(Function, Result), 
    ?LOG_DEBUG(#{desc => "Derivade of activation function result",
                 func => Function, result => Result, pid  => self()},
               #{logger_formatter=>#{title=>"ACTIVATION FUNCTION"}}) 
).
-else.
-define(LOG_ACTIVATION_FUNCTION_REQUEST(F, V), F,V).
-define(LOG_ACTIVATION_FUNCTION_RESULT(F, V),  F,V).
-define(LOG_ACTIVATION_DERIVADE_REQUEST(F, V), F,V).
-define(LOG_ACTIVATION_DERIVADE_RESULT(F, V),  F,V).
-endif.


%%--------------------------------------------------------------------
%% @doc Enables logs on cortex state changes.
%% @end
%%--------------------------------------------------------------------
-ifdef(debug_cortex_states).
-define(LOG_STATE_CHANGE(OldState),
    ?LOG_INFO(#{desc => "Cortex state has changed", 
                state_new => ?FUNCTION_NAME, old_state => OldState},
              #{logger_formatter=>#{title=>"CORTEX STATE CHANGE"}})
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
    ?LOG_DEBUG(#{desc => "Feedforward request",
                 ext_inputs => ExtInputs, state => ?FUNCTION_NAME},
               #{logger_formatter=>#{title=>"CORTEX EVENT"}})
).
-define(LOG_EVENT_BACKFORWARD(Errors),
    ?LOG_DEBUG(#{desc => "Backforward request",
                 ext_errors => Errors, state => ?FUNCTION_NAME},
               #{logger_formatter=>#{title=>"CORTEX EVENT"}})
).
-define(LOG_EVENT_START_NEURONS_NETWORK,
    ?LOG_DEBUG(#{desc => "Neurons network start request",
                 state => ?FUNCTION_NAME},
               #{logger_formatter=>#{title=>"CORTEX EVENT"}})
).
-else.
-define(LOG_EVENT_FEEDFORWARD(ExtInputs), ExtInputs).
-define(LOG_EVENT_BACKFORWARD(Errors), Errors).
-define(LOG_EVENT_START_NEURONS_NETWORK, ok).
-endif.


%%--------------------------------------------------------------------
%% @doc Enables logs on propagations.
%% @end
%%--------------------------------------------------------------------
-ifdef(debug_propagation).
-define(LOG_FORWARD_MESSAGE_RECEIVED(From, Signal),
    ?LOG_DEBUG(#{desc => "Forward message received", 
                 pid => self(), id => get(id), From => Signal},
               #{logger_formatter=>#{title=>"PROPGATION DEBUG"}})
).
-define(LOG_BACKWARD_MESSAGE_RECEIVED(From, Error),
    ?LOG_DEBUG(#{desc => "Backward message received", 
                 pid => self(), id => get(id), From => Error},
               #{logger_formatter=>#{title=>"PROPAGATION DEBUG"}})
).
-define(LOG_FORWARD_PROPAGATION(Sent),
    ?LOG_DEBUG(#{desc => "Forward propagation trigger", 
                 pid=>self(), id => get(id), sent => Sent},
               #{logger_formatter=>#{title=>"PROPAGATION DEBUG"}})
).
-define(LOG_FORWARD_PROPAGATION_RECURRENT_OUTPUTS(Sent),
    ?LOG_DEBUG(#{desc => "Forward prop trigger (recurrent outputs)",
                 pid=>self(), id => get(id), sent => Sent},
               #{logger_formatter=>#{title=>"PROPAGATION DEBUG"}})
).
-define(LOG_BACKWARD_PROPAGATION(Sent),
    ?LOG_DEBUG(#{desc => "Backward propagation trigger", 
                 pid=>self(), id => get(id), sent => Sent},
               #{logger_formatter=>#{title=>"PROPAGATION DEBUG"}})
).
-define(LOG_BACKWARD_PROPAGATION_RECURRENT_INPUTS(Sent),
    ?LOG_DEBUG(#{desc => "Backward prop trigger (recurrent inputs)",
                 pid=>self(), id => get(id), sent => Sent},
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
    ?LOG_DEBUG(#{desc => "Neuron started", 
                 pid  => self(), id => get(id)},
               #{logger_formatter=>#{title=>"NEURON STATE DEBUG"}})
).
-define(LOG_WAITING_NEURONS(State),
    ?LOG_DEBUG(#{desc => "Neuron waiting for these signals", 
                 waiting => #{forward  => State#state.forward_wait,
                              backward => State#state.backward_wait},
                 pid=>self(), id => get(id)},
               #{logger_formatter=>#{title=>"NEURON STATE DEBUG"}})
).
-define(LOG_NEURON_TERMINATING,
    ?LOG_DEBUG(#{desc => "Neuron terminating", 
                 pid  => self(), id => get(id)},
               #{logger_formatter=>#{title=>"NEURON STATE DEBUG"}})
).
-else.
-define(LOG_NEURON_STARTED, ok).
-define(LOG_WAITING_NEURONS(State), State).
-define(LOG_NEURON_TERMINATING, ok).
-endif.


