%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc  
%%%
%%% TODO: Make gen_statem
%%% @end
%%%-------------------------------------------------------------------
-module(training).
-compile([export_all, nowarn_export_all]). %% TODO: To delete after build

-include_lib("math_constants.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
%%-export([start_link/0]).
-type_export([option/0, result/0]).

%% gen_statem callbacks
-export([init/5, predict/3, fit/3, results/3, terminate/1]).

-type option() :: {return, Returns :: [return()]} 
                | print
                | loss
                | log.
-type return() :: prediction
                | errors
                | loss.
-type result() :: [float()]
                | undefined.

-define(PROGRESS_BAR, #{size => 20}).

-record(state, {
    inputsList = [] :: [[number()]],
    optimaList = [] :: [[number()]],
    cycle :: integer(),
    log   :: undefined | {ok, reference()} | closed
}).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Executes a training (or only prediction) session.
%% @end
%%--------------------------------------------------------------------
-spec start_link(Cortex_Pid :: pid(), Inputs :: [float()], 
                 Optima :: [float()], Options :: [option()]) ->
    Resutls :: [result()].
start_link(Cortex_Pid, Inputs, Optima, Options) ->
    spawn_link(?MODULE,init,
        [self(), Cortex_Pid, Inputs, Optima, Options]
    ),
    receive {training, Returns} -> Returns end.


%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Executes the preparations for the training cycles. 
%% @end
%%--------------------------------------------------------------------
init(User_Pid, Cortex_Pid, InputsList, OptimaList, Options) -> 
    put(  user_pid,   User_Pid),
    put(cortex_pid, Cortex_Pid),
    put(prediction_list, []),
    put(    errors_list, []),
    put( bp_errors_list, []),
    init(Options, #{}, #state{
        inputsList = InputsList,
        optimaList = OptimaList,
        cycle      = 0
    }).

init([{return, Returns} | Options], Map, State) -> 
    init(Options, Map#{return => Returns}, State);
init([            print | Options], Map, State) -> 
    Size = length(State#state.inputsList),
    init(Options, Map#{print => Size}, State);
init([             loss | Options], Map, State) -> 
    put(loss_list, []), 
    init(Options, Map#{loss => true}, State);
init([{   log, LogName} | Options], Map, State) -> 
    {ok, LogRef} = LogState = datalog:new(LogName), 
    init(Options, Map#{log => LogRef}, State#state{log = LogState});
init([], OptionsMap, State) -> 
    put(options, OptionsMap), 
    predict(enter, init, State).

%%--------------------------------------------------------------------
%% @private
%% @doc Executes the training cycle
%%--------------------------------------------------------------------
%% predict: Performs a prediction from the input    
%%--------------------------------------------------------------------
predict(enter, _OldState, #state{inputsList = []} = State) ->
    terminate(State);
predict(enter, _OldState, State) ->
    [Inputs | InputsList] = State#state.inputsList,
    Data = #{inputs => Inputs},
    predict(internal, Data, State#state{inputsList = InputsList});
predict(internal, Data, State) -> 
    Inputs = maps:get(inputs, Data),
    Cortex = get(cortex_pid),
    Prediction = cortex:predict(Cortex, Inputs),
    put(prediction_list, [Prediction | get(prediction_list)]),
    put(data, Data#{prediction => Prediction}),
    fit(enter, predict, State).
%%--------------------------------------------------------------------
%% fit: Trains the model if optima available
%%--------------------------------------------------------------------
fit(enter, _OldState, #state{optimaList = []} = State) ->
    results(enter, fit, State);
fit(enter, _OldState, State) -> 
    [Optima | OptimaList] = State#state.optimaList,
    Data = maps:put(optima, Optima, get(data)),
    fit(calc_errors, Data, State#state{optimaList = OptimaList});
fit(calc_errors, Data, State) -> 
    Optima     = maps:get(    optima, Data),
    Prediction = maps:get(prediction, Data),
    Errors     = [O-P || {O,P} <- lists:zip(Optima, Prediction)],
    put(errors_list, [Errors | get(errors_list)]),
    fit(do_fit, Data#{errors => Errors}, State);
fit(do_fit, Data, State) -> 
    Cortex    = get(cortex_pid),
    Errors    = maps:get(errors, Data),
    BP_Errors = cortex:fit(Cortex, Errors),
    put(bp_errors_list, [BP_Errors | get(bp_errors_list)]),
    put(data, Data#{bp_errors => BP_Errors}),
    results(enter, fit, State).
%%--------------------------------------------------------------------
%% results: Applies the result functions (Loss calculation, log, etc.)
%%--------------------------------------------------------------------
results(enter, _OldState, State) ->
    Options = get(options),
    Data    = get(data),
    results(Options, Data, State);
results(#{loss := true} = Options, Data, State)  ->
    Loss = ?LOSS(maps:get(errors, Data)),
    put(loss_list, [Loss | get(loss_list)]),
    results(maps:remove(loss, Options), Data#{loss => Loss}, State);
results(#{print:=Size} = Options, Data, #state{cycle=Cyc} = State)
when Cyc rem ceil(Size/10) == 0 -> 
    Errors = maps:get(errors, Data),
    Report = [Cyc, Cyc/Size, "acu:",lists:sum(Errors)/length(Errors)],
    Print  = reports:progress_line(2, Report, ?PROGRESS_BAR),
    io:format([Print | "\n"]),
    results(maps:remove(print, Options), Data, State);
results(#{log := LogRef} = Options, Data, State) ->
    datalog:write(LogRef, Data),
    results(maps:remove(log, Options), Data, State);
results(_NotResultOptions, Data, State) -> 
    put(data, Data),
    predict(enter, results, State). 
%%--------------------------------------------------------------------
%% @end
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @private
%% @doc Cleans the training and returns the results.
%% @end
%%--------------------------------------------------------------------
terminate(#state{log = {ok, LogRef}} = State) -> 
    ok = datalog:close(LogRef),
    terminate(State#state{log = closed});
terminate(_State) ->
    #{return := Returns} = get(options),
    User_Pid = get(user_pid),
    User_Pid ! {training, return(Returns)}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

return([prediction | Rest]) -> 
    [lists:reverse(get(prediction_list)) | return(Rest)];
return([errors | Rest]) -> 
    [lists:reverse(get(errors_list)) | return(Rest)];
return([loss | Rest]) -> 
    [lists:reverse(get(loss_list)) | return(Rest)];
return([]) -> 
    [].


%%====================================================================
%% Eunit white box tests
%%====================================================================

% --------------------------------------------------------------------
% SPECIFIC SETUP FUNCTIONS -------------------------------------------
pred_err_lists() ->
    put(prediction_list, [random_list(3) || _ <- lists:seq(1, 2)]),
    put(errors_list, [random_list(3) || _ <- lists:seq(1, 2)]),
    ok.

with_loss_lists() ->
    pred_err_lists(),
    put(loss_list, [random_list(3) || _ <- lists:seq(1, 2)]).

no_cleanup(_) ->
    ok.

% --------------------------------------------------------------------
% TESTS DESCRIPTIONS -------------------------------------------------
white_test_() ->
    % {setup, Where, Setup, Cleanup, Tests | Instantiator}
    [
        {"Tests for basic returns (predictions and errors)",
         {setup, local, fun pred_err_lists/0, fun no_cleanup/1, 
          [
            ?_assert([lists:reverse(get(prediction_list))] == 
                      return([prediction])),
            ?_assert([undefined] == 
                      return([loss])),
            ?_assert([lists:reverse(get(errors_list)), 
                      lists:reverse(get(prediction_list))] ==
                     return([errors, prediction])),
            ?_assert([lists:reverse(get(prediction_list)), 
                      lists:reverse(get(errors_list))] ==
                     return([prediction, errors]))
          ]}},
        {"Tests for returns with loss",
         {setup, local, fun with_loss_lists/0, fun no_cleanup/1, 
          [
            ?_assert([lists:reverse(get(prediction_list))] == 
                      return([prediction])),
            ?_assert([lists:reverse(get(loss_list))] == 
                      return([loss])),
            ?_assert([lists:reverse(get(errors_list)), 
                      lists:reverse(get(loss_list))] ==
                     return([errors, loss])),
            ?_assert([lists:reverse(get(loss_list)), 
                      lists:reverse(get(errors_list))] ==
                     return([loss, errors]))
          ]}}

    ].

% --------------------------------------------------------------------
% ACTUAL TESTS -------------------------------------------------------


% --------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS ------------------------------------------

random_list(N) -> 
    [(rand:uniform(20) - 10) /10 || _ <- lists:seq(1, N)].







