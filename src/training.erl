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
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/4]).
-type_export([option/0, return/0, result/0]).

%% gen_statem callbacks
-export([init/5, predict/2, errors/2, fit/2, results/2, terminate/1]).

-type option() :: {return, Returns :: [return()]}  % Return values
                | { print,   Lines :: integer() }  % Reports in io
                | {   log, LogName :: string()  }. % Reports in file
-type return() :: prediction    % Predicted outputs
                | errors        % Errors = Optima - Predicted
                | loss          % Loss
                | bp_errors.    % Back propagation errors (-1.0layer)
-type result() :: [float()]
                | undefined.

-define(PROGRESS_BAR, #{size => 20}).

-record(data, {
    inputs,
    prediction,
    optima,
    errors,
    loss,
    bperrs
}).
-record(state, {
    inputsList = [] :: [[number()]],
    optimaList = [] :: [[number()]],
    predicList = [] :: [[number()]],
    errorsList = [] :: [[number()]],
    lossList   = [] :: [[number()]],
    bperrsList = [] :: [[number()]],
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
    init(Options, #{}, #state{
        inputsList = InputsList,
        optimaList = OptimaList,
        cycle      = 1
    }).

init([{return, Returns} | Options], Map, State) -> 
    init(Options, Map#{return => Returns}, State);
init([{ print,   Lines} | Options], Map, State) -> 
    Size = length(State#state.inputsList),
    Each = ceil(Size / Lines),
    init(Options, Map#{print => {Each, Size}}, State);
init([{   log, LogName} | Options], Map, State) -> 
    {ok, LogRef} = LogState = datalog:new(LogName), 
    init(Options, Map#{log => LogRef}, State#state{log = LogState});
init([], OptionsMap, State) -> 
    put(options, OptionsMap), 
    predict(#data{}, State).

%%--------------------------------------------------------------------
%% @private
%% @doc Executes the training cycle
%%--------------------------------------------------------------------
%% predict: Performs a prediction from the input    
%%--------------------------------------------------------------------
predict(_Data,#state{inputsList =                    []} = State) ->
    terminate(State);
predict(Data, #state{inputsList = [Inputs | InputsList]} = State) ->
    Prediction = cortex:predict(get(cortex_pid), Inputs),
    errors(
        Data#data{inputs = Inputs, prediction = Prediction}, 
        State#state{inputsList = InputsList}
    ).
%%--------------------------------------------------------------------
%% errors: Calculates the errors (if optima available)
%%--------------------------------------------------------------------
errors(Data, #state{optimaList =                    []} = State) -> 
    results(Data, State);
errors(Data, #state{optimaList = [Optima | OptimaList]} = State) -> 
    Errors = [O-P || {O,P} <-lists:zip(Optima,Data#data.prediction)],
    Loss   = ?LOSS(Errors),
    fit(
        Data#data{optima = Optima, errors = Errors, loss = Loss}, 
        State#state{optimaList = OptimaList}
    ).
%%--------------------------------------------------------------------
%% fit: Trains the model if optima available
%%--------------------------------------------------------------------
fit(Data, State) -> 
    BP_Errors = cortex:fit(get(cortex_pid), Data#data.errors),
    results(Data#data{bperrs = BP_Errors}, State).
%%--------------------------------------------------------------------
%% results: Applies the result functions (Loss calculation, log, etc.)
%%--------------------------------------------------------------------
results(Data, State) ->
    ok = options(get(options), Data, State),
    predict(#data{}, State#state{
        predicList = [Data#data.prediction | State#state.predicList],
        errorsList = [    Data#data.errors | State#state.errorsList],
        lossList   = [      Data#data.loss | State#state.lossList  ],
        bperrsList = [    Data#data.bperrs | State#state.bperrsList],
        cycle = State#state.cycle + 1
    }).
%%--------------------------------------------------------------------
%% @end
%%-------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @private
%% @doc Cleans the training and returns the results.
%% @end
%%--------------------------------------------------------------------
terminate(#state{log = {ok, LogRef}} = State) -> 
    ok = datalog:close(LogRef),
    terminate(State#state{log = closed});
terminate(State) ->
    #{return := Returns} = get(options),
    User_Pid = get(user_pid),
    User_Pid ! {training, return(Returns, State)}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

% --------------------------------------------------------------------
options(#{print:={Each,Size}}=Options, Data, #state{cycle=Cyc}=State)
when Cyc rem Each == 0 -> 
    Report = try
        {LossList, _} = lists:split(Each-1, State#state.lossList),
        LossVal       = lists:sum(LossList) / Each,
        [Cyc, Cyc/Size, "loss:", LossVal]
    catch
        error:badarith -> [Cyc, Cyc/Size, "loss:", "?.???"]
    end,
    Print  = reports:progress_line(2, Report, ?PROGRESS_BAR),
    io:format([Print | "\n"]),
    options(maps:remove(print, Options), Data, State);
options(#{log := LogRef} = Options, Data, State) ->
    datalog:write(LogRef, data2map(Data)),
    options(maps:remove(log, Options), Data, State);
options(_NotResultOptions,_Data,_State) -> 
    ok. 

% --------------------------------------------------------------------
return([prediction | Rest], State) -> 
    [lists:reverse(State#state.predicList) | return(Rest, State)];
return([    errors | Rest], State) -> 
    [lists:reverse(State#state.errorsList) | return(Rest, State)];
return([      loss | Rest], State) -> 
    [lists:reverse(State#state.lossList)   | return(Rest, State)];
return([ bp_errors | Rest], State) -> 
    [lists:reverse(State#state.bperrsList) | return(Rest, State)];
return([],_State) -> 
    [].

% --------------------------------------------------------------------
data2map(Data) -> data2map(Data, #{}).

data2map(#data{    inputs = Val} = D, M) when Val=/=undefined -> 
    data2map(D#data{    inputs = undefined}, M#{    inputs => Val});
data2map(#data{prediction = Val} = D, M) when Val=/=undefined -> 
    data2map(D#data{prediction = undefined}, M#{prediction => Val});
data2map(#data{    optima = Val} = D, M) when Val=/=undefined -> 
    data2map(D#data{    optima = undefined}, M#{    optima => Val});
data2map(#data{    errors = Val} = D, M) when Val=/=undefined -> 
    data2map(D#data{    errors = undefined}, M#{    errors => Val});
data2map(#data{      loss = Val} = D, M) when Val=/=undefined -> 
    data2map(D#data{      loss = undefined}, M#{      loss => Val});
data2map(#data{    bperrs = Val} = D, M) when Val=/=undefined -> 
    data2map(D#data{    bperrs = undefined}, M#{    bperrs => Val});
data2map(                         _D, M)                      -> M.


%%====================================================================
%% Eunit white box tests
%%====================================================================

% --------------------------------------------------------------------
% SPECIFIC SETUP FUNCTIONS -------------------------------------------


% --------------------------------------------------------------------
% TESTS DESCRIPTIONS -------------------------------------------------
white_test_() ->
    % {setup, Where, Setup, Cleanup, Tests | Instantiator}
    [].

% --------------------------------------------------------------------
% ACTUAL TESTS -------------------------------------------------------


% --------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS ------------------------------------------

