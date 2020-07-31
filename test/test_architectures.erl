%%%-------------------------------------------------------------------
%%% @author borja
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Feb 2019 11:56
%%%-------------------------------------------------------------------
-module(test_architectures).
-compile([export_all, nowarn_export_all]). %%TODO: To delete after build

-include_lib("layers.hrl").

%% Defined agent species
-export([]).

-define(MAX_UNITS_PER_LAYER,   20).
-define(MAX_NUMBER_LAYERS,      4).
-define(RAND(Max_Units), rand:uniform(Max_Units)).
-define(HIDDEN(N), list_to_atom("hidden" ++ integer_to_list(N))).


%%%===================================================================
%%% Defined Neural Architectures
%%%===================================================================

% -------------------------------------------------------------------
% TODO: Define specs and comments
random_dense() -> 
    random_dense(?MAX_UNITS_PER_LAYER, ?MAX_NUMBER_LAYERS). 

random_dense(Max_Units, Max_Layers) -> 
    M0 = #{outputs => ?output(?RAND(Max_Units), #{})},
    random_dense(?RAND(Max_Layers), Max_Units, M0, outputs).

random_dense(N, Max_Units, M0, To) when N > 0 -> 
    M1 = M0#{?HIDDEN(N) => ?dense(?RAND(Max_Units), #{To => sequential})},
    random_dense(N-1, Max_Units, M1, ?HIDDEN(N));
random_dense(0, Max_Units, Mx, To) -> 
    Mx#{inputs => ?input(?RAND(Max_Units), #{To => sequential})}.

% -------------------------------------------------------------------
% TODO: Define specs and comments
example() ->
    #{inputs  =>   ?input(2, #{hidden1 => sequential}),
      hidden1 => ?sigmoid(4, #{hidden2 => sequential}),
      hidden2 =>   ?dense(3, #{outputs => sequential}),
      outputs =>  ?output(1, #{})}.

% -------------------------------------------------------------------
% TODO: Define specs and comments
xor_gate() ->
    #{inputs  =>  ?input(2, #{hidden1 => sequential}),
      hidden1 =>    ?elu(2, #{outputs => sequential}),
      outputs => ?output(1, #{})}.

% -------------------------------------------------------------------
% TODO: Define specs and comments
addition() ->
    #{inputs  =>  ?input(2, #{outputs => sequential}),
      outputs => ?output(1, #{})}.

% -------------------------------------------------------------------
% TODO: Define specs and comments
multiplication() ->
    CustomOutput = layer:create_type(#{aggregation => product,
                                       activation  => direct}), 
    #{inputs  => ?input(2, #{outputs => sequential}),
      outputs => CustomOutput(1, #{})}.

% -------------------------------------------------------------------
% TODO: Define specs and comments
recurrent() ->
    #{inputs  =>   ?input(1, #{hidden1 => sequential}),
      hidden1 => ?sigmoid(4, #{outputs => sequential,
                               inputs  => {recurrent, 0.5}}),
      outputs =>  ?output(1, #{hidden1 => recurrent,
                               inputs  => recurrent})}. 

% -------------------------------------------------------------------
% TODO: Define specs and comments
classification() ->
    _Model = tbd.

% -------------------------------------------------------------------
% TODO: Define specs and comments
broken_connections() ->
    #{inputs  =>  ?input(2, #{hidden1 => sequential}),
      hidden1 =>  ?dense(2, #{}),
      hidden2 =>  ?dense(2, #{outputs => sequential}),                  
      outputs => ?output(2, #{})}. 

% -------------------------------------------------------------------
% TODO: Define specs and comments
infinite_loop() ->
    #{inputs  =>  ?input(2, #{outputs => sequential}),
      hidden1 =>  ?dense(2, #{hidden2 => sequential}),
      hidden2 =>  ?dense(2, #{hidden1 => sequential}),                  
      outputs => ?output(1, #{})}.

% -------------------------------------------------------------------
% TODO: Define specs and comments
dummy_neurons() ->
    #{inputs  =>  ?input(2, #{outputs => sequential,
                              hidden1 => sequential}),
      hidden1 =>  ?dense(2, #{}),
      hidden2 =>  ?dense(2, #{outputs => sequential}),                  
      outputs => ?output(1, #{})}.

% -------------------------------------------------------------------
% TODO: Define specs and comments
shuffle_connections(Id, N) -> 
    [shuffle_connections(Id) || _ <- lists:seq(1, N)],
    ok.

shuffle_connections(Id) -> 
    {atomic, _} = mnesia:transaction(
        fun() -> 
            NNodes = nnet:nodes(Id),
            Size_f = math:sqrt(map_size(NNodes)),
            ToDisconnect = rcomb(
                [{N1,N2} || N1 <- maps:keys(NNodes), 
                            N2 <- maps:keys(NNodes)],
                1.0/Size_f),
            nnet:disconnect(ToDisconnect),
            ToReconnect = rcomb(
                [{N1,N2} || N1 <- maps:keys(NNodes), 
                            N2 <- maps:keys(NNodes)],
                1.0/Size_f),
            nnet:connect(ToReconnect)
        end
    ), ok.

% Returns a list of random combinations -----------------------------
rcomb(List, X) -> 
    lists:filter(fun(_) -> rand:uniform() < X end, List).

