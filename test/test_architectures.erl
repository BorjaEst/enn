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

-define(RAND(Max_Units), rand:uniform(Max_Units)).
-define(HIDDEN(N), list_to_atom("hidden" ++ integer_to_list(N))).


%%%===================================================================
%%% Defined Neural Architectures
%%%===================================================================

% -------------------------------------------------------------------
% TODO: Define specs and comments
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
    #{inputs  =>  ?input(2, #{hidden1 => sequential}),
      hidden1 =>  ?dense(4, #{hidden2 => sequential}),
      hidden2 =>  ?dense(3, #{outputs => sequential}),
      outputs => ?output(1, #{})}.

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
    #{inputs  =>  ?input(1, #{hidden1 => sequential}),
      hidden1 =>  ?dense(4, #{outputs => sequential,
                              inputs  => {recurrent, 0.5}}),
      outputs => ?output(1, #{hidden1 => recurrent,
                              inputs  => recurrent})}. 

% -------------------------------------------------------------------
% TODO: Define specs and comments
classification() ->
    _Model = tbd.


