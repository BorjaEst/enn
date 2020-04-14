%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%%
%%%
%%% TODO: Add cortex? Start? End?
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(network).

-export([new/0, new/1, delete/1, info/1]).

-export([add_neuron/1, add_neuron/2, add_neuron/3]).
-export([del_neuron/2, del_neurons/2]).
-export([neuron/2, no_neurons/1, neurons/1]).
-export([source_neurons/1, sink_neurons/1]).

-export([add_conn/3, add_conn/4, add_conn/5]).
-export([del_conn/2, del_conns/2, del_path/3]).
-export([conn/2, no_conns/1, conns/1]).

-export([out_neighbours/2, in_neighbours/2]).
-export([out_conns/2, in_conns/2, conns/2]).
-export([out_degree/2, in_degree/2]).
-export([get_path/3, get_cycle/2]).

-export([get_short_path/3, get_short_cycle/2]).

-export_type([network/0, d_type/0, neuron/0, conn/0, label/0]).

-record(network, {
    ntab = notable   :: ets:tab(),
    ctab = notable   :: ets:tab(),
    gtab = notable   :: ets:tab(),
    recurrent = true :: boolean()
}).

-opaque network() :: #network{}.

-type conn()    :: term().
-type label()   :: term().
-type neuron()  :: term().

-type d_type()  :: 'sequential' | 'recurrent'.

-type add_conn_err_rsn() :: {'bad_conn', Path :: [neuron()]}
                          | {'bad_neuron',  N ::  neuron() }.

-define(NTAB_CONFIGUTATION, [set, public, { read_concurrency,true}]).
-define(CTAB_CONFIGUTATION, [set, public, {write_concurrency,true}]).
-define(GTAB_CONFIGUTATION, [set, public, {write_concurrency,true}]).


%%%===================================================================
%%% API
%%%==================================================================

%%-------------------------------------------------------------------
%% @doc Creates a new network.  
%% @end
%%-------------------------------------------------------------------
-spec new() -> network().
new() -> new(recurrent).

-spec new(Type) -> network() when
      Type :: [d_type()].
new(Type) ->
    case check_type(Type, []) of
    {ok, Ts} ->
        N  = ets:new(   neurons, ?NTAB_CONFIGUTATION),
        C  = ets:new(     conns, ?CTAB_CONFIGUTATION),
        NN = ets:new(neighbours, ?GTAB_CONFIGUTATION),
        ets:insert(NN, [{'$vid', 0}, {'$eid', 0}]),
        set_type(Ts, #network{ntab=N, ctab=C, gtab=NN});
    error ->
        erlang:error(badarg)
    end.

check_type([sequential| Ts], L) -> check_type(Ts, [{recurrent,false} | L]);
check_type([recurrent | Ts], L) -> check_type(Ts, [{recurrent, true} | L]);
check_type(              [], L) -> {ok, L};
check_type(               _, _) -> error.

set_type([{recurrent,X} | Ks], NN) -> set_type(Ks, NN#network{recurrent = X});
set_type(                  [], NN) -> NN.

%%-------------------------------------------------------------------
%% @doc Deletes a network.  
%% @end
%%-------------------------------------------------------------------
-spec delete(NN) -> 'true' when
      NN :: network().
delete(NN) ->
    ets:delete(NN#network.ntab),
    ets:delete(NN#network.ctab),
    ets:delete(NN#network.gtab).

%%-------------------------------------------------------------------
%% @doc Information from the network.  
%% @end
%%-------------------------------------------------------------------
-spec info(NN) -> InfoList when
      NN :: network(),
      InfoList :: [{'type', Type :: d_type()} |
                   {'memory', NoWords :: non_neg_integer()}].
info(NN) ->
    NT = NN#network.ntab,
    ET = NN#network.ctab,
    GT = NN#network.gtab,
    Type = case NN#network.recurrent of
            true  -> recurrent;
            false -> sequential
        end,
    Protection = ets:info(NT, protection),
    Memory = ets:info(NT,memory)+ets:info(ET,memory)+ets:info(GT,memory),
    [{type, Type}, {memory, Memory}, {protection, Protection}].

%%-------------------------------------------------------------------
%% @doc Adds a neuron to the network.  
%%
%% TODO: A neuron must have at least 1 direct input
%% TODO: How to differenciate the direct from recurrent?
%% @end
%%-------------------------------------------------------------------
-spec add_neuron(NN) -> neuron() when
      NN :: network().
add_neuron(NN) ->
    add_neuron(NN, neuron:new()).

-spec add_neuron(NN, N) -> neuron() when
      NN :: network(),
      N  :: neuron().
add_neuron(NN, N) ->
    add_neuron(NN, N, #{}).

-spec add_neuron(NN, N, Label) -> neuron() when
      NN :: network(),
      N  :: neuron(),
      Label :: label().
add_neuron(NN, N, D) ->
    ets:insert(NN#network.ntab, {N, D}),
    N.

%%-------------------------------------------------------------------
%% @doc Deletes a neuron from the network.  
%% @end
%%-------------------------------------------------------------------
-spec del_neuron(NN, N) -> 'true' when
      NN :: network(),
      N  :: neuron().
del_neuron(NN, N) ->
    do_del_nconns(ets:lookup(NN#network.gtab, {in, N}), NN),
    do_del_nconns(ets:lookup(NN#network.gtab, {out, N}), NN),
    % <- Probably delete from mnesia
    ets:delete(NN#network.ntab, N).

-spec del_neurons(NN, Neurons) -> 'true' when
      NN :: network(),
      Neurons :: [neuron()].
del_neurons(NN, [N | Ns]) -> 
    del_neuron(N, NN),
    del_neurons(Ns, NN);
del_neurons([], #network{}) -> true.

%%-------------------------------------------------------------------
%% @doc Returns the neuron with the attached information or false if 
%% the neuron does not belong to that network.  
%% @end
%%-------------------------------------------------------------------
-spec neuron(NN, N) -> {N, Label} | 'false' when
      NN :: network(),
      N :: neuron(),
      Label :: label().
neuron(NN, N) ->
    case ets:lookup(NN#network.ntab, N) of
    []       -> false;
    [Neuron] -> Neuron
    end.

%%-------------------------------------------------------------------
%% @doc Returns the number of neurons of the network.  
%% @end
%%-------------------------------------------------------------------
-spec no_neurons(NN) -> non_neg_integer() when
      NN :: network().
no_neurons(NN) ->
    ets:info(NN#network.ntab, size).

%%-------------------------------------------------------------------
%% @doc Returns a list of all neurons of the network.  
%% @end
%%-------------------------------------------------------------------
-spec neurons(NN) -> Neurons when
      NN :: network(),
      Neurons :: [neuron()].
neurons(NN) ->
    ets:select(NN#network.ntab, [{{'$1', '_'}, [], ['$1']}]).

%%-------------------------------------------------------------------
%% @doc Returns all neurons in the network which have at least one
%% output.  
%% @end
%%-------------------------------------------------------------------
-spec source_neurons(network()) -> [neuron()].
source_neurons(NN) ->
    collect_neurons(NN, in).

%%-------------------------------------------------------------------
%% @doc Returns all neurons in the network which have at least one
%% input.  
%% @end
%%-------------------------------------------------------------------
-spec sink_neurons(network()) -> [neuron()].
sink_neurons(NN) ->
    collect_neurons(NN, out).




-spec in_degree(NN, N) -> non_neg_integer() when
      NN :: network(),
      N :: neuron().

in_degree(NN, N) ->
    length(ets:lookup(NN#network.gtab, {in, N})).

-spec in_neighbours(NN, N) -> Neuron when
      NN :: network(),
      N :: neuron(),
      Neuron :: [neuron()].

in_neighbours(NN, N) ->
    ET = NN#network.ctab,
    GT = NN#network.gtab,
    collect_elems(ets:lookup(GT, {in, N}), ET, 2).

-spec in_conns(NN, N) -> Conns when
      NN :: network(),
      N :: neuron(),
      Conns :: [conn()].

in_conns(NN, N) ->
    ets:select(NN#network.gtab, [{{{in, N}, '$1'}, [], ['$1']}]).

-spec out_degree(NN, N) -> non_neg_integer() when
      NN :: network(),
      N :: neuron().

out_degree(NN, N) ->
    length(ets:lookup(NN#network.gtab, {out, N})).

-spec out_neighbours(NN, N) -> Neurons when
      NN :: network(),
      N :: neuron(),
      Neurons :: [neuron()].

out_neighbours(NN, N) ->
    ET = NN#network.ctab,
    GT = NN#network.gtab,
    collect_elems(ets:lookup(GT, {out, N}), ET, 3).

-spec out_conns(NN, N) -> Conns when
      NN :: network(),
      N :: neuron(),
      Conns :: [conn()].

out_conns(NN, N) ->
    ets:select(NN#network.gtab, [{{{out, N}, '$1'}, [], ['$1']}]).

-spec add_conn(NN, V1, V2) -> conn() | {'error', add_conn_err_rsn()} when
      NN :: network(),
      V1 :: neuron(),
      V2 :: neuron().

add_conn(NN, V1, V2) ->
    do_add_conn({new_conn_id(NN), V1, V2, []}, NN).

-spec add_conn(NN, V1, V2, Label) -> conn() | {'error', add_conn_err_rsn()} when
      NN :: network(),
      V1 :: neuron(),
      V2 :: neuron(),
      Label :: label().

add_conn(NN, V1, V2, D) ->
    do_add_conn({new_conn_id(NN), V1, V2, D}, NN).

-spec add_conn(NN, C, V1, V2, Label) -> conn() | {'error', add_conn_err_rsn()} when
      NN :: network(),
      C :: conn(),
      V1 :: neuron(),
      V2 :: neuron(),
      Label :: label().

add_conn(NN, C, V1, V2, D) ->
    do_add_conn({C, V1, V2, D}, NN).

-spec del_conn(NN, C) -> 'true' when
      NN :: network(),
      C :: conn().

del_conn(NN, C) ->
    do_del_conns([C], NN).

-spec del_conns(NN, Conns) -> 'true' when
      NN :: network(),
      Conns :: [conn()].

del_conns(NN, Es) ->
    do_del_conns(Es, NN).

-spec no_conns(NN) -> non_neg_integer() when
      NN :: network().

no_conns(NN) ->
    ets:info(NN#network.ctab, size).

-spec conns(NN) -> Conns when
      NN :: network(),
      Conns :: [conn()].

conns(NN) ->
    ets:select(NN#network.ctab, [{{'$1', '_', '_', '_'}, [], ['$1']}]).

-spec conns(NN, N) -> Conns when
      NN :: network(),
      N :: neuron(),
      Conns :: [conn()].

conns(NN, N) ->
    ets:select(NN#network.gtab, [{{{out, N},'$1'}, [], ['$1']},
                {{{in, N}, '$1'}, [], ['$1']}]).

-spec conn(NN, C) -> {C, V1, V2, Label} | 'false' when
      NN :: network(),
      C :: conn(),
      V1 :: neuron(),
      V2 :: neuron(),
      Label :: label().

conn(NN, C) ->
    case ets:lookup(NN#network.ctab,C) of
    [] -> false;
    [Conn] -> Conn
    end.

%%
%% Generate a "unique" conn identifier (relative to this network)
%%
-spec new_conn_id(network()) -> conn().

-dialyzer({no_improper_lists, new_conn_id/1}).

new_conn_id(NN) ->
    GT = NN#network.gtab,
    [{'$eid', K}] = ets:lookup(GT, '$eid'),
    true = ets:delete(GT, '$eid'),
    true = ets:insert(GT, {'$eid', K+1}),
    ['$e' | K].

%%
%% Collect elements for a index in a tuple
%%
collect_elems(Keys, Table, Index) ->
    collect_elems(Keys, Table, Index, []).

collect_elems([{_,Key}|Keys], Table, Index, Acc) ->
    collect_elems(Keys, Table, Index,
          [ets:lookup_element(Table, Key, Index)|Acc]);
collect_elems([], _, _, Acc) -> Acc.




%%
%% Delete neurons
%%


do_del_nconns([{_, C}|Ns], NN) ->
    case ets:lookup(NN#network.ctab, C) of
    [{C, V1, V2, _}] ->
        do_del_conn(C, V1, V2, NN),
        do_del_nconns(Ns, NN);
    [] -> % cannot happen
        do_del_nconns(Ns, NN)
    end;
do_del_nconns([], #network{}) -> true.

%%
%% Delete conns
%%
do_del_conns([C|Es], NN) ->
    case ets:lookup(NN#network.ctab, C) of
    [{C,V1,V2,_}] ->
        do_del_conn(C,V1,V2,NN),
        do_del_conns(Es, NN);
    [] ->
        do_del_conns(Es, NN)
    end;
do_del_conns([], #network{}) -> true.

do_del_conn(C, V1, V2, NN) ->
    ets:select_delete(NN#network.gtab, [{{{in, V2}, C}, [], [true]},
                       {{{out,V1}, C}, [], [true]}]),
    ets:delete(NN#network.ctab, C).

-spec rm_conns([neuron(),...], network()) -> 'true'.

rm_conns([V1, V2|Ns], NN) ->
    rm_conn(V1, V2, NN),
    rm_conns([V2|Ns], NN);
rm_conns(_, _) -> true.

-spec rm_conn(neuron(), neuron(), network()) -> 'ok'.

rm_conn(V1, V2, NN) ->
    Es = out_conns(NN, V1),
    rm_conn_0(Es, V1, V2, NN).
    
rm_conn_0([C|Es], V1, V2, NN) ->
    case ets:lookup(NN#network.ctab, C) of
    [{C, V1, V2, _}]  ->
            do_del_conn(C, V1, V2, NN),
        rm_conn_0(Es, V1, V2, NN);
    _ ->
        rm_conn_0(Es, V1, V2, NN)
    end;
rm_conn_0([], _, _, #network{}) -> ok.
    
%%
%% Check that endpoints exist
%%
-spec do_add_conn({conn(), neuron(), neuron(), label()}, network()) ->
    conn() | {'error', add_conn_err_rsn()}.

do_add_conn({C, V1, V2, Label}, NN) ->
    case ets:member(NN#network.ntab, V1) of
    false -> {error, {bad_neuron, V1}};
    true  ->
        case ets:member(NN#network.ntab, V2) of
        false -> {error, {bad_neuron, V2}};
                true ->
                    case other_conn_exists(NN, C, V1, V2) of
                        true -> {error, {bad_conn, [V1, V2]}};
                        false when NN#network.recurrent =:= false ->
                            acyclic_add_conn(C, V1, V2, Label, NN);
                        false ->
                            do_insert_conn(C, V1, V2, Label, NN)
                    end
        end
    end.

other_conn_exists(#network{ctab = ET}, C, V1, V2) ->
    case ets:lookup(ET, C) of
        [{C, Vert1, Vert2, _}] when Vert1 =/= V1; Vert2 =/= V2 ->
            true;
        _ ->
            false
    end.

-spec do_insert_conn(conn(), neuron(), neuron(), label(), network()) -> conn().

do_insert_conn(C, V1, V2, Label, #network{gtab=GT, ctab=ET}) ->
    ets:insert(GT, [{{out, V1}, C}, {{in, V2}, C}]),
    ets:insert(ET, {C, V1, V2, Label}),
    C.

-spec acyclic_add_conn(conn(), neuron(), neuron(), label(), network()) ->
    conn() | {'error', {'bad_conn', [neuron()]}}.

acyclic_add_conn(_E, V1, V2, _L, _G) when V1 =:= V2 ->
    {error, {bad_conn, [V1, V2]}};
acyclic_add_conn(C, V1, V2, Label, NN) ->
    case get_path(NN, V2, V1) of
    false -> do_insert_conn(C, V1, V2, Label, NN);
    Path -> {error, {bad_conn, Path}}
    end.

%%
%% Delete all paths from neuron V1 to neuron V2
%%

-spec del_path(NN, V1, V2) -> 'true' when
      NN :: network(),
      V1 :: neuron(),
      V2 :: neuron().

del_path(NN, V1, V2) ->
    case get_path(NN, V1, V2) of
    false -> true;
    Path ->
        rm_conns(Path, NN),
        del_path(NN, V1, V2)
    end.

%%
%% Find a cycle through N
%% return the cycle as list of neurons [N ... N]
%% if no cycle exists false is returned
%% if only a cycle of length one exists it will be
%% returned as [N] but only after longer cycles have
%% been searched.
%%

-spec get_cycle(NN, N) -> Neurons | 'false' when
      NN :: network(),
      N :: neuron(),
      Neurons :: [neuron(),...].

get_cycle(NN, N) ->
    case one_path(out_neighbours(NN, N), N, [], [N], [N], 2, NN, 1) of
    false ->
        case lists:member(N, out_neighbours(NN, N)) of
        true -> [N];
        false -> false
        end;
    Ns -> Ns
    end.

%%
%% Find a path from V1 to V2
%% return the path as list of neurons [V1 ... V2]
%% if no path exists false is returned
%%

-spec get_path(NN, V1, V2) -> Neurons | 'false' when
      NN :: network(),
      V1 :: neuron(),
      V2 :: neuron(),
      Neurons :: [neuron(),...].

get_path(NN, V1, V2) ->
    one_path(out_neighbours(NN, V1), V2, [], [V1], [V1], 1, NN, 1).

%%
%% prune_short_path (evaluate conditions on path)
%% short : if path is too short
%% ok    : if path is ok
%%
prune_short_path(Counter, Min) when Counter < Min ->
    short;
prune_short_path(_Counter, _Min) ->
    ok.

one_path([W|Ws], W, Cont, Xs, Ps, Prune, NN, Counter) ->
    case prune_short_path(Counter, Prune) of
    short -> one_path(Ws, W, Cont, Xs, Ps, Prune, NN, Counter);
    ok -> lists:reverse([W|Ps])
    end;
one_path([N|Ns], W, Cont, Xs, Ps, Prune, NN, Counter) ->
    case lists:member(N, Xs) of
    true ->  one_path(Ns, W, Cont, Xs, Ps, Prune, NN, Counter);
    false -> one_path(out_neighbours(NN, N), W, 
              [{Ns,Ps} | Cont], [N|Xs], [N|Ps], 
              Prune, NN, Counter+1)
    end;
one_path([], W, [{Ns,Ps}|Cont], Xs, _, Prune, NN, Counter) ->
    one_path(Ns, W, Cont, Xs, Ps, Prune, NN, Counter-1);
one_path([], _, [], _, _, _, _, _Counter) -> false.

%%
%% Like get_cycle/2, but a cycle of length one is preferred.
%%

-spec get_short_cycle(NN, N) -> Neurons | 'false' when
      NN :: network(),
      N :: neuron(),
      Neurons :: [neuron(),...].

get_short_cycle(NN, N) ->
    get_short_path(NN, N, N).

%%
%% Like get_path/3, but using a breadth-first search makes it possible
%% to find a short path.
%%

-spec get_short_path(NN, V1, V2) -> Neurons | 'false' when
      NN :: network(),
      V1 :: neuron(),
      V2 :: neuron(),
      Neurons :: [neuron(),...].

get_short_path(NN, V1, V2) ->
    T = new(),
    add_neuron(T, V1),
    Q = queue:new(),
    Q1 = queue_out_neighbours(V1, NN, Q),
    L = spath(Q1, NN, V2, T),
    delete(T),
    L.
    
spath(Q, NN, Sink, T) ->
    case queue:out(Q) of
    {{value, C}, Q1} ->
        {_E, V1, V2, _Label} = conn(NN, C),
        if 
        Sink =:= V2 ->
            follow_path(V1, T, [V2]);
        true ->
            case neuron(T, V2) of
            false ->
                add_neuron(T, V2),
                add_conn(T, V2, V1),
                NQ = queue_out_neighbours(V2, NN, Q1),
                spath(NQ, NN, Sink, T);
            _V ->
                spath(Q1, NN, Sink, T)
            end
        end;
    {empty, _Q1} ->
        false
    end.

follow_path(N, T, P) ->
    P1 = [N | P],
    case out_neighbours(T, N) of
    [NN] ->
        follow_path(NN, T, P1);
    [] ->
        P1
    end.

queue_out_neighbours(N, NN, Q0) ->
    lists:foldl(fun(C, Q) -> queue:in(C, Q) end, Q0, out_conns(NN, N)).













%%====================================================================
%% Internal functions
%%====================================================================
collect_neurons(NN, Type) ->
    Ns = neurons(NN),
    lists:foldl(fun(N, A) ->
            case ets:member(NN#network.gtab, {Type, N}) of
                true -> A;
                false -> [N|A]
            end
        end, [], Ns).


