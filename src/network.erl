%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
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

-export_type([graph/0, d_type/0, neuron/0, conn/0, label/0]).

-record(network, {
    vtab = notable :: ets:tab(),
    ctab = notable :: ets:tab(),
    ntab = notable :: ets:tab(),
    cyclic = true  :: boolean()
}).

-opaque graph() :: #network{}.

-type conn()    :: term().
-type label()   :: term().
-type neuron()  :: term().

-type add_conn_err_rsn() :: {'bad_conn', Path :: [neuron()]}
                          | {'bad_neuron',  V ::  neuron() }.


%%%===================================================================
%%% API
%%%==================================================================

%%--------------------------------------------------------------------
%% @doc The neuron initialization.  
%% @end
%%-------------------------------------------------------------------
-spec new() -> graph().
new() -> new([]).

-spec new(Type) -> graph() when
      Type :: [d_type()].

new(Type) ->
    case check_type(Type, protected, []) of
    {Access, Ts} ->
        V = ets:new(neurons, [set, Access]),
        C = ets:new(conns, [set, Access]),
        N = ets:new(neighbours, [bag, Access]),
        ets:insert(N, [{'$vid', 0}, {'$eid', 0}]),
        set_type(Ts, #network{vtab=V, ctab=C, ntab=N});
    error ->
        erlang:error(badarg)
    end.

%%
%% Check type of graph
%%
%-spec check_type([d_type()], d_protection(), [{'cyclic', boolean()}]) ->
%           {d_protection(), [{'cyclic', boolean()}]}.

check_type([acyclic|Ts], A, L) ->
    check_type(Ts, A,[{cyclic,false} | L]);
check_type([cyclic | Ts], A, L) ->
    check_type(Ts, A, [{cyclic,true} | L]);
check_type([protected | Ts], _, L) ->
    check_type(Ts, protected, L);
check_type([private | Ts], _, L) ->
    check_type(Ts, private, L);
check_type([], A, L) -> {A, L};
check_type(_, _, _) -> error.

%%
%% Set graph type
%%
-spec set_type([{'cyclic', boolean()}], graph()) -> graph().

set_type([{cyclic,V} | Ks], N) ->
    set_type(Ks, N#network{cyclic = V});
set_type([], N) -> N.


%% Data access functions

-spec delete(N) -> 'true' when
      N :: graph().

delete(N) ->
    ets:delete(N#network.vtab),
    ets:delete(N#network.ctab),
    ets:delete(N#network.ntab).

-spec info(N) -> InfoList when
      N :: graph(),
      InfoList :: [{'cyclicity', Cyclicity :: d_cyclicity()} |
                   {'memory', NoWords :: non_neg_integer()} |
                   {'protection', Protection :: d_protection()}].

info(N) ->
    VT = N#network.vtab,
    ET = N#network.ctab,
    NT = N#network.ntab,
    Cyclicity = case N#network.cyclic of
            true  -> cyclic;
            false -> acyclic
        end,
    Protection = ets:info(VT, protection),
    Memory = ets:info(VT, memory) + ets:info(ET, memory) + ets:info(NT, memory),
    [{cyclicity, Cyclicity}, {memory, Memory}, {protection, Protection}].

-spec add_neuron(N) -> neuron() when
      N :: graph().

add_neuron(N) ->
    do_add_neuron({new_neuron_id(N), []}, N).

-spec add_neuron(N, V) -> neuron() when
      N :: graph(),
      V :: neuron().

add_neuron(N, V) ->
    do_add_neuron({V, []}, N).

-spec add_neuron(N, V, Label) -> neuron() when
      N :: graph(),
      V :: neuron(),
      Label :: label().

add_neuron(N, V, D) ->
    do_add_neuron({V, D}, N).

-spec del_neuron(N, V) -> 'true' when
      N :: graph(),
      V :: neuron().

del_neuron(N, V) ->
    do_del_neuron(V, N).

-spec del_neurons(N, Neurons) -> 'true' when
      N :: graph(),
      Neurons :: [neuron()].

del_neurons(N, Vs) -> 
    do_del_neurons(Vs, N).

-spec neuron(N, V) -> {V, Label} | 'false' when
      N :: graph(),
      V :: neuron(),
      Label :: label().

neuron(N, V) ->
    case ets:lookup(N#network.vtab, V) of
    [] -> false;
    [Neuron] -> Neuron
    end.

-spec no_neurons(N) -> non_neg_integer() when
      N :: graph().

no_neurons(N) ->
    ets:info(N#network.vtab, size).

-spec neurons(N) -> Neurons when
      N :: graph(),
      Neurons :: [neuron()].

neurons(N) ->
    ets:select(N#network.vtab, [{{'$1', '_'}, [], ['$1']}]).

-spec source_neurons(graph()) -> [neuron()].

source_neurons(N) ->
    collect_neurons(N, in).

-spec sink_neurons(graph()) -> [neuron()].

sink_neurons(N) ->
    collect_neurons(N, out).

-spec in_degree(N, V) -> non_neg_integer() when
      N :: graph(),
      V :: neuron().

in_degree(N, V) ->
    length(ets:lookup(N#network.ntab, {in, V})).

-spec in_neighbours(N, V) -> Neuron when
      N :: graph(),
      V :: neuron(),
      Neuron :: [neuron()].

in_neighbours(N, V) ->
    ET = N#network.ctab,
    NT = N#network.ntab,
    collect_elems(ets:lookup(NT, {in, V}), ET, 2).

-spec in_conns(N, V) -> Conns when
      N :: graph(),
      V :: neuron(),
      Conns :: [conn()].

in_conns(N, V) ->
    ets:select(N#network.ntab, [{{{in, V}, '$1'}, [], ['$1']}]).

-spec out_degree(N, V) -> non_neg_integer() when
      N :: graph(),
      V :: neuron().

out_degree(N, V) ->
    length(ets:lookup(N#network.ntab, {out, V})).

-spec out_neighbours(N, V) -> Neurons when
      N :: graph(),
      V :: neuron(),
      Neurons :: [neuron()].

out_neighbours(N, V) ->
    ET = N#network.ctab,
    NT = N#network.ntab,
    collect_elems(ets:lookup(NT, {out, V}), ET, 3).

-spec out_conns(N, V) -> Conns when
      N :: graph(),
      V :: neuron(),
      Conns :: [conn()].

out_conns(N, V) ->
    ets:select(N#network.ntab, [{{{out, V}, '$1'}, [], ['$1']}]).

-spec add_conn(N, V1, V2) -> conn() | {'error', add_conn_err_rsn()} when
      N :: graph(),
      V1 :: neuron(),
      V2 :: neuron().

add_conn(N, V1, V2) ->
    do_add_conn({new_conn_id(N), V1, V2, []}, N).

-spec add_conn(N, V1, V2, Label) -> conn() | {'error', add_conn_err_rsn()} when
      N :: graph(),
      V1 :: neuron(),
      V2 :: neuron(),
      Label :: label().

add_conn(N, V1, V2, D) ->
    do_add_conn({new_conn_id(N), V1, V2, D}, N).

-spec add_conn(N, C, V1, V2, Label) -> conn() | {'error', add_conn_err_rsn()} when
      N :: graph(),
      C :: conn(),
      V1 :: neuron(),
      V2 :: neuron(),
      Label :: label().

add_conn(N, C, V1, V2, D) ->
    do_add_conn({C, V1, V2, D}, N).

-spec del_conn(N, C) -> 'true' when
      N :: graph(),
      C :: conn().

del_conn(N, C) ->
    do_del_conns([C], N).

-spec del_conns(N, Conns) -> 'true' when
      N :: graph(),
      Conns :: [conn()].

del_conns(N, Es) ->
    do_del_conns(Es, N).

-spec no_conns(N) -> non_neg_integer() when
      N :: graph().

no_conns(N) ->
    ets:info(N#network.ctab, size).

-spec conns(N) -> Conns when
      N :: graph(),
      Conns :: [conn()].

conns(N) ->
    ets:select(N#network.ctab, [{{'$1', '_', '_', '_'}, [], ['$1']}]).

-spec conns(N, V) -> Conns when
      N :: graph(),
      V :: neuron(),
      Conns :: [conn()].

conns(N, V) ->
    ets:select(N#network.ntab, [{{{out, V},'$1'}, [], ['$1']},
                {{{in, V}, '$1'}, [], ['$1']}]).

-spec conn(N, C) -> {C, V1, V2, Label} | 'false' when
      N :: graph(),
      C :: conn(),
      V1 :: neuron(),
      V2 :: neuron(),
      Label :: label().

conn(N, C) ->
    case ets:lookup(N#network.ctab,C) of
    [] -> false;
    [Conn] -> Conn
    end.

%%
%% Generate a "unique" conn identifier (relative to this graph)
%%
-spec new_conn_id(graph()) -> conn().

-dialyzer({no_improper_lists, new_conn_id/1}).

new_conn_id(N) ->
    NT = N#network.ntab,
    [{'$eid', K}] = ets:lookup(NT, '$eid'),
    true = ets:delete(NT, '$eid'),
    true = ets:insert(NT, {'$eid', K+1}),
    ['$e' | K].

%%
%% Generate a "unique" neuron identifier (relative to this graph)
%%
-spec new_neuron_id(graph()) -> neuron().

-dialyzer({no_improper_lists, new_neuron_id/1}).

new_neuron_id(N) ->
    NT = N#network.ntab,
    [{'$vid', K}] = ets:lookup(NT, '$vid'),
    true = ets:delete(NT, '$vid'),
    true = ets:insert(NT, {'$vid', K+1}),
    ['$v' | K].

%%
%% Collect elements for a index in a tuple
%%
collect_elems(Keys, Table, Index) ->
    collect_elems(Keys, Table, Index, []).

collect_elems([{_,Key}|Keys], Table, Index, Acc) ->
    collect_elems(Keys, Table, Index,
          [ets:lookup_element(Table, Key, Index)|Acc]);
collect_elems([], _, _, Acc) -> Acc.

-spec do_add_neuron({neuron(), label()}, graph()) -> neuron().

do_add_neuron({V, _Label} = VL, N) ->
    ets:insert(N#network.vtab, VL),
    V.

%%
%% Collect either source or sink neurons.
%%
collect_neurons(N, Type) ->
    Vs = neurons(N),
    lists:foldl(fun(V, A) ->
            case ets:member(N#network.ntab, {Type, V}) of
                true -> A;
                false -> [V|A]
            end
        end, [], Vs).

%%
%% Delete neurons
%%
do_del_neurons([V | Vs], N) ->
    do_del_neuron(V, N),
    do_del_neurons(Vs, N);
do_del_neurons([], #network{}) -> true.

do_del_neuron(V, N) ->
    do_del_nconns(ets:lookup(N#network.ntab, {in, V}), N),
    do_del_nconns(ets:lookup(N#network.ntab, {out, V}), N),
    ets:delete(N#network.vtab, V).

do_del_nconns([{_, C}|Ns], N) ->
    case ets:lookup(N#network.ctab, C) of
    [{C, V1, V2, _}] ->
        do_del_conn(C, V1, V2, N),
        do_del_nconns(Ns, N);
    [] -> % cannot happen
        do_del_nconns(Ns, N)
    end;
do_del_nconns([], #network{}) -> true.

%%
%% Delete conns
%%
do_del_conns([C|Es], N) ->
    case ets:lookup(N#network.ctab, C) of
    [{C,V1,V2,_}] ->
        do_del_conn(C,V1,V2,N),
        do_del_conns(Es, N);
    [] ->
        do_del_conns(Es, N)
    end;
do_del_conns([], #network{}) -> true.

do_del_conn(C, V1, V2, N) ->
    ets:select_delete(N#network.ntab, [{{{in, V2}, C}, [], [true]},
                       {{{out,V1}, C}, [], [true]}]),
    ets:delete(N#network.ctab, C).

-spec rm_conns([neuron(),...], graph()) -> 'true'.

rm_conns([V1, V2|Vs], N) ->
    rm_conn(V1, V2, N),
    rm_conns([V2|Vs], N);
rm_conns(_, _) -> true.

-spec rm_conn(neuron(), neuron(), graph()) -> 'ok'.

rm_conn(V1, V2, N) ->
    Es = out_conns(N, V1),
    rm_conn_0(Es, V1, V2, N).
    
rm_conn_0([C|Es], V1, V2, N) ->
    case ets:lookup(N#network.ctab, C) of
    [{C, V1, V2, _}]  ->
            do_del_conn(C, V1, V2, N),
        rm_conn_0(Es, V1, V2, N);
    _ ->
        rm_conn_0(Es, V1, V2, N)
    end;
rm_conn_0([], _, _, #network{}) -> ok.
    
%%
%% Check that endpoints exist
%%
-spec do_add_conn({conn(), neuron(), neuron(), label()}, graph()) ->
    conn() | {'error', add_conn_err_rsn()}.

do_add_conn({C, V1, V2, Label}, N) ->
    case ets:member(N#network.vtab, V1) of
    false -> {error, {bad_neuron, V1}};
    true  ->
        case ets:member(N#network.vtab, V2) of
        false -> {error, {bad_neuron, V2}};
                true ->
                    case other_conn_exists(N, C, V1, V2) of
                        true -> {error, {bad_conn, [V1, V2]}};
                        false when N#network.cyclic =:= false ->
                            acyclic_add_conn(C, V1, V2, Label, N);
                        false ->
                            do_insert_conn(C, V1, V2, Label, N)
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

-spec do_insert_conn(conn(), neuron(), neuron(), label(), graph()) -> conn().

do_insert_conn(C, V1, V2, Label, #network{ntab=NT, ctab=ET}) ->
    ets:insert(NT, [{{out, V1}, C}, {{in, V2}, C}]),
    ets:insert(ET, {C, V1, V2, Label}),
    C.

-spec acyclic_add_conn(conn(), neuron(), neuron(), label(), graph()) ->
    conn() | {'error', {'bad_conn', [neuron()]}}.

acyclic_add_conn(_E, V1, V2, _L, _G) when V1 =:= V2 ->
    {error, {bad_conn, [V1, V2]}};
acyclic_add_conn(C, V1, V2, Label, N) ->
    case get_path(N, V2, V1) of
    false -> do_insert_conn(C, V1, V2, Label, N);
    Path -> {error, {bad_conn, Path}}
    end.

%%
%% Delete all paths from neuron V1 to neuron V2
%%

-spec del_path(N, V1, V2) -> 'true' when
      N :: graph(),
      V1 :: neuron(),
      V2 :: neuron().

del_path(N, V1, V2) ->
    case get_path(N, V1, V2) of
    false -> true;
    Path ->
        rm_conns(Path, N),
        del_path(N, V1, V2)
    end.

%%
%% Find a cycle through V
%% return the cycle as list of neurons [V ... V]
%% if no cycle exists false is returned
%% if only a cycle of length one exists it will be
%% returned as [V] but only after longer cycles have
%% been searched.
%%

-spec get_cycle(N, V) -> Neurons | 'false' when
      N :: graph(),
      V :: neuron(),
      Neurons :: [neuron(),...].

get_cycle(N, V) ->
    case one_path(out_neighbours(N, V), V, [], [V], [V], 2, N, 1) of
    false ->
        case lists:member(V, out_neighbours(N, V)) of
        true -> [V];
        false -> false
        end;
    Vs -> Vs
    end.

%%
%% Find a path from V1 to V2
%% return the path as list of neurons [V1 ... V2]
%% if no path exists false is returned
%%

-spec get_path(N, V1, V2) -> Neurons | 'false' when
      N :: graph(),
      V1 :: neuron(),
      V2 :: neuron(),
      Neurons :: [neuron(),...].

get_path(N, V1, V2) ->
    one_path(out_neighbours(N, V1), V2, [], [V1], [V1], 1, N, 1).

%%
%% prune_short_path (evaluate conditions on path)
%% short : if path is too short
%% ok    : if path is ok
%%
prune_short_path(Counter, Min) when Counter < Min ->
    short;
prune_short_path(_Counter, _Min) ->
    ok.

one_path([W|Ws], W, Cont, Xs, Ps, Prune, N, Counter) ->
    case prune_short_path(Counter, Prune) of
    short -> one_path(Ws, W, Cont, Xs, Ps, Prune, N, Counter);
    ok -> lists:reverse([W|Ps])
    end;
one_path([V|Vs], W, Cont, Xs, Ps, Prune, N, Counter) ->
    case lists:member(V, Xs) of
    true ->  one_path(Vs, W, Cont, Xs, Ps, Prune, N, Counter);
    false -> one_path(out_neighbours(N, V), W, 
              [{Vs,Ps} | Cont], [V|Xs], [V|Ps], 
              Prune, N, Counter+1)
    end;
one_path([], W, [{Vs,Ps}|Cont], Xs, _, Prune, N, Counter) ->
    one_path(Vs, W, Cont, Xs, Ps, Prune, N, Counter-1);
one_path([], _, [], _, _, _, _, _Counter) -> false.

%%
%% Like get_cycle/2, but a cycle of length one is preferred.
%%

-spec get_short_cycle(N, V) -> Neurons | 'false' when
      N :: graph(),
      V :: neuron(),
      Neurons :: [neuron(),...].

get_short_cycle(N, V) ->
    get_short_path(N, V, V).

%%
%% Like get_path/3, but using a breadth-first search makes it possible
%% to find a short path.
%%

-spec get_short_path(N, V1, V2) -> Neurons | 'false' when
      N :: graph(),
      V1 :: neuron(),
      V2 :: neuron(),
      Neurons :: [neuron(),...].

get_short_path(N, V1, V2) ->
    T = new(),
    add_neuron(T, V1),
    Q = queue:new(),
    Q1 = queue_out_neighbours(V1, N, Q),
    L = spath(Q1, N, V2, T),
    delete(T),
    L.
    
spath(Q, N, Sink, T) ->
    case queue:out(Q) of
    {{value, C}, Q1} ->
        {_E, V1, V2, _Label} = conn(N, C),
        if 
        Sink =:= V2 ->
            follow_path(V1, T, [V2]);
        true ->
            case neuron(T, V2) of
            false ->
                add_neuron(T, V2),
                add_conn(T, V2, V1),
                NQ = queue_out_neighbours(V2, N, Q1),
                spath(NQ, N, Sink, T);
            _V ->
                spath(Q1, N, Sink, T)
            end
        end;
    {empty, _Q1} ->
        false
    end.

follow_path(V, T, P) ->
    P1 = [V | P],
    case out_neighbours(T, V) of
    [N] ->
        follow_path(N, T, P1);
    [] ->
        P1
    end.

queue_out_neighbours(V, N, Q0) ->
    lists:foldl(fun(C, Q) -> queue:in(C, Q) end, Q0, out_conns(N, V)).
