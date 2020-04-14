%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%%
%%%
%%% TODO: Add cortex? Start? End?
%%% TODO: States -> unmounted (lists); -> mounted (ETS, Global read)
%%%              -> Everyone can edit; -> Only contex can write (concurrent read optimised)
%%%
%%% TODO: Weights information are stored in MNESIA so each neuron can individually
%%%       update its weight values {{FromID, ToID, connection}, W}.
%%% @end
%%%-------------------------------------------------------------------
-module(network).

-export([new/0, new/1, delete/1, info/1]).

-export([add_neuron/1, add_neuron/2, del_neuron/2]).
-export([neuron/2, no_neurons/1, neurons/1]).
-export([source_neurons/1, sink_neurons/1]).

-export([add_conn/3]).
-export([del_conn/2, del_conns/2, del_path/3]).
-export([conn/2, no_conns/1, conns/1]).

-export([out_neighbours/2, in_neighbours/2]).
-export([out_conn/2, in_conn/2, conns/2]).
-export([out_degree/2, in_degree/2]).
-export([get_path/3, get_cycle/2]).

-export([get_short_path/3, get_short_cycle/2]).

-export_type([network/0, d_type/0, neuron/0, conn/0, label/0]).

-record(network, {
    ntab = notable   :: ets:tab(),
    ctab = notable   :: ets:tab(),
    dtab = notable   :: ets:tab(),
    rtab = norable   :: ets:tab(),
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
-define(DTAB_CONFIGUTATION, [set, public, {write_concurrency,true}]).
-define(RTAB_CONFIGUTATION, [set, public, {write_concurrency,true}]).


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
        NT = ets:new(  neurons, ?NTAB_CONFIGUTATION),
        CT = ets:new(    conns, ?CTAB_CONFIGUTATION),
        DT = ets:new(   direct, ?DTAB_CONFIGUTATION),
        RT = ets:new(recurrent, ?RTAB_CONFIGUTATION),
        ets:insert(NT, [{'$start', 0}, {'$end', 0}]),
        set_type(Ts, #network{ntab=NT, ctab=CT, dtab=DT, rtab=RT});
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
    ets:delete(NN#network.dtab),
    ets:delete(NN#network.rtab).

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
    CT = NN#network.ctab,
    DT = NN#network.dtab,
    RT = NN#network.rtab,
    Type = case NN#network.recurrent of
            true  -> recurrent;
            false -> sequential
        end,
    Protection = ets:info(NT, protection),
    Memory = ets:info(NT,memory) + ets:info(CT,memory) + 
             ets:info(DT,memory) + ets:info(RT,memory),
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
    ets:insert(NN#network.ntab, {N, []}),
    N.

%%-------------------------------------------------------------------
%% @doc Deletes a neuron from the network.  
%% @end
%%-------------------------------------------------------------------
-spec del_neuron(NN, N) -> 'true' when
      NN :: network(),
      N  :: neuron().
del_neuron(NN, N) ->
    do_del_nconns(ets:lookup(NN#network.dtab, {in, N}), NN),
    do_del_nconns(ets:lookup(NN#network.rtab, {in, N}), NN),
    do_del_nconns(ets:lookup(NN#network.dtab, {out, N}), NN),
    do_del_nconns(ets:lookup(NN#network.rtab, {out, N}), NN),
    % <- Probably delete from mnesia
    ets:delete(NN#network.ntab, N).

%%-------------------------------------------------------------------
%% @doc Returns the neuron with the attached information or false if 
%% the neuron does not belong to that network.  
%% @end
%%-------------------------------------------------------------------
-spec neuron(NN, N) -> {N, Label} | 'false' when
      NN :: network(),
      N  :: neuron(),
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
%% input.  
%% @end
%%-------------------------------------------------------------------
-spec sink_neurons(NN) -> Neurons when
      NN :: network(),
      Neurons :: [neuron()].
sink_neurons(NN) ->
    sets:to_list(sets:from_list(
        sink_neurons(NN, sequential),
        sink_neurons(NN,  recurrent)
    )).

-spec sink_neurons(NN, Type) -> Neurons when
      NN      :: network(),
      Type    :: d_type(),
      Neurons :: [neuron()].
sink_neurons(NN, Type) ->
    collect_neurons(NN, Type, out).

%%-------------------------------------------------------------------
%% @doc Returns all neurons in the network which have at least one
%% output.  
%% @end
%%-------------------------------------------------------------------
-spec source_neurons(NN) -> Neurons when
      NN :: network(),
      Neurons :: [neuron()].
source_neurons(NN) ->
    sets:to_list(sets:from_list(
        source_neurons(NN, sequential),
        source_neurons(NN,  recurrent)
    )).

-spec source_neurons(NN, Type) -> Neurons when
      NN      :: network(),
      Type    :: d_type(),
      Neurons :: [neuron()].
source_neurons(NN, Type) ->
    collect_neurons(NN, Type, in).

%%-------------------------------------------------------------------
%% @doc Returns the in-degree of neuron N of network NN.  
%% @end
%%-------------------------------------------------------------------
-spec in_degree(NN, N) -> non_neg_integer() when
      NN :: network(),
      N  :: neuron().
in_degree(NN, N) ->
    length(ets:lookup(NN#network.dtab, {in, N})) +
    length(ets:lookup(NN#network.rtab, {in, N})).

%%-------------------------------------------------------------------
%% @doc Returns the out-degree of neuron N of network NN.  
%% @end
%%-------------------------------------------------------------------
-spec out_degree(NN, N) -> non_neg_integer() when
      NN :: network(),
      N  :: neuron().
out_degree(NN, N) ->
    length(ets:lookup(NN#network.dtab, {out, N})) + 
    length(ets:lookup(NN#network.rtab, {out, N})).

%%-------------------------------------------------------------------
%% @doc Returns a list of all in-neighbors of N of network NN. 
%% @end
%%-------------------------------------------------------------------
-spec in_neighbours(NN, N) -> Neurons when
      NN :: network(),
      N  :: neuron(),
      Neurons :: [neuron()].
in_neighbours(NN, N) ->
    CT = NN#network.ctab,
    DT = NN#network.dtab,
    RT = NN#network.rtab,
    collect_elems(ets:lookup(DT, {in, N}), CT, 2) ++ 
    collect_elems(ets:lookup(RT, {in, N}), CT, 2).

%%-------------------------------------------------------------------
%% @doc Returns a list of all out-neighbors of N of network NN. 
%% @end
%%-------------------------------------------------------------------
-spec out_neighbours(NN, N) -> Neurons when
      NN :: network(),
      N  :: neuron(),
      Neurons :: [neuron()].
out_neighbours(NN, N) ->
    CT = NN#network.ctab,
    DT = NN#network.dtab,
    RT = NN#network.rtab,
    collect_elems(ets:lookup(DT, {out, N}), CT, 3) ++
    collect_elems(ets:lookup(RT, {out, N}), CT, 3).

%%-------------------------------------------------------------------
%% @doc Returns all connections incident on N of network NN. 
%% @end
%%-------------------------------------------------------------------
-spec in_conn(NN, N) -> Connections when
      NN :: network(),
      N  :: neuron(),
      Connections :: [conn()].
in_conn(NN, N) ->
    in_conn(NN, N, sequential) ++ in_conn(NN, N, recurrent).

-spec in_conn(NN, N, Type) -> Connections when
      NN   :: network(),
      N    :: neuron(),
      Type :: d_type(),
      Connections :: [conn()].
in_conn(NN, N, Type) ->
    case Type of
        sequential -> 
            ets:select(NN#network.dtab,[{{{ in,N},'$1'},[],['$1']}]);
        recurrent  -> 
            ets:select(NN#network.rtab,[{{{ in,N},'$1'},[],['$1']}])
    end.

%%-------------------------------------------------------------------
%% @doc Returns all connections emanating from N of network NN. 
%% @end
%%-------------------------------------------------------------------
-spec out_conn(NN, N) -> Connections when
      NN :: network(),
      N  :: neuron(),
      Connections :: [conn()].
out_conn(NN, N) ->
    out_conn(NN, N, sequential) ++ out_conn(NN, N, recurrent).

-spec out_conn(NN, N, Type) -> Connections when
      NN   :: network(),
      N    :: neuron(),
      Type :: d_type(),
      Connections :: [conn()].
out_conn(NN, N, Type) ->
    case Type of
        sequential -> 
            ets:select(NN#network.dtab,[{{{out,N},'$1'},[],['$1']}]);
        recurrent  -> 
            ets:select(NN#network.rtab,[{{{out,N},'$1'},[],['$1']}])
    end.

%%-------------------------------------------------------------------
%% @doc Creates (or modifies) a connection betweeb N1 and N2. 
%% @end
%%------------------------------------------------------------------
-spec add_conn(NN, N1, N2) -> Result when
      NN :: network(),
      N1 :: neuron(),
      N2 :: neuron(),
      Result :: conn() | {'error', add_conn_err_rsn()}.
add_conn(NN, N1, N2) ->
    case check_neurons(NN, [N1, N2]) of 
    {error, Reason} -> {error, Reason};
    ok              -> insert_conn(NN, N1, N2)
    end.

insert_conn(NN, N1, N2) when N1 =:= N2 ->
    insert_rcc_conn(NN, N1, N2, [N1,N2]);
insert_conn(NN, N1, N2) ->
    case get_path(NN, N2, N1) of
        false -> insert_seq_conn(NN, N1, N2);
        Path  -> insert_rcc_conn(NN, N1, N2, Path)
    end.

insert_seq_conn(#network{dtab=DT, ctab=CT}, N1, N2) ->
    Id = connection:new(N1, N2),
    ets:insert(DT, [{{out, N1}, Id}, {{in, N2}, Id}]),
    ets:insert(CT, {Id, N1, N2, []}),
    Id.

insert_rcc_conn(#network{recurrent=false}, _, _, Path) ->
    {error, {bad_conn, Path}};
insert_rcc_conn(#network{rtab=RT, ctab=CT}, N1, N2, _) ->
    Id = connection:new(N1, N2),
    ets:insert(RT, [{{out, N1}, Id}, {{in, N2}, Id}]),
    ets:insert(CT, {Id, N1, N2, []}),
    Id.

%%-------------------------------------------------------------------
%% @doc Creates (or modifies) a connection betweeb N1 and N2. 
%% @end
%%------------------------------------------------------------------
-spec del_conn(NN, N1, N2) -> Result when
      NN :: network(),
      N1 :: neuron(),
      N2 :: neuron(),
      Result :: conn() | {'error', add_conn_err_rsn()}.













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
    ets:select(NN#network.dtab, [{{{out, N},'$1'}, [], ['$1']},
                {{{in, N}, '$1'}, [], ['$1']}]).

-spec conn(NN, C) -> {C, N1, N2, Label} | 'false' when
      NN :: network(),
      C :: conn(),
      N1 :: neuron(),
      N2 :: neuron(),
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
    DT = NN#network.dtab,
    [{'$eid', K}] = ets:lookup(DT, '$eid'),
    true = ets:delete(DT, '$eid'),
    true = ets:insert(DT, {'$eid', K+1}),
    ['$e' | K].





%%
%% Delete neurons
%%


do_del_nconns([{_, C}|Ns], NN) ->
    case ets:lookup(NN#network.ctab, C) of
    [{C, N1, N2, _}] ->
        do_del_conn(C, N1, N2, NN),
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
    [{C,N1,N2,_}] ->
        do_del_conn(C,N1,N2,NN),
        do_del_conns(Es, NN);
    [] ->
        do_del_conns(Es, NN)
    end;
do_del_conns([], #network{}) -> true.

do_del_conn(C, N1, N2, NN) ->
    ets:select_delete(NN#network.dtab, [{{{in, N2}, C}, [], [true]},
                       {{{out,N1}, C}, [], [true]}]),
    ets:delete(NN#network.ctab, C).

-spec rm_conns([neuron(),...], network()) -> 'true'.

rm_conns([N1, N2|Ns], NN) ->
    rm_conn(N1, N2, NN),
    rm_conns([N2|Ns], NN);
rm_conns(_, _) -> true.

-spec rm_conn(neuron(), neuron(), network()) -> 'ok'.

rm_conn(N1, N2, NN) ->
    Es = out_conn(NN, N1),
    rm_conn_0(Es, N1, N2, NN).
    
rm_conn_0([C|Es], N1, N2, NN) ->
    case ets:lookup(NN#network.ctab, C) of
    [{C, N1, N2, _}]  ->
            do_del_conn(C, N1, N2, NN),
        rm_conn_0(Es, N1, N2, NN);
    _ ->
        rm_conn_0(Es, N1, N2, NN)
    end;
rm_conn_0([], _, _, #network{}) -> ok.
    


%%
%% Delete all paths from neuron N1 to neuron N2
%%

-spec del_path(NN, N1, N2) -> 'true' when
      NN :: network(),
      N1 :: neuron(),
      N2 :: neuron().

del_path(NN, N1, N2) ->
    case get_path(NN, N1, N2) of
    false -> true;
    Path ->
        rm_conns(Path, NN),
        del_path(NN, N1, N2)
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
%% Find a path from N1 to N2
%% return the path as list of neurons [N1 ... N2]
%% if no path exists false is returned
%%

-spec get_path(NN, N1, N2) -> Neurons | 'false' when
      NN :: network(),
      N1 :: neuron(),
      N2 :: neuron(),
      Neurons :: [neuron(),...].

get_path(NN, N1, N2) ->
    one_path(out_neighbours(NN, N1), N2, [], [N1], [N1], 1, NN, 1).

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

-spec get_short_path(NN, N1, N2) -> Neurons | 'false' when
      NN :: network(),
      N1 :: neuron(),
      N2 :: neuron(),
      Neurons :: [neuron(),...].

get_short_path(NN, N1, N2) ->
    T = new(),
    add_neuron(T, N1),
    Q = queue:new(),
    Q1 = queue_out_neighbours(N1, NN, Q),
    L = spath(Q1, NN, N2, T),
    delete(T),
    L.
    
spath(Q, NN, Sink, T) ->
    case queue:out(Q) of
    {{value, C}, Q1} ->
        {_E, N1, N2, _Label} = conn(NN, C),
        if 
        Sink =:= N2 ->
            follow_path(N1, T, [N2]);
        true ->
            case neuron(T, N2) of
            false ->
                add_neuron(T, N2),
                add_conn(T, N2, N1),
                NQ = queue_out_neighbours(N2, NN, Q1),
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
    lists:foldl(fun(C, Q) -> queue:in(C, Q) end, Q0, out_conn(NN, N)).













%%====================================================================
%% Internal functions
%%====================================================================

% Cheks if the neuron belongs to the ntab ----------------------------
check_neurons(NN, [N | Neurons]) -> 
    case ets:member(NN#network.ntab, N) of
        false -> {error, {bad_neuron, N}};
        true  -> check_neurons(NN, Neurons)
    end;
check_neurons(_NN, []) -> 
    ok.

% Collects the neurons with a type (d_type) and direction (in|out) --
collect_neurons(NN, Type, Direction) -> 
    Ns = neurons(NN),
    case Type of 
        sequential -> filter_neurons(Ns, NN#network.dtab, Direction);
        recurrent  -> filter_neurons(Ns, NN#network.rtab, Direction)
    end.

% Filters the neurons with a specific direction from a table --------
filter_neurons(Ns, Table, Direction) ->
    lists:foldl(fun(N, A) ->
            case ets:member(Table, {Direction, N}) of
                true -> A;
                false -> [N|A]
            end
        end, [], Ns).

%% Collect elements for a index in a tuple --------------------------
collect_elems(Keys, Table, Index) ->
    collect_elems(Keys, Table, Index, []).

collect_elems([{_,Key}|Keys], Table, Index, Acc) ->
    collect_elems(Keys, Table, Index,
          [ets:lookup_element(Table, Key, Index)|Acc]);
collect_elems([], _, _, Acc) -> Acc.

% True if the id is already used in a different connection ----------
other_conn_exists(#network{ctab = CT}, C, N1, N2) ->
    case ets:lookup(CT, C) of
        [{C,V1,V2,_}] when V1=/=N1; V2=/=N2 -> true;
        _                                   -> false
    end.

