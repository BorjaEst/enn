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
-module(netgraph).

-export([new/0, new/1, delete/1, info/1]).

-export([add_neuron/1, add_neuron/2, del_neuron/2]).
-export([no_neurons/1, neurons/1]).
-export([source_neurons/1, source_neurons/2]).
-export([sink_neurons/1, sink_neurons/2]).

-export([add_conn/3, del_conn/3]).
-export([no_conn/1, conn/1, conn/2]).

-export([out_neighbours/2, in_neighbours/2]).
-export([out_conn/2, in_conn/2]).
-export([out_degree/2, in_degree/2]).

-export_type([netgraph/0, d_type/0, neuron/0, conn/0]).

-record(netgraph, {
    ntab = notable   :: ets:tab(),
    ctab = notable   :: ets:tab(),
    dtab = notable   :: ets:tab(),
    rtab = norable   :: ets:tab(),
    recurrent = true :: boolean()
}).
-type netgraph() :: #netgraph{}.
-type conn()    :: term().
-type neuron()  :: term().
-type d_type()  :: 'sequential' | 'recurrent'.

-type add_conn_err_rsn() :: {'bad_conn', Path :: [neuron()]}
                          | {'bad_neuron',  N ::  neuron() }.

-define(NTAB_CONFIGUTATION, [set,protected,{read_concurrency,true}]).
-define(CTAB_CONFIGUTATION, [set,protected,{read_concurrency,true}]).
-define(DTAB_CONFIGUTATION, [set,protected,{read_concurrency,true}]).
-define(RTAB_CONFIGUTATION, [set,protected,{read_concurrency,true}]).


%%%===================================================================
%%% API
%%%===================================================================

%%-------------------------------------------------------------------
%% @doc Creates a new netgraph.  
%% @end
%%-------------------------------------------------------------------
-spec new() -> netgraph().
new() -> new(recurrent).

-spec new(Type) -> netgraph() when
      Type :: [d_type()].
new(Type) ->
    case check_type(Type, []) of
    {ok, Ts} ->
        NT = ets:new(  neurons, ?NTAB_CONFIGUTATION),
        CT = ets:new(    conns, ?CTAB_CONFIGUTATION),
        DT = ets:new(   direct, ?DTAB_CONFIGUTATION),
        RT = ets:new(recurrent, ?RTAB_CONFIGUTATION),
        ets:insert(NT, [{'$start', 0}, {'$end', 0}]),
        set_type(Ts, #netgraph{ntab=NT, ctab=CT, dtab=DT, rtab=RT});
    error ->
        erlang:error(badarg)
    end.

check_type([sequential| Ts], L) -> check_type(Ts, [{recurrent,false} | L]);
check_type([recurrent | Ts], L) -> check_type(Ts, [{recurrent, true} | L]);
check_type(              [], L) -> {ok, L};
check_type(               _, _) -> error.

set_type([{recurrent,X} | Ks], NN) -> set_type(Ks, NN#netgraph{recurrent = X});
set_type(                  [], NN) -> NN.

%%-------------------------------------------------------------------
%% @doc Deletes a netgraph.  
%% @end
%%-------------------------------------------------------------------
-spec delete(NN) -> 'true' when
      NN :: netgraph().
delete(NN) ->
    ets:delete(NN#netgraph.ntab),
    ets:delete(NN#netgraph.ctab),
    ets:delete(NN#netgraph.dtab),
    ets:delete(NN#netgraph.rtab).

%%-------------------------------------------------------------------
%% @doc Information from the netgraph.  
%% @end
%%-------------------------------------------------------------------
-spec info(NN) -> InfoList when
      NN :: netgraph(),
      InfoList :: [{'type', Type :: d_type()} |
                   {'memory', NoWords :: non_neg_integer()}].
info(NN) ->
    NT = NN#netgraph.ntab,
    CT = NN#netgraph.ctab,
    DT = NN#netgraph.dtab,
    RT = NN#netgraph.rtab,
    Type = case NN#netgraph.recurrent of
            true  -> recurrent;
            false -> sequential
        end,
    Protection = ets:info(NT, protection),
    Memory = ets:info(NT,memory) + ets:info(CT,memory) + 
             ets:info(DT,memory) + ets:info(RT,memory),
    [{type, Type}, {memory, Memory}, {protection, Protection}].

%%-------------------------------------------------------------------
%% @doc Adds a neuron to the netgraph.  
%%
%% TODO: A neuron must have at least 1 direct input
%% TODO: How to differenciate the direct from recurrent?
%% @end
%%-------------------------------------------------------------------
-spec add_neuron(NN) -> neuron() when
      NN :: netgraph().
add_neuron(NN) ->
    add_neuron(NN, neuron:new()).

-spec add_neuron(NN, N) -> neuron() when
      NN :: netgraph(),
      N  :: neuron().
add_neuron(NN, N) ->
    ets:insert(NN#netgraph.ntab, {N, []}),
    N.

%%-------------------------------------------------------------------
%% @doc Deletes a neuron from the netgraph.  
%% @end
%%-------------------------------------------------------------------
-spec del_neuron(NN, N) -> 'true' when
      NN :: netgraph(),
      N  :: neuron().
del_neuron(NN, N1) ->
    [del_conn(NN, N1, N2) || N2 <- out_neighbours(NN, N1)],
    [del_conn(NN, N2, N1) || N2 <-  in_neighbours(NN, N1)],
    neuron:delete(N1),
    ets:delete(NN#netgraph.ntab, N1).

%%-------------------------------------------------------------------
%% @doc Returns the number of neurons of the netgraph.  
%% @end
%%-------------------------------------------------------------------
-spec no_neurons(NN) -> non_neg_integer() when
      NN :: netgraph().
no_neurons(NN) ->
    ets:info(NN#netgraph.ntab, size).

%%-------------------------------------------------------------------
%% @doc Returns a list of all neurons of the netgraph.  
%% @end
%%-------------------------------------------------------------------
-spec neurons(NN) -> Neurons when
      NN :: netgraph(),
      Neurons :: [neuron()].
neurons(NN) ->
    ets:select(NN#netgraph.ntab, [{{'$1', '_'}, [], ['$1']}]).

%%-------------------------------------------------------------------
%% @doc Returns all neurons in the netgraph which have at least one
%% input.  
%% @end
%%-------------------------------------------------------------------
-spec sink_neurons(NN) -> Neurons when
      NN :: netgraph(),
      Neurons :: [neuron()].
sink_neurons(NN) ->
    sets:to_list(sets:from_list(
        sink_neurons(NN, sequential),
        sink_neurons(NN,  recurrent)
    )).

-spec sink_neurons(NN, Type) -> Neurons when
      NN      :: netgraph(),
      Type    :: d_type(),
      Neurons :: [neuron()].
sink_neurons(NN, Type) ->
    collect_neurons(NN, Type, out).

%%-------------------------------------------------------------------
%% @doc Returns all neurons in the netgraph which have at least one
%% output.  
%% @end
%%-------------------------------------------------------------------
-spec source_neurons(NN) -> Neurons when
      NN :: netgraph(),
      Neurons :: [neuron()].
source_neurons(NN) ->
    sets:to_list(sets:from_list(
        source_neurons(NN, sequential),
        source_neurons(NN,  recurrent)
    )).

-spec source_neurons(NN, Type) -> Neurons when
      NN      :: netgraph(),
      Type    :: d_type(),
      Neurons :: [neuron()].
source_neurons(NN, Type) ->
    collect_neurons(NN, Type, in).

%%-------------------------------------------------------------------
%% @doc Returns the in-degree of neuron N of netgraph NN.  
%% @end
%%-------------------------------------------------------------------
-spec in_degree(NN, N) -> non_neg_integer() when
      NN :: netgraph(),
      N  :: neuron().
in_degree(NN, N) ->
    length(ets:lookup(NN#netgraph.dtab, {in, N})) +
    length(ets:lookup(NN#netgraph.rtab, {in, N})).

%%-------------------------------------------------------------------
%% @doc Returns the out-degree of neuron N of netgraph NN.  
%% @end
%%-------------------------------------------------------------------
-spec out_degree(NN, N) -> non_neg_integer() when
      NN :: netgraph(),
      N  :: neuron().
out_degree(NN, N) ->
    length(ets:lookup(NN#netgraph.dtab, {out, N})) + 
    length(ets:lookup(NN#netgraph.rtab, {out, N})).

%%-------------------------------------------------------------------
%% @doc Returns a list of all in-neighbors of N of netgraph NN. 
%% @end
%%-------------------------------------------------------------------
-spec in_neighbours(NN, N) -> Neurons when
      NN :: netgraph(),
      N  :: neuron(),
      Neurons :: [neuron()].
in_neighbours(NN, N) ->
    CT = NN#netgraph.ctab,
    DT = NN#netgraph.dtab,
    RT = NN#netgraph.rtab,
    collect_elems(ets:lookup(DT, {in, N}), CT, 2) ++ 
    collect_elems(ets:lookup(RT, {in, N}), CT, 2).

%%-------------------------------------------------------------------
%% @doc Returns a list of all out-neighbors of N of netgraph NN. 
%% @end
%%-------------------------------------------------------------------
-spec out_neighbours(NN, N) -> Neurons when
      NN :: netgraph(),
      N  :: neuron(),
      Neurons :: [neuron()].
out_neighbours(NN, N) ->
    CT = NN#netgraph.ctab,
    DT = NN#netgraph.dtab,
    RT = NN#netgraph.rtab,
    collect_elems(ets:lookup(DT, {out, N}), CT, 3) ++
    collect_elems(ets:lookup(RT, {out, N}), CT, 3).

%%-------------------------------------------------------------------
%% @doc Returns all connections incident on N of netgraph NN. 
%% @end
%%-------------------------------------------------------------------
-spec in_conn(NN, N) -> Connections when
      NN :: netgraph(),
      N  :: neuron(),
      Connections :: [conn()].
in_conn(NN, N) ->
    in_conn(NN, N, sequential) ++ in_conn(NN, N, recurrent).

-spec in_conn(NN, N, Type) -> Connections when
      NN   :: netgraph(),
      N    :: neuron(),
      Type :: d_type(),
      Connections :: [conn()].
in_conn(NN, N, sequential) -> 
    ets:select(NN#netgraph.dtab,[{{{ in,N},'$1'},[],['$1']}]);
in_conn(NN, N, recurrent)  -> 
    ets:select(NN#netgraph.rtab,[{{{ in,N},'$1'},[],['$1']}]).

%%-------------------------------------------------------------------
%% @doc Returns all connections emanating from N of netgraph NN. 
%% @end
%%-------------------------------------------------------------------
-spec out_conn(NN, N) -> Connections when
      NN :: netgraph(),
      N  :: neuron(),
      Connections :: [conn()].
out_conn(NN, N) ->
    out_conn(NN, N, sequential) ++ out_conn(NN, N, recurrent).

-spec out_conn(NN, N, Type) -> Connections when
      NN   :: netgraph(),
      N    :: neuron(),
      Type :: d_type(),
      Connections :: [conn()].
out_conn(NN, N, sequential) ->
    ets:select(NN#netgraph.dtab,[{{{out,N},'$1'},[],['$1']}]);
out_conn(NN, N, recurrent) -> 
    ets:select(NN#netgraph.rtab,[{{{out,N},'$1'},[],['$1']}]).

%%-------------------------------------------------------------------
%% @doc Creates (or modifies) a connection between N1 and N2. 
%% @end
%%------------------------------------------------------------------
-spec add_conn(NN, N1, N2) -> Result when
      NN :: netgraph(),
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
    case seq_path(NN, N2, N1) of
        false -> insert_seq_conn(NN, N1, N2);
        Path  -> insert_rcc_conn(NN, N1, N2, Path)
    end.

insert_seq_conn(#netgraph{dtab=DT, ctab=CT}, N1, N2) ->
    Id = connection:new(N1, N2),
    ets:insert(DT, [{{out, N1}, Id}, {{in, N2}, Id}]),
    ets:insert(CT, {Id, N1, N2, []}),
    Id.

insert_rcc_conn(#netgraph{recurrent=false}, _, _, Path) ->
    {error, {bad_conn, Path}};
insert_rcc_conn(#netgraph{rtab=RT, ctab=CT}, N1, N2, _) ->
    Id = connection:new(N1, N2),
    ets:insert(RT, [{{out, N1}, Id}, {{in, N2}, Id}]),
    ets:insert(CT, {Id, N1, N2, []}),
    Id.

%%-------------------------------------------------------------------
%% @doc Deletes the connection between N1 and N2. 
%% @end
%%-------------------------------------------------------------------
-spec del_conn(NN, N1, N2) -> 'true' when
      NN :: netgraph(),
      N1 :: neuron(),
      N2 :: neuron().
del_conn(NN, N1, N2) ->
    C = connection:id(N1, N2),
    DQuery = [{{{in,N2},C}, [], [true]}, {{{out,N1},C}, [], [true]}],
    ets:select_delete(NN#netgraph.dtab, DQuery),
    ets:select_delete(NN#netgraph.rtab, DQuery),
    ets:delete(NN#netgraph.ctab, C).

%%-------------------------------------------------------------------
%% @doc Returs the number of connections in the netgraph. 
%% @end
%%-------------------------------------------------------------------
-spec no_conn(NN) -> non_neg_integer() when
      NN :: netgraph().
no_conn(NN) ->
    ets:info(NN#netgraph.ctab, size).

%%-------------------------------------------------------------------
%% @doc Returs all netgraph connections. 
%% @end
%%-------------------------------------------------------------------
-spec conn(NN) -> Conns when
      NN :: netgraph(),
      Conns :: [conn()].
conn(NN) ->
    ets:select(NN#netgraph.ctab, [{{'$1','_','_','_'}, [], ['$1']}]).

%%-------------------------------------------------------------------
%% @doc Returs all the neuron connections. 
%% @end
%%-------------------------------------------------------------------
-spec conn(NN, N) -> Conns when
      NN :: netgraph(),
      N  :: neuron(),
      Conns :: [conn()].
conn(NN, N) ->
    Query = [{{{out,N},'$1'},[],['$1']}, {{{in,N},'$1'},[],['$1']}],
    ets:select(NN#netgraph.dtab, Query) ++ 
    ets:select(NN#netgraph.rtab, Query).


%%====================================================================
%% Internal functions
%%====================================================================

% Cheks if the neuron belongs to the ntab ----------------------------
check_neurons(NN, [N | Neurons]) -> 
    case ets:member(NN#netgraph.ntab, N) of
        false -> {error, {bad_neuron, N}};
        true  -> check_neurons(NN, Neurons)
    end;
check_neurons(_NN, []) -> 
    ok.

% Collects the neurons with a type (d_type) and direction (in|out) --
collect_neurons(NN, Type, Direction) -> 
    Ns = neurons(NN),
    case Type of 
        sequential -> filter_neurons(Ns, NN#netgraph.dtab, Direction);
        recurrent  -> filter_neurons(Ns, NN#netgraph.rtab, Direction)
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

% Finds a path from N1 to N2 ----------------------------------------
seq_path(NN, N1, N2) ->
    one_path(out_neighbours(NN, N1), N2, [], [N1], [N1], NN).

one_path([W| _], W,    _,  _, Ps,  _) -> % The path is found
    lists:reverse([W|Ps]); 
one_path([N|Ns], W, Cont, Xs, Ps, NN) -> 
    case lists:member(N, Xs) of
        true  -> % That neuron were evluated before
            one_path(Ns, W, Cont, Xs, Ps, NN);
        false -> % That neuron out neighbours can be check firts
            Nexts = out_neighbours(NN,N),
            one_path(Nexts, W, [{Ns,Ps}|Cont], [N|Xs], [N|Ps], NN)
    end;
one_path([], W, [{Ns,Ps}|Cont], Xs, _, NN) -> % End of neighbours
    one_path(Ns, W, Cont, Xs, Ps, NN);
one_path([], _,             [],  _, _,  _) -> % No seq path
    false.
