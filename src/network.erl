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
%%%       update its weight values {{FromID, ToID, link}, W}.
%%% @end
%%%-------------------------------------------------------------------
-module(network).

-export([new/0, new/1, delete/1, info/1]).

-export([add_neuron/1, add_neuron/2, del_neuron/2]).
-export([neuron/2, no_neurons/1, neurons/1]).
-export([source_neurons/1, source_neurons/2]).
-export([sink_neurons/1, sink_neurons/2]).

-export([add_link/3, del_link/3]).
-export([no_link/1, link/1, link/2]).

-export([out_neighbours/2, in_neighbours/2]).
-export([out_link/2, in_link/2]).
-export([out_degree/2, in_degree/2]).

-export_type([network/0, d_type/0, neuron/0, link/0, label/0]).

-type network_id() :: {reference(), network}.
-type network_type()  :: 'sequential' | 'recurrent'.
-record(network, {
    id = {make_ref(), network} :: network_id(),
    cn = #{} :: #{neuron_id() => connections()},   
    type = recurrent :: network_type()
}).
-type network() :: #network{}.

-record(connections, {
    seq = {[],[]} :: {In :: [neuron_id()], Out :: [neuron_id()]},
    rcc = {[],[]} :: {In :: [neuron_id()], Out :: [neuron_id()]}
}).
-type connections() :: #connections{}.

-type link_id() :: {From :: neuron_id(), To :: neuron_id(), link}.
-record(link, {
    id :: linkn_id(),
    w = uninitialized :: weight()
}).
-type link() :: #link{}.


-type neuron()  :: term().


-type add_link_err_rsn() :: {'bad_link', Path :: [neuron()]}
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
        CT = ets:new(    links, ?CTAB_CONFIGUTATION),
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
del_neuron(NN, N1) ->
    [del_link(NN, N1, N2) || N2 <- out_neighbours(NN, N1)],
    [del_link(NN, N2, N1) || N2 <-  in_neighbours(NN, N1)],
    neuron:delete(N1),
    ets:delete(NN#network.ntab, N1).

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
%% @doc Returns all links incident on N of network NN. 
%% @end
%%-------------------------------------------------------------------
-spec in_link(NN, N) -> Links when
      NN :: network(),
      N  :: neuron(),
      Links :: [link()].
in_link(NN, N) ->
    in_link(NN, N, sequential) ++ in_link(NN, N, recurrent).

-spec in_link(NN, N, Type) -> Links when
      NN :: network(),
      N  :: neuron(),
      Type  :: d_type(),
      Links :: [link()].
in_link(NN, N, sequential) -> 
    ets:select(NN#network.dtab,[{{{ in,N},'$1'},[],['$1']}]);
in_link(NN, N, recurrent)  -> 
    ets:select(NN#network.rtab,[{{{ in,N},'$1'},[],['$1']}]).

%%-------------------------------------------------------------------
%% @doc Returns all links emanating from N of network NN. 
%% @end
%%-------------------------------------------------------------------
-spec out_link(NN, N) -> Links when
      NN :: network(),
      N  :: neuron(),
      Links :: [link()].
out_link(NN, N) ->
    out_link(NN, N, sequential) ++ out_link(NN, N, recurrent).

-spec out_link(NN, N, Type) -> Links when
      NN   :: network(),
      N    :: neuron(),
      Type :: d_type(),
      Links :: [link()].
out_link(NN, N, sequential) ->
    ets:select(NN#network.dtab,[{{{out,N},'$1'},[],['$1']}]);
out_link(NN, N, recurrent) -> 
    ets:select(NN#network.rtab,[{{{out,N},'$1'},[],['$1']}]).

%%-------------------------------------------------------------------
%% @doc Creates (or modifies) a link between N1 and N2. 
%% @end
%%------------------------------------------------------------------
-spec add_link(NN, N1, N2) -> Result when
      NN :: network(),
      N1 :: neuron(),
      N2 :: neuron(),
      Result :: link() | {'error', add_link_err_rsn()}.
add_link(NN, N1, N2) ->
    case check_neurons(NN, [N1, N2]) of 
    {error, Reason} -> {error, Reason};
    ok              -> insert_link(NN, N1, N2)
    end.

insert_link(NN, N1, N2) when N1 =:= N2 ->
    insert_rcc_link(NN, N1, N2, [N1,N2]);
insert_link(NN, N1, N2) ->
    case seq_path(NN, N2, N1) of
        false -> insert_seq_link(NN, N1, N2);
        Path  -> insert_rcc_link(NN, N1, N2, Path)
    end.

insert_seq_link(#network{dtab=DT, ctab=CT}, N1, N2) ->
    Id = link:new(N1, N2),
    ets:insert(DT, [{{out, N1}, Id}, {{in, N2}, Id}]),
    ets:insert(CT, {Id, N1, N2, []}),
    Id.

insert_rcc_link(#network{recurrent=false}, _, _, Path) ->
    {error, {bad_link, Path}};
insert_rcc_link(#network{rtab=RT, ctab=CT}, N1, N2, _) ->
    Id = link:new(N1, N2),
    ets:insert(RT, [{{out, N1}, Id}, {{in, N2}, Id}]),
    ets:insert(CT, {Id, N1, N2, []}),
    Id.

%%-------------------------------------------------------------------
%% @doc Deletes the link between N1 and N2. 
%% @end
%%-------------------------------------------------------------------
-spec del_link(NN, N1, N2) -> 'true' when
      NN :: network(),
      N1 :: neuron(),
      N2 :: neuron().
del_link(NN, N1, N2) ->
    C = link:id(N1, N2),
    DQuery = [{{{in,N2},C}, [], [true]}, {{{out,N1},C}, [], [true]}],
    ets:select_delete(NN#network.dtab, DQuery),
    ets:select_delete(NN#network.rtab, DQuery),
    ets:delete(NN#network.ctab, C).

%%-------------------------------------------------------------------
%% @doc Returs the number of links in the network. 
%% @end
%%-------------------------------------------------------------------
-spec no_link(NN) -> non_neg_integer() when
      NN :: network().
no_link(NN) ->
    ets:info(NN#network.ctab, size).

%%-------------------------------------------------------------------
%% @doc Returs all network links. 
%% @end
%%-------------------------------------------------------------------
-spec link(NN) -> Links when
      NN :: network(),
      Links :: [link()].
link(NN) ->
    ets:select(NN#network.ctab, [{{'$1','_','_','_'}, [], ['$1']}]).

%%-------------------------------------------------------------------
%% @doc Returs all the neuron links. 
%% @end
%%-------------------------------------------------------------------
-spec link(NN, N) -> Links when
      NN :: network(),
      N  :: neuron(),
      Links :: [link()].
link(NN, N) ->
    Query = [{{{out,N},'$1'},[],['$1']}, {{{in,N},'$1'},[],['$1']}],
    ets:select(NN#network.dtab, Query) ++ 
    ets:select(NN#network.rtab, Query).


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
