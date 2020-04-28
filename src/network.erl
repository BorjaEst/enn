%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(network).

-export([new/0, new/1, id/1, clone/2]).
-export([record_fields/0, info/1, to_map/1]).

-export([add_neuron/2, add_neurons/2, del_neuron/2, del_neurons/2]).
-export([no_neurons/1, neurons/1]).
-export([start_neurons/1, in_degree/1, sink_neurons/1]).

-export([add_link/2, add_links/2, del_link/2, del_links/2]).
-export([out_links/2, no_links/1, links/1]).

-export_type([network/0, d_type/0, d_node/0, info/0]).

-type id()      :: {reference(), network}.
-type d_type()  :: 'sequential' | 'recurrent'.
-type d_node()  :: neuron:id() | 'start' | 'end'.
-record(network, {
    id = {make_ref(), network} :: id(),
    nodes :: #{d_node() => sets:set(d_node())},
    type  :: d_type()
}).
-type network() :: #network{}.
-type info()    :: #{'type' =>    Type :: d_type(),
                     'size' => NoWords :: non_neg_integer()}.


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
      Type :: d_type().
new(Type) ->
    case check_type(Type) of
        ok -> #network{type=Type, nodes=start_nodes()};
        _  -> erlang:error(badarg)
    end.

check_type(sequential) -> ok;
check_type( recurrent) -> ok;
check_type(         _) -> error.

start_nodes() -> #{'start'=> sets:new()}.

%%--------------------------------------------------------------------
%% @doc Returns the network id.
%% @end
%%-------------------------------------------------------------------
-spec id(Network :: network()) -> id().
id(NN) -> NN#network.id.

%%-------------------------------------------------------------------
%% @doc Clones a link replacing the From and To ids using a map.
%% @end
%%-------------------------------------------------------------------
-spec clone(NN :: network(), #{Old => New}) -> network() when 
    Old :: neuron:id(),
    New :: neuron:id().
clone(NN, Map) ->
    NN#network{
        id    = {make_ref(), network},
        nodes = replace(NN#network.nodes, Map)
    }.

replace(Nodes, Map) -> 
    maps:from_list([{maps:get(N,Map,N), replace_set(Conn,Map)} 
        || {N,Conn} <- maps:to_list(Nodes)]).

replace_set(Conn, Map) -> 
    sets:from_list([maps:get(N,Map,N)|| N <- sets:to_list(Conn)]).

%%-------------------------------------------------------------------
%% @doc Record fields from network.  
%% @end
%%-------------------------------------------------------------------
-spec record_fields() -> ListOfFields :: [atom()].
record_fields() -> record_info(fields, network).

%%-------------------------------------------------------------------
%% @doc Information from the network.  
%% @end
%%-------------------------------------------------------------------
-spec info(NN :: network()) -> info().
info(#network{} = NN) ->
    Type = NN#network.type,
    Size = no_neurons(NN),
    #{id=>id(NN), type=>Type, size=>Size}.

%%-------------------------------------------------------------------
%% @doc Adds a neuron to the network.  
%%
%% TODO: A neuron must have at least 1 direct input
%% @end
%%-------------------------------------------------------------------
-spec add_neuron(NN1, N) -> NN2 when
      NN1 :: network(),
      NN2 :: network(),
      N   :: neuron:id().
add_neuron(#network{nodes=Nodes} = NN, N) ->
    NN#network{
        nodes = Nodes#{N => sets:new()}
    }.

%%-------------------------------------------------------------------
%% @doc Adds a list of neurons to the network.  
%% @end
%%-------------------------------------------------------------------
-spec add_neurons(NN0, Ns) -> NN when
      NN0 :: network(),
      NN  :: network(),
      Ns  :: [neuron:id()].
add_neurons(NN, [N|Nx]) ->
    add_neurons(add_neuron(NN,N), Nx);
add_neurons(NN, []) ->
    NN.

%%-------------------------------------------------------------------
%% @doc Deletes a neuron from the network.  
%% @end
%%-------------------------------------------------------------------
-spec del_neuron(NN0, N) -> NN when
      NN0 :: network(),
      NN  :: network(),
      N   :: neuron:id().
del_neuron(NN, N) ->
    NN#network{nodes = maps:remove(N, NN#network.nodes)}.

%%-------------------------------------------------------------------
%% @doc Deletes a list of neurons from the network.  
%% @end
%%-------------------------------------------------------------------
-spec del_neurons(NN0, Ns) -> NN when
      NN0 :: network(),
      NN  :: network(),
      Ns  :: [neuron:id()].
del_neurons(NN, [N|Nx]) ->
    del_neurons(del_neuron(NN,N), Nx);
del_neurons(NN, []) -> 
    NN.

%%-------------------------------------------------------------------
%% @doc Returns the number of neurons of the network.  
%% @end
%%-------------------------------------------------------------------
-spec no_neurons(NN) -> non_neg_integer() when
      NN :: network().
no_neurons(NN) ->
    maps:size(NN#network.nodes) - 1. %(-'start')

%%-------------------------------------------------------------------
%% @doc Returns a list of all neurons of the network.  
%% @end
%%-------------------------------------------------------------------
-spec neurons(NN) -> Neurons when
      NN :: network(),
      Neurons :: [neuron:id()].
neurons(NN) ->
    maps:keys(NN#network.nodes) -- ['start'].

%%-------------------------------------------------------------------
%% @doc Returns the neurons connected to start. 
%% @end
%%-------------------------------------------------------------------
-spec start_neurons(NN) -> Neurons when
      NN :: network(),
      Neurons :: [neuron:id()].
start_neurons(NN) ->
    out_nodes(NN, 'start').

%%-------------------------------------------------------------------
%% @doc Returns all neurons in the network without outputs.  
%% @end
%%-------------------------------------------------------------------
-spec sink_neurons(NN) -> Neurons when
      NN :: network(),
      Neurons :: [neuron:id()].
sink_neurons(NN) ->
    Pred = fun(_,Conn) -> sets:is_empty(Conn) end,
    Sink = maps:filter(Pred, NN#network.nodes),
    maps:keys(Sink).

%%-------------------------------------------------------------------
%% @doc Returns the in-degree of the network.  
%% @end
%%-------------------------------------------------------------------
-spec in_degree(NN) -> non_neg_integer() when
      NN :: network().
in_degree(NN) ->
    length(start_neurons(NN)).

%%-------------------------------------------------------------------
%% @doc Returns all nodes emanating from N of network NN. 
%% @end
%%-------------------------------------------------------------------
-spec out_nodes(NN, N) -> Nodes when
      NN :: network(),
      N  :: d_node(),
      Nodes :: [d_node()].
out_nodes(NN, N) -> 
    sets:to_list(maps:get(N, NN#network.nodes)).

%%-------------------------------------------------------------------
%% @doc Returns all links emanating from N of network NN. 
%% @end
%%-------------------------------------------------------------------
-spec out_links(NN, N) -> Links when
      NN :: network(),
      N  :: d_node(),
      Links :: [link:link()].
out_links(NN, N1) -> 
    [{N1,N2} || N2 <- out_links(NN,N1)].

%%-------------------------------------------------------------------
%% @doc Creates (or modifies) a link between N1 and N2. Atoms 'start'
%% and 'end' can be used as nodes to add a link to the network start
%% or/and end.
%% @end
%%------------------------------------------------------------------
-spec add_link(NN0, Link) -> NN1 when
      NN0  :: network(),
      Link :: link:link(),
      NN1  :: network().
add_link(NN, {N1, N2}) ->
    is_allowed(NN, N2, N1),
    insert_link(NN, N1, N2).

is_allowed(#network{type= recurrent} =  _,  _,  _) -> true;
is_allowed(#network{type=sequential} = NN, N1, N2) -> 
    case find_path(NN, N2, N1) of
        not_found -> true;
        Path      -> error({bad_link, Path})
    end.

insert_link(#network{nodes=Nodes} = NN, N1, N2) ->
    #{N1:= ConnN1} = Nodes,
    NN#network{nodes = Nodes#{N1:=sets:add_element(N2, ConnN1)}}.

%%-------------------------------------------------------------------
%% @doc Creates (or modifies) a link between N1 and N2. 
%% @end
%%------------------------------------------------------------------
-spec add_links(NN0, Links) -> NN1 when
      NN0   :: network(),
      Links :: [link:link()],
      NN1   :: network().
add_links(NN, [L|Lx]) -> add_links(add_link(NN, L), Lx);
add_links(NN,     []) -> NN.

%%-------------------------------------------------------------------
%% @doc Deletes the link between N1 and N2. 
%% @end
%%-------------------------------------------------------------------
-spec del_link(NN0, Links) -> NN1 when
      NN0   :: network(),
      Links :: [link:link()],
      NN1   :: network().
del_link(NN, {N1, N2}) ->
    remove_link(NN, N1, N2).

remove_link(#network{nodes=Nodes} = NN, N1, N2) ->
    #{N1:= ConnN1} = Nodes,
    NN#network{nodes = Nodes#{N1:=sets:del_element(N2, ConnN1)}}.

%%-------------------------------------------------------------------
%% @doc Deletes the links using lists of neurons. 
%% @end
%%-------------------------------------------------------------------
-spec del_links(NN0, Links) -> NN1 when
      NN0   :: network(),
      Links :: [link:link()],
      NN1   :: network().
del_links(NN, [L|Lx]) -> del_links(del_link(NN, L), Lx);
del_links(NN,     []) -> NN.

%%-------------------------------------------------------------------
%% @doc Returs the number of links in the network. 
%% @end
%%-------------------------------------------------------------------
-spec no_links(NN) -> non_neg_integer() when
      NN :: network().
no_links(NN) ->
    length(links(NN)).

%%-------------------------------------------------------------------
%% @doc Returs all network links. 
%% @end
%%-------------------------------------------------------------------
-spec links(NN) -> Links when
      NN :: network(),
      Links :: [link:link()].
links(#network{nodes=Nodes} = NN) ->
    lists:append([out_links(NN,N) || N <- maps:keys(Nodes)]).

%%-------------------------------------------------------------------
%% @doc Return a network in a map format. 
%% @end
%%-------------------------------------------------------------------
-spec to_map(NN) -> NN_Map when
      NN     :: network(),
      NN_Map :: #{id    => network:id(),
                  nodes => #{From :: d_node() => To ::[d_nodes]},
                  type  => sequential | recurrent}.
to_map(NN) ->
    #{
        id    => NN#network.id,
        nodes => to_map_nodes(NN#network.nodes),
        type  => NN#network.type
    }.


%%====================================================================
%% Internal functions
%%====================================================================

% Finds a path from N1 to N2 ----------------------------------------
find_path( _,  N,  N) -> 
    [N];
find_path(NN, N1, N2) ->
    one_path(out_nodes(NN,N1), N2, [], [N1], [N1], NN).

one_path([W| _], W,    _,  _, Ps,  _) -> % The path is found
    lists:reverse([W|Ps]); 
one_path([N|Ns], W, Cont, Xs, Ps, NN) -> 
    case lists:member(N, Xs) of
        true  -> % That neuron were evluated before
            one_path(Ns, W, Cont, Xs, Ps, NN);
        false -> % That neuron out neighbours can be check firts
            Nexts = out_nodes(NN,N),
            one_path(Nexts, W, [{Ns,Ps}|Cont], [N|Xs], [N|Ps], NN)
    end;
one_path([], W, [{Ns,Ps}|Cont], Xs, _, NN) -> % End of neighbours
    one_path(Ns, W, Cont, Xs, Ps, NN);
one_path([], _,             [],  _, _,  _) -> % No seq path
    not_found.

% Converts the nodes and connections into a map with lists ----------
to_map_nodes(Nodes) -> 
    Fun = fun(_,S) -> sets:to_list(S) end,
    maps:map(Fun, Nodes).

