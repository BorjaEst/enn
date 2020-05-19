%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(network).

-export([new/0, new/1, id/1, clone/1]).
-export([record_fields/0, info/1, to_map/1]).
-export([no_neurons/1, neurons/1, connections/2]).
-export([sink_neurons/1, bias_neurons/1]).

-export([add_neuron/2, add_neurons/2, del_neuron/2, del_neurons/2]).
-export([in_degree/1, in_nodes/1, out_degree/1, out_nodes/1]).
-export([in_degree/2, in_nodes/2, out_degree/2, out_nodes/2]).

-export([add_link/2, add_links/2, del_link/2, del_links/2]).
-export([in_links/1, out_links/1, links/1, no_links/1]).
-export([in_links/2, out_links/2, links/2, no_links/2]).

-export_type([network/0, connections/0, d_type/0, d_node/0, info/0]).

-type id()      :: {reference(), network}.
-define(NEW_ID, {make_ref(), network}).
-type d_type()  :: 'sequential' | 'recurrent'.
-type d_node()  :: neuron:id() | 'start' | 'end'.
-record(cn, {
    in  = #{} :: #{d_node() => link},
    out = #{} :: #{d_node() => link}
}).
-record(network, {
    id = ?NEW_ID :: id(),
    nodes :: #{d_node() => #{d_node() => #cn{}}},
    type  :: d_type()
}).
-type network() :: #network{}.

-type connections() :: #{in  => #{d_node() => link},
                         out => #{d_node() => link}}.
-type info()    :: #{'type' =>    Type :: d_type(),
                     'size' => NoWords :: non_neg_integer()}.


%%%===================================================================
%%% API
%%%===================================================================

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

start_nodes() -> #{'start'=>#cn{},'end'=>#cn{}}.

%%--------------------------------------------------------------------
%% @doc Returns the network id.
%% @end
%%-------------------------------------------------------------------
-spec id(Network :: network()) -> id().
id(NN) -> NN#network.id.

%%-------------------------------------------------------------------
%% @doc Clones a link replacing the From and To ids using a map.
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec clone(Id :: id()) -> id().
clone(Id) ->
    [NN]   = mnesia:read(network, Id),
    NClones = [neuron:clone(N) || N <- neurons(NN)],
    NMap    = maps:from_list(lists:zip(neurons(NN), NClones)),
    [link:clone({From,To}, NMap) || {From,To} <- network:links(NN)],
    Nodes = replace(NN#network.nodes, NMap),
    Clone = NN#network{id=?NEW_ID, nodes=Nodes},
    ok = mnesia:write(Clone),
    id(Clone).

replace(Nodes, NMap) -> 
    maps:from_list([{maps:get(N,NMap,N), replace_conn(Conn,NMap)} 
        || {N,Conn} <- maps:to_list(Nodes)]).

replace_conn(Conn, NMap) -> 
    #cn{
        in  = replace_map( Conn#cn.in, NMap),
        out = replace_map(Conn#cn.out, NMap)
    }.

replace_map(Map, NMap) -> 
    maps:from_list([{maps:get(Node, NMap, Node),Value} 
        || {Node,Value} <- maps:to_list(Map)]).

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
    #{type=>Type, size=>Size}.

%%-------------------------------------------------------------------
%% @doc Return a network in a map format. 
%% @end
%%-------------------------------------------------------------------
-spec to_map(NN) -> NN_Map when
    NN     :: network(),
    NN_Map :: #{id    => network:id(),
                nodes => #{d_node() => connections()},
                type  => d_type()}.
    
to_map(NN) ->
    #{
        id    => NN#network.id,
        nodes => maps:map(fun map_nodes/2, NN#network.nodes),
        type  => NN#network.type
    }.

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
        nodes = Nodes#{N => #cn{}}
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
del_neuron(NN0, N) ->
    NN = del_links(NN0, links(NN0,N)),
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
%% @doc Returns the node connections.  
%% @end
%%-------------------------------------------------------------------
-spec connections(NN, N) -> connections() when
      NN :: network(),
      N  :: d_node().
connections(NN, N) ->
    map_nodes(N, maps:get(N, NN#network.nodes)).

%%-------------------------------------------------------------------
%% @doc Returns the number of neurons of the network.  
%% @end
%%-------------------------------------------------------------------
-spec no_neurons(NN) -> non_neg_integer() when
      NN :: network().
no_neurons(NN) ->
    maps:size(NN#network.nodes) - 2. %(-'start' -'end')

%%-------------------------------------------------------------------
%% @doc Returns a list of all neurons of the network.  
%% @end
%%-------------------------------------------------------------------
-spec neurons(NN) -> Neurons when
      NN :: network(),
      Neurons :: [neuron:id()].
neurons(NN) ->
    maps:keys(NN#network.nodes) -- ['start', 'end'].

%%-------------------------------------------------------------------
%% @doc Returns all neurons in the network without outputs.  
%% @end
%%-------------------------------------------------------------------
-spec sink_neurons(NN) -> Neurons when
      NN :: network(),
      Neurons :: [neuron:id()].
sink_neurons(NN) ->
    Pred = fun(_,Conn) -> #{} == Conn#cn.out end,
    Sink = maps:filter(Pred, NN#network.nodes),
    maps:keys(Sink) -- ['start', 'end'].

%%-------------------------------------------------------------------
%% @doc Returns all neurons in the network without inputs.  
%% @end
%%-------------------------------------------------------------------
-spec bias_neurons(NN) -> Neurons when
      NN :: network(),
      Neurons :: [neuron:id()].
bias_neurons(NN) ->
    Pred = fun(_,Conn) -> #{} == Conn#cn.in end,
    Bias = maps:filter(Pred, NN#network.nodes),
    maps:keys(Bias) -- ['start', 'end'].

%%-------------------------------------------------------------------
%% @doc Returns the neurons connected to start. 
%% @end
%%-------------------------------------------------------------------
-spec in_nodes(NN) -> Neurons when
      NN :: network(),
      Neurons :: [neuron:id()].
in_nodes(NN) ->
    out_nodes(NN, 'start').

%%-------------------------------------------------------------------
%% @doc Returns the in-degree of the network.  
%% @end
%%-------------------------------------------------------------------
-spec in_degree(NN) -> non_neg_integer() when
      NN :: network().
in_degree(NN) ->
    out_degree(NN, 'start').

%%-------------------------------------------------------------------
%% @doc Returns all nodes emanating from N of network NN. 
%% @end
%%-------------------------------------------------------------------
-spec in_nodes(NN, N) -> Nodes when
      NN :: network(),
      N  :: d_node(),
      Nodes :: [d_node()].
in_nodes(NN, N) -> 
    Conn = maps:get(N, NN#network.nodes),
    maps:keys(Conn#cn.in).

%%-------------------------------------------------------------------
%% @doc Returns the in-degree of the network node.  
%% @end
%%-------------------------------------------------------------------
-spec in_degree(NN, N) -> non_neg_integer() when
      NN :: network(),
      N  :: d_node().
in_degree(NN, N) ->
    Conn = maps:get(N, NN#network.nodes),
    maps:size(Conn#cn.in).

%%-------------------------------------------------------------------
%% @doc Returns the neurons connected to end. 
%% @end
%%-------------------------------------------------------------------
-spec out_nodes(NN) -> Neurons when
      NN :: network(),
      Neurons :: [neuron:id()].
out_nodes(NN) ->
    in_nodes(NN, 'end').

%%-------------------------------------------------------------------
%% @doc Returns the out-degree of the network.  
%% @end
%%-------------------------------------------------------------------
-spec out_degree(NN) -> non_neg_integer() when
      NN :: network().
out_degree(NN) ->
    in_degree(NN, 'end').

%%-------------------------------------------------------------------
%% @doc Returns all nodes incident in N of network NN. 
%% @end
%%-------------------------------------------------------------------
-spec out_nodes(NN, N) -> Nodes when
      NN :: network(),
      N  :: d_node(),
      Nodes :: [d_node()].
out_nodes(NN, N) -> 
    Conn = maps:get(N, NN#network.nodes),
    maps:keys(Conn#cn.out).

%%-------------------------------------------------------------------
%% @doc Returns the out-degree of the network node.  
%% @end
%%-------------------------------------------------------------------
-spec out_degree(NN, N) -> non_neg_integer() when
      NN :: network(),
      N  :: d_node().
out_degree(NN, N) ->
    Conn = maps:get(N, NN#network.nodes),
    maps:size(Conn#cn.out).

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
    is_allowed(NN, N1, N2),
    insert_link(NN, N1, N2).

is_allowed(#network{type= recurrent} =  _,  _,  _) -> true;
is_allowed(#network{type=sequential} = NN, N1, N2) -> 
    case find_path(NN, N2, N1) of
        not_found -> true;
        Path      -> error({bad_link, Path})
    end.

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
%% @doc Returns all links emanating from the start of the network. 
%% @end
%%-------------------------------------------------------------------
-spec in_links(NN) -> Links when
      NN :: network(),
      Links :: [link:link()].
in_links(NN) ->
    out_links(NN, 'start').

%%-------------------------------------------------------------------
%% @doc Returns all links incident on N of network NN. 
%% @end
%%-------------------------------------------------------------------
-spec in_links(NN, N) -> Links when
      NN :: network(),
      N  :: d_node(),
      Links :: [link:link()].
in_links(NN, N2) ->
    ConnN2 = maps:get(N2, NN#network.nodes),
    [{N1,N2} || {N1,link} <- maps:to_list(ConnN2#cn.in)].

%%-------------------------------------------------------------------
%% @doc Returns all links incident on the end of the network.  
%% @end
%%-------------------------------------------------------------------
-spec out_links(NN) -> Links when
      NN :: network(),
      Links :: [link:link()].
out_links(NN) ->
    in_links(NN, 'end').

%%-------------------------------------------------------------------
%% @doc Returns all links emanating from N of network NN. 
%% @end
%%-------------------------------------------------------------------
-spec out_links(NN, N) -> Links when
      NN :: network(),
      N  :: d_node(),
      Links :: [link:link()].
out_links(NN, N1) ->
    ConnN1 = maps:get(N1, NN#network.nodes),
    [{N1,N2} || {N2,link} <- maps:to_list(ConnN1#cn.out)].

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
%% @doc Returs all node links. 
%% @end
%%-------------------------------------------------------------------
-spec links(NN, N) -> Links when
      NN :: network(),
      N  :: d_node(),
      Links :: [link:link()].
links(NN, N) ->
    lists:append(in_links(NN,N), out_links(NN, N)).

%%-------------------------------------------------------------------
%% @doc Returs the number of links in the network. 
%% @end
%%-------------------------------------------------------------------
-spec no_links(NN) -> non_neg_integer() when
      NN :: network().
no_links(NN) ->
    length(links(NN)).

%%-------------------------------------------------------------------
%% @doc Returs the number of links in the node. 
%% @end
%%-------------------------------------------------------------------
-spec no_links(NN, N) -> non_neg_integer() when
      NN :: network(),
      N  :: d_node().
no_links(NN, N) ->
    length(links(NN, N)).


%%====================================================================
%% Internal functions
%%====================================================================

% Finds a path from N1 to N2 ----------------------------------------
find_path( _,  N,  N) -> 
    [N];
find_path(NN, N1, N2) ->
    Nexts = out_nodes(NN,N1),
    one_path(Nexts, N2, [], [N1], [N1], NN).

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

% Inserts a link on the nodes ---------------------------------------
insert_link(#network{nodes=Nodes} = NN,  N,  N) ->
    #{N:= Conn} = Nodes,
    NN#network{nodes = Nodes#{
        N := add_out(add_in(Conn, N, link), N, link)
    }};
insert_link(#network{nodes=Nodes} = NN, N1, N2) ->
    #{N1:= ConnN1, N2:=ConnN2} = Nodes,
    NN#network{nodes = Nodes#{
        N1 := add_out(ConnN1, N2, link),
        N2 := add_in( ConnN2, N1, link)
    }}.

add_out(#cn{out=Out} = ConnN1, N2, What) -> 
    ConnN1#cn{out = Out#{N2 => What}}.

add_in(#cn{in=In} = ConnN2, N1, What) -> 
    ConnN2#cn{in = In#{N1 => What}}.

% Removes all link from the nodes -----------------------------------
remove_link(#network{nodes=Nodes} = NN,  N,  N) ->
    #{N:= Conn} = Nodes,
    NN#network{nodes = Nodes#{
        N := remove_out(remove_in(Conn, N), N)
    }};
remove_link(#network{nodes=Nodes} = NN, N1, N2) ->
    #{N1:= ConnN1, N2:=ConnN2} = Nodes,
    NN#network{nodes = Nodes#{
        N1 := remove_out(ConnN1, N2),
        N2 := remove_in( ConnN2, N1)
    }}.

remove_out(#cn{out=Out} = ConnN1, N2) -> 
    ConnN1#cn{out = maps:remove(N2, Out)}.

remove_in(#cn{in=In} = ConnN2, N1) -> 
    ConnN2#cn{in = maps:remove(N1, In)}.

% Converts a cn record into a connection ----------------------------
map_nodes(_Key, #cn{in=In,out=Out}) ->
    #{in=>In,out=>Out}.

