%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(network).

-export([new/0, new/1, id/1, clone/2, record_fields/0, info/1]).

-export([add_neuron/2, add_neurons/2, del_neuron/2, del_neurons/2]).
-export([node/2, no_neurons/1, neurons/1]).
-export([start_neurons/1, end_neurons/1]).
-export([sink_neurons/1, bias_neurons/1]).

-export([in_degree/1, out_degree/1]).
-export([add_link/2, add_links/2, del_link/2, del_links/2]).
-export([out_links/2, out_links/3, in_links/2,  in_links/3]). 
-export([no_links/1, links/1, links/2]).

-export_type([network/0, d_type/0, d_node/0, info/0]).

-type id()      :: {reference(), network}.
-type d_type()  :: 'sequential' | 'recurrent'.
-type d_node()  :: neuron:id() | 'start' | 'end'.
-record(network, {
    id = {make_ref(), network} :: id(),
    nodes :: #{d_node() => nn_node:connections()},
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

start_nodes() -> #{'start'=>nn_node:new(),'end'=>nn_node:new()}.

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
        nodes = Nodes#{N => nn_node:new()}
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
%% @doc Returns the node information or false if the node does not 
%% belong to that network.  
%% @end
%%-------------------------------------------------------------------
-spec node(NN, N) -> connections:connections() | 'false' when
      NN :: network(),
      N  :: d_node().
node(NN, N) ->
    maps:get(N, NN#network.nodes, 'false').

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
%% @doc Returns the neurons connected to start. 
%% @end
%%-------------------------------------------------------------------
-spec start_neurons(NN) -> Neurons when
      NN :: network(),
      Neurons :: [neuron:id()].
start_neurons(NN) ->
    nn_node:out_neighbours(node(NN, 'start')).

%%-------------------------------------------------------------------
%% @doc Returns the neurons connected to end. 
%% @end
%%-------------------------------------------------------------------
-spec end_neurons(NN) -> Neurons when
      NN :: network(),
      Neurons :: [neuron:id()].
end_neurons(NN) ->
    nn_node:in_neighbours(node(NN, 'end')).

%%-------------------------------------------------------------------
%% @doc Returns all neurons in the network without outputs.  
%% @end
%%-------------------------------------------------------------------
-spec sink_neurons(NN) -> Neurons when
      NN :: network(),
      Neurons :: [neuron:id()].
sink_neurons(NN) ->
    Pred = fun(_,Conn) -> nn_node:is_sink(Conn) end,
    Sink = maps:filter(Pred, NN#network.nodes),
    maps:keys(Sink) -- ['end'].

%%-------------------------------------------------------------------
%% @doc Returns all neurons in the network without inputs.  
%% @end
%%-------------------------------------------------------------------
-spec bias_neurons(NN) -> Neurons when
      NN :: network(),
      Neurons :: [neuron:id()].
bias_neurons(NN) ->
    Pred = fun(_,Conn) -> nn_node:is_bias(Conn) end,
    Bias = maps:filter(Pred, NN#network.nodes),
    maps:keys(Bias) -- ['start'].

%%-------------------------------------------------------------------
%% @doc Returns the in-degree of the network.  
%% @end
%%-------------------------------------------------------------------
-spec in_degree(NN) -> non_neg_integer() when
      NN :: network().
in_degree(NN) ->
    nn_node:out_degree(node(NN, 'start')).

%%-------------------------------------------------------------------
%% @doc Returns the out-degree of the network.  
%% @end
%%-------------------------------------------------------------------
-spec out_degree(NN) -> non_neg_integer() when
      NN :: network().
out_degree(NN) ->
    nn_node:in_degree(node(NN, 'end')).

%%-------------------------------------------------------------------
%% @doc Returns all links incident on N of network NN. 
%% @end
%%-------------------------------------------------------------------
-spec in_links(NN, N) -> Links when
      NN :: network(),
      N  :: d_node(),
      Links :: [link:link()].
in_links(NN, N2) ->
    [{N1,N2} || N1<-nn_node:in_neighbours(node(NN,N2))].

-spec in_links(NN, N, Type) -> Links when
      NN :: network(),
      N  :: d_node(),
      Type  :: d_type(),
      Links :: [link:link()].
in_links(NN, N2, Type) -> 
    [{N1,N2} || N1<-nn_node:in_neighbours(node(NN,N2),Type)].

%%-------------------------------------------------------------------
%% @doc Returns all links emanating from N of network NN. 
%% @end
%%-------------------------------------------------------------------
-spec out_links(NN, N) -> Links when
      NN :: network(),
      N  :: d_node(),
      Links :: [link:link()].
out_links(NN, N1) ->
    [{N1,N2} || N2<-nn_node:out_neighbours(node(NN,N1))].

-spec out_links(NN, N, Type) -> Links when
      NN   :: network(),
      N    :: d_node(),
      Type  :: d_type(),
      Links :: [link:link()].
out_links(NN, N1, Type) ->
    [{N1,N2} || N2<-nn_node:out_neighbours(node(NN,N1),Type)].

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
    case seq_path(NN, N2, N1) of
        false -> insert_seq_link(NN, N1, N2);
        Path  -> insert_rcc_link(NN, N1, N2, Path)
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
%% @doc Returs all the neuron links. 
%% @end
%%-------------------------------------------------------------------
-spec links(NN, N) -> Links when
      NN :: network(),
      N  :: d_node(),
      Links :: [link:link()].
links(NN, N) ->
    in_links(NN, N) ++ out_links(NN, N).


%%====================================================================
%% Internal functions
%%====================================================================

% Finds a path from N1 to N2 ----------------------------------------
seq_path( _,  N,  N) -> 
    [N];
seq_path(NN, N1, N2) ->
    OutSeq = nn_node:out_neighbours(node(NN,N1), sequential),
    one_path(OutSeq, N2, [], [N1], [N1], NN).

one_path([W| _], W,    _,  _, Ps,  _) -> % The path is found
    lists:reverse([W|Ps]); 
one_path([N|Ns], W, Cont, Xs, Ps, NN) -> 
    case lists:member(N, Xs) of
        true  -> % That neuron were evluated before
            one_path(Ns, W, Cont, Xs, Ps, NN);
        false -> % That neuron out neighbours can be check firts
            Nexts = nn_node:out_neighbours(node(NN,N), sequential),
            one_path(Nexts, W, [{Ns,Ps}|Cont], [N|Xs], [N|Ps], NN)
    end;
one_path([], W, [{Ns,Ps}|Cont], Xs, _, NN) -> % End of neighbours
    one_path(Ns, W, Cont, Xs, Ps, NN);
one_path([], _,             [],  _, _,  _) -> % No seq path
    false.

% Inserts a sequential link on the nodes ----------------------------
insert_seq_link(#network{nodes=Nodes} = NN, N1, N2) ->
    #{N1:= ConnN1, N2:=ConnN2} = Nodes,
    NN#network{nodes = Nodes#{
        N1 := nn_node:add_sequential_out(ConnN1, N2),
        N2 := nn_node:add_sequential_in( ConnN2, N1)
    }}.

% Inserts a recurrent link on the nodes -----------------------------
insert_rcc_link(#network{type=sequential}, _, _, Path) ->
    error({bad_link, Path});
insert_rcc_link(#network{nodes=Nodes} = NN,  N,  N, _) ->
    #{N:= ConnN} = Nodes,
    NN#network{nodes = Nodes#{
        N := nn_node:add_recurrent_out(
               nn_node:add_recurrent_in(ConnN, N), N)
    }};
insert_rcc_link(#network{nodes=Nodes} = NN, N1, N2, _) ->
    #{N1:= ConnN1, N2:=ConnN2} = Nodes,
    NN#network{nodes = Nodes#{
        N1 := nn_node:add_recurrent_out(ConnN1, N2),
        N2 := nn_node:add_recurrent_in( ConnN2, N1)
    }}.

% Removes all link on the nodes -------------------------------------
remove_link(#network{nodes=Nodes} = NN, N1, N2) ->
    #{N1:= ConnN1, N2:=ConnN2} = Nodes,
    NN#network{nodes = Nodes#{
        N1 := nn_node:remove_sequential_out(
                nn_node:remove_recurrent_out(ConnN1, N2), N2),
        N2 := nn_node:remove_sequential_in(
                nn_node:remove_recurrent_in(ConnN2, N1), N1)
    }}.

% Replaces all nodes and their links --------------------------------
replace(Nodes, Map) -> 
    maps:from_list([{maps:get(N,Map,N), nn_node:replace(Conn,Map)} 
        || {N,Conn} <- maps:to_list(Nodes)]).

