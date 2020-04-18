%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(network).

-export([new/0, new/1, info/1]).

-export([add_neuron/2, del_neuron/2]).
-export([node/2, no_neurons/1, neurons/1]).
-export([start_neurons/1, end_neurons/1]).
-export([sink_neurons/1, bias_neurons/1]).

-export([add_link/3, add_links/3, del_link/3, del_links/3]).
-export([out_links/2, out_links/3, in_links/2,  in_links/3]). 
-export([no_links/1, links/1, links/2]).

-export_type([network/0, d_type/0, link/0]).

-type id()      :: {reference(), network}.
-type d_type()  :: 'sequential' | 'recurrent'.
-type d_node()  :: neuron:id() | 'start' | 'end'.
-type link()    :: {From :: d_node(), To :: d_node()}.
-record(network, {
    id   = {make_ref(), network} :: id(),
    cn   :: #{d_node() => nn_node:connections()},
    type :: d_type()
}).
-type network() :: #network{}.


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
        ok -> #network{type=Type, cn=start_cn()};
        _  -> erlang:error(badarg)
    end.

check_type(sequential) -> ok;
check_type( recurrent) -> ok;
check_type(         _) -> error.

start_cn() -> #{'start'=>nn_node:new(),'end'=>nn_node:new()}.


%%-------------------------------------------------------------------
%% @doc Information from the network.  
%% @end
%%-------------------------------------------------------------------
-spec info(NN) -> InfoList when
      NN :: network(),
      InfoList :: [{'type',    Type :: d_type()} |
                   {'size', NoWords :: non_neg_integer()}].
info(#network{} = NN) ->
    Type = NN#network.type,
    Size = no_neurons(NN),
    [{type, Type}, {size, Size}].

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
add_neuron(#network{cn=CN} = NN, N) ->
    NN#network{
        cn = CN#{N => nn_node:new()}
    }.

%%-------------------------------------------------------------------
%% @doc Deletes a neuron from the network.  
%% @end
%%-------------------------------------------------------------------
-spec del_neuron(NN, N) -> 'true' when
      NN :: network(),
      N  :: neuron:id().
del_neuron(NN0, N) ->
    NN1 = del_links(NN0, [N], nn_node:out_neighbours(N)),
    NN2 = del_links(NN1, nn_node:in_neighbours(N), [N]),
    NN2#network{cn = maps:remove(N, NN2#network.cn)}.
    
%%-------------------------------------------------------------------
%% @doc Returns the node information or false if the node does not 
%% belong to that network.  
%% @end
%%-------------------------------------------------------------------
-spec node(NN, N) -> connections:connections() | 'false' when
      NN :: network(),
      N  :: d_node().
node(NN, N) ->
    maps:get(N, NN#network.cn, 'false').

%%-------------------------------------------------------------------
%% @doc Returns the number of neurons of the network.  
%% @end
%%-------------------------------------------------------------------
-spec no_neurons(NN) -> non_neg_integer() when
      NN :: network().
no_neurons(NN) ->
    maps:size(NN#network.cn) - 2. %(-'start' -'end')

%%-------------------------------------------------------------------
%% @doc Returns a list of all neurons of the network.  
%% @end
%%-------------------------------------------------------------------
-spec neurons(NN) -> Neurons when
      NN :: network(),
      Neurons :: [neuron:id()].
neurons(NN) ->
    maps:keys(NN#network.cn) -- ['start', 'end'].

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
    Pred = fun(_,Conn) -> connection:is_sink(Conn) end,
    Sink = maps:filter(Pred, NN#network.cn),
    Sink -- ['end'].

%%-------------------------------------------------------------------
%% @doc Returns all neurons in the network without inputs.  
%% @end
%%-------------------------------------------------------------------
-spec bias_neurons(NN) -> Neurons when
      NN :: network(),
      Neurons :: [neuron:id()].
bias_neurons(NN) ->
    Pred = fun(_,Conn) -> connection:is_bias(Conn) end,
    Bias = maps:filter(Pred, NN#network.cn),
    Bias -- ['start'].

%%-------------------------------------------------------------------
%% @doc Returns all links incident on N of network NN. 
%% @end
%%-------------------------------------------------------------------
-spec in_links(NN, N) -> Links when
      NN :: network(),
      N  :: d_node(),
      Links :: [link()].
in_links(_, N2) ->
    [{N1, N2} || N1 <- nn_node:in_neighbours(N2)].

-spec in_links(NN, N, Type) -> Links when
      NN :: network(),
      N  :: d_node(),
      Type  :: d_type(),
      Links :: [link()].
in_links(_, N2, Type) -> 
    [{N1, N2} || N1 <- nn_node:in_neighbours(N2, Type)].

%%-------------------------------------------------------------------
%% @doc Returns all links emanating from N of network NN. 
%% @end
%%-------------------------------------------------------------------
-spec out_links(NN, N) -> Links when
      NN :: network(),
      N  :: d_node(),
      Links :: [link()].
out_links(_, N1) ->
    [{N1, N2} || N2 <- nn_node:out_neighbours(N1)].

-spec out_links(NN, N, Type) -> Links when
      NN   :: network(),
      N    :: d_node(),
      Type  :: d_type(),
      Links :: [link()].
out_links(_, N1, Type) ->
    [{N1, N2} || N2 <- nn_node:out_neighbours(N1, Type)].

%%-------------------------------------------------------------------
%% @doc Creates (or modifies) a link between N1 and N2. 
%%
%% TODO: Take into accound 'start' and 'end'
%% @end
%%------------------------------------------------------------------
-spec add_link(NN1, N1, N2) -> NN2 when
      NN1 :: network(),
      N1  :: d_node(),
      N2  :: d_node(),
      NN2 :: network().
add_link(NN, N1, N2) when N1 =:= N2 ->
    insert_rcc_link(NN, N1, N2, [N1,N2]);
add_link(NN, N1, N2) ->
    case seq_path(NN, N2, N1) of
        false -> insert_seq_link(NN, N1, N2);
        Path  -> insert_rcc_link(NN, N1, N2, Path)
    end.

%%-------------------------------------------------------------------
%% @doc Creates (or modifies) a link between N1 and N2. 
%% @end
%%------------------------------------------------------------------
-spec add_links(NN1, Ns1, Ns2) -> NN2 when
      NN1 :: network(),
      Ns1 :: [d_node()],
      Ns2 :: [d_node()],
      NN2 :: network().
add_links(NN, Ns1, Ns2) ->
   add_links(NN, Ns1, Ns1, Ns2, Ns2). 

add_links(NN, [N1|Nx1], Ns1, [N2|Nx2], Ns2) -> 
    add_links(add_link(NN,N1,N2), [N1|Nx1], Ns1, Nx2, Ns2);
add_links(NN, [_|Nx1], Ns1, [], Ns2) -> 
    add_links(NN, Nx1, Ns1, Ns2, Ns2);
add_links(NN, [], _, _, _) -> NN.

%%-------------------------------------------------------------------
%% @doc Deletes the link between N1 and N2. 
%% @end
%%-------------------------------------------------------------------
-spec del_link(NN, N1, N2) -> 'true' when
      NN :: network(),
      N1 :: d_node(),
      N2 :: d_node().
del_link(NN, N1, N2) ->
    remove_link(NN, N1, N2).

%%-------------------------------------------------------------------
%% @doc Deletes the links using lists of neurons. 
%% @end
%%-------------------------------------------------------------------
-spec del_links(NN1, Ns1, Ns2) -> NN2 when
      NN1 :: network(),
      Ns1 :: [d_node()],
      Ns2 :: [d_node()],
      NN2 :: network().
del_links(NN, Ns1, Ns2) -> 
    del_links(NN, Ns1, Ns1, Ns2, Ns2).

del_links(NN, [N1|Nx1], Ns1, [N2|Nx2], Ns2) -> 
    del_links(del_link(NN,N1,N2), [N1|Nx1], Ns1, Nx2, Ns2);
del_links(NN, [_|Nx1], Ns1, [], Ns2) -> 
    del_links(NN, Nx1, Ns1, Ns2, Ns2);
del_links(NN, [], _, _, _) -> NN.

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
      Links :: [link()].
links(#network{cn=CN} = NN) ->
    [{N1,N2} || N1<-maps:keys(CN), N2<-nn_node:out_neighbours(N1)].

%%-------------------------------------------------------------------
%% @doc Returs all the neuron links. 
%% @end
%%-------------------------------------------------------------------
-spec links(NN, N) -> Links when
      NN :: network(),
      N  :: d_node(),
      Links :: [link()].
links(NN, N) ->
    [{N1,N} || N1 <-  nn_node:in_neighbours(N)] ++ 
    [{N,N2} || N2 <- nn_node:out_neighbours(N)].


%%====================================================================
%% Internal functions
%%====================================================================

%% Collect elements for a index in a tuple --------------------------
collect_nodes(Ns, N, Type) ->
    CN = maps:get(N, Ns),
    case Type of 
        sequential -> CN#connections.seq;
        recurrent  -> CN#connections.rcc;
        _  -> error(badarg)
    end.

% Finds a path from N1 to N2 ----------------------------------------
seq_path(NN, N1, N2) ->
    OutSeq = nn_node:out_neighbours(N1, sequential),
    one_path(OutSeq, N2, [], [N1], [N1], NN).

one_path([W| _], W,    _,  _, Ps,  _) -> % The path is found
    lists:reverse([W|Ps]); 
one_path([N|Ns], W, Cont, Xs, Ps, NN) -> 
    case lists:member(N, Xs) of
        true  -> % That neuron were evluated before
            one_path(Ns, W, Cont, Xs, Ps, NN);
        false -> % That neuron out neighbours can be check firts
            Nexts = nn_node:out_neighbours(N, sequential),
            one_path(Nexts, W, [{Ns,Ps}|Cont], [N|Xs], [N|Ps], NN)
    end;
one_path([], W, [{Ns,Ps}|Cont], Xs, _, NN) -> % End of neighbours
    one_path(Ns, W, Cont, Xs, Ps, NN);
one_path([], _,             [],  _, _,  _) -> % No seq path
    false.


% Inserts a sequential link on the nodes ----------------------------
insert_seq_link(#network{cn=CN} = NN, N1, N2) ->
    #{N1:= ConnN1, N2:=ConnN2} = CN,
    NN#network{cn = CN#{
        N1 := ConnN1#connections{
            seq = {
                       nn_node:in_neighbours(N1, sequential),
                [N2 | nn_node:out_neighbours(N1, sequential)]
            }},
        N2 := ConnN2#connections{
            seq = {
                [N1 |  nn_node:in_neighbours(N2, sequential)],
                      nn_node:out_neighbours(N2, sequential)
            }}
    }}.

% Inserts a recurrent link on the nodes -----------------------------
insert_rcc_link(#network{type=sequential}, _, _, Path) ->
    error({bad_link, Path});
insert_rcc_link(#network{cn=CN} = NN, N1, N2, _) ->
    #{N1:= ConnN1, N2:=ConnN2} = CN,
    NN#network{cn = CN#{
        N1 := ConnN1#connections{
            rcc = {
                       nn_node:in_neighbours(N1, recurrent),
                [N2 | nn_node:out_neighbours(N1, recurrent)]
            }},
        N2 := ConnN2#connections{
            rcc = {
                [N1 |  nn_node:in_neighbours(N2, recurrent)],
                      nn_node:out_neighbours(N2, recurrent)
            }}
    }}.

% Inserts a recurrent link on the nodes -----------------------------
remove_link(#network{cn=CN} = NN, N1, N2) ->
    #{N1:= ConnN1, N2:=ConnN2} = CN,
    NN#network{cn = CN#{
        N1 := ConnN1#connections{
            seq = { 
                                  nn_node:in_neighbours(N1, recurrent),
                lists:delete(N2, nn_node:out_neighbours(N1, recurrent))
            },
            rcc = { 
                                  nn_node:in_neighbours(N1, recurrent),
                lists:delete(N2, nn_node:out_neighbours(N1, recurrent))
            }},
        N2 := ConnN2#connections{
            seq = {
                lists:delete(N1,  nn_node:in_neighbours(N2, recurrent)),
                                 nn_node:out_neighbours(N2, recurrent)
            },
            rcc = {
                lists:delete(N1,  nn_node:in_neighbours(N2, recurrent)),
                                 nn_node:out_neighbours(N2, recurrent)
            }}
    }}.

