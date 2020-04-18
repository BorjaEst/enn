%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(network).

-export([new/0, info/1]).

-export([node/2, no_neurons/1, neurons/1]).
-export([is_sink/1, is_bias/1]).

-export([add_link/3, add_links/3, del_link/3, del_links/3]).
-export([no_links/1, links/1, links/2]).

-export([out_neighbours/2, out_neighbours/3]).
-export([ in_neighbours/2,  in_neighbours/3]).
-export([out_degree/2, out_links/2, out_links/3]). 
-export([ in_degree/2,  in_links/2,  in_links/3]).

-export_type([connections/0, d_type/0, link/0]).

-record(connections, {
    seq = {[],[]} :: {In :: [d_node()], Out :: [d_node()]},
    rcc = {[],[]} :: {In :: [d_node()], Out :: [d_node()]}
}).
-type connections() :: #connections{}.
-define( IN(InOut), element(1, InOut)).
-define(OUT(InOut), element(2, InOut)).


%%%===================================================================
%%% API
%%%==================================================================

%%-------------------------------------------------------------------
%% @doc Creates a new nn_node connections.  
%% @end
%%-------------------------------------------------------------------
-spec new() -> connections().
new() -> #connections{}.

%%-------------------------------------------------------------------
%% @doc Information about the connection.  
%% @end
%%-------------------------------------------------------------------
-spec info(Connections) -> InfoList when
    Connections :: connections(),
    InfoList :: #{'sequential' => InOut,
                  'recurrent'  => InOut},
    InOut    :: #{'inputs'     => [network:d_node()],
                  'outputs'    => [network:d_node()]}.
info(#connections{} = Conn) -> #{
    sequential => #{
        inputs  =>  ?IN(Conn#connections.seq),
        outputs => ?OUT(Conn#connections.seq)
    },
    recurrent => #{
        inputs  =>  ?IN(Conn#connections.rcc),
        outputs => ?OUT(Conn#connections.rcc)
    }}.

%%-------------------------------------------------------------------
%% @doc Returns true if no outputs, otherwise false.  
%% @end
%%-------------------------------------------------------------------
-spec is_sink(Connections :: connections()) -> boolean().
is_sink(#connections{seq={_,[]},rcc={_,[]}}) -> true;
is_sink(#connections{})                      -> false.

%%-------------------------------------------------------------------
%% @doc Returns all neurons in the network without inputs.  
%% @end
%%-------------------------------------------------------------------
-spec is_bias(Connections :: connections()) -> boolean().
is_bias(#connections{seq={[],_},rcc={[],_}}) -> true;
is_bias(#connections{})                      -> false.

%%-------------------------------------------------------------------
%% @doc Returns the in-degree of neuron N of network Connections.  
%% @end
%%-------------------------------------------------------------------
-spec in_degree(Connections) -> non_neg_integer() when
      Connections :: connections().
in_degree(Connections) ->
    length(in_neighbours(Connections)).

%%-------------------------------------------------------------------
%% @doc Returns the out-degree of neuron N of network Connections.  
%% @end
%%-------------------------------------------------------------------
-spec out_degree(Connections) -> non_neg_integer() when
      Connections :: connections().
out_degree(Connections) ->
    length(out_neighbours(Connections)).

%%-------------------------------------------------------------------
%% @doc Returns a list of all in-neighbors of N of network Connections. 
%% @end
%%-------------------------------------------------------------------
-spec in_neighbours(Connections) -> Nodes when
      Connections :: connections(),
      Nodes :: [d_node()].
in_neighbours(Connections) ->
    in_neighbours(Connections, sequential) ++ 
    in_neighbours(Connections,  recurrent).

-spec in_neighbours(Connections, Type) -> Nodes when
      Connections :: connections(),
      Type  :: d_type(),
      Nodes :: [d_node()].
in_neighbours(#connections{seq=Seq}, sequential) -> ?IN(Seq);
in_neighbours(#connections{rcc=Rcc},  recurrent) -> ?IN(Rcc).

%%-------------------------------------------------------------------
%% @doc Returns a list of all out-neighbors of N of network Connections. 
%% @end
%%-------------------------------------------------------------------
-spec out_neighbours(Connections) -> Nodes when
      Connections :: connections(),
      Nodes :: [d_node()].
out_neighbours(Connections) ->
    out_neighbours(Connections, sequential) ++ 
    out_neighbours(Connections,  recurrent).

-spec out_neighbours(Connections, Type) -> Nodes when
      Connections :: connections(),
      Type  :: d_type(),
      Nodes :: [d_node()].
out_neighbours(#connections{seq=Seq}, sequential) -> ?OUT(Seq);
out_neighbours(#connections{rcc=Rcc},  recurrent) -> ?OUT(Rcc).



















%%-------------------------------------------------------------------
%% @doc Returns all links incident on N of network Connections. 
%% @end
%%-------------------------------------------------------------------
-spec in_links(Connections, N) -> Links when
      Connections :: connections(),
      N  :: d_node(),
      Links :: [link()].
in_links(Connections, N2) ->
    [{N1, N2} || N1 <- in_neighbours(Connections, N2)].

-spec in_links(Connections, N, Type) -> Links when
      Connections :: connections(),
      N  :: d_node(),
      Type  :: d_type(),
      Links :: [link()].
in_links(Connections, N2, Type) -> 
    [{N1, N2} || N1 <- in_neighbours(Connections, N2, Type)].

%%-------------------------------------------------------------------
%% @doc Returns all links emanating from N of network Connections. 
%% @end
%%-------------------------------------------------------------------
-spec out_links(Connections, N) -> Links when
      Connections :: connections(),
      N  :: d_node(),
      Links :: [link()].
out_links(Connections, N1) ->
    [{N1, N2} || N2 <- out_neighbours(Connections, N1)].

-spec out_links(Connections, N, Type) -> Links when
      Connections   :: network(),
      N    :: d_node(),
      Type  :: d_type(),
      Links :: [link()].
out_links(Connections, N1, Type) ->
    [{N1, N2} || N2 <- out_neighbours(Connections, N1, Type)].

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
add_link(Connections, N1, N2) when N1 =:= N2 ->
    insert_rcc_link(Connections, N1, N2, [N1,N2]);
add_link(Connections, N1, N2) ->
    case seq_path(Connections, N2, N1) of
        false -> insert_seq_link(Connections, N1, N2);
        Path  -> insert_rcc_link(Connections, N1, N2, Path)
    end.

%%-------------------------------------------------------------------
%% @doc Creates (or modifies) a link between N1 and N2. 
%%
%% TODO: Take into accound 'start' and 'end'
%% @end
%%------------------------------------------------------------------
-spec add_links(NN1, Ns1, Ns2) -> NN2 when
      NN1 :: network(),
      Ns1 :: [d_node()],
      Ns2 :: [d_node()],
      NN2 :: network().
add_links(Connections, Ns1, Ns2) ->
   add_links(Connections, Ns1, Ns1, Ns2, Ns2). 

add_links(Connections, [N1|Nx1], Ns1, [N2|Nx2], Ns2) -> 
    add_links(add_link(Connections,N1,N2), [N1|Nx1], Ns1, Nx2, Ns2);
add_links(Connections, [_|Nx1], Ns1, [], Ns2) -> 
    add_links(Connections, Nx1, Ns1, Ns2, Ns2);
add_links(Connections, [], _, _, _) -> Connections.

%%-------------------------------------------------------------------
%% @doc Deletes the link between N1 and N2. 
%% @end
%%-------------------------------------------------------------------
-spec del_link(Connections, N1, N2) -> 'true' when
      Connections :: connections(),
      N1 :: d_node(),
      N2 :: d_node().
del_link(Connections, N1, N2) ->
    remove_link(Connections, N1, N2).

%%-------------------------------------------------------------------
%% @doc Deletes the links using lists of neurons. 
%% @end
%%-------------------------------------------------------------------
-spec del_links(NN1, Ns1, Ns2) -> NN2 when
      NN1 :: network(),
      Ns1 :: [d_node()],
      Ns2 :: [d_node()],
      NN2 :: network().
del_links(Connections, Ns1, Ns2) -> 
    del_links(Connections, Ns1, Ns1, Ns2, Ns2).

del_links(Connections, [N1|Nx1], Ns1, [N2|Nx2], Ns2) -> 
    del_links(del_link(Connections,N1,N2), [N1|Nx1], Ns1, Nx2, Ns2);
del_links(Connections, [_|Nx1], Ns1, [], Ns2) -> 
    del_links(Connections, Nx1, Ns1, Ns2, Ns2);
del_links(Connections, [], _, _, _) -> Connections.

%%-------------------------------------------------------------------
%% @doc Returs the number of links in the network. 
%% @end
%%-------------------------------------------------------------------
-spec no_links(Connections) -> non_neg_integer() when
      Connections :: connections().
no_links(Connections) ->
    length(links(Connections)).

%%-------------------------------------------------------------------
%% @doc Returs all network links. 
%% @end
%%-------------------------------------------------------------------
-spec links(Connections) -> Links when
      Connections :: connections(),
      Links :: [link()].
links(#network{cn=CN} = Connections) ->
    [{N1,N2} || N1<-maps:keys(CN), N2<-out_neighbours(Connections, N1)].

%%-------------------------------------------------------------------
%% @doc Returs all the neuron links. 
%% @end
%%-------------------------------------------------------------------
-spec links(Connections, N) -> Links when
      Connections :: connections(),
      N  :: d_node(),
      Links :: [link()].
links(Connections, N) ->
    [{N1,N} || N1 <-  in_neighbours(Connections, N)] ++ 
    [{N,N2} || N2 <- out_neighbours(Connections, N)].


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
seq_path(Connections, N1, N2) ->
    OutSeq = out_neighbours(Connections, N1, sequential),
    one_path(OutSeq, N2, [], [N1], [N1], Connections).

one_path([W| _], W,    _,  _, Ps,  _) -> % The path is found
    lists:reverse([W|Ps]); 
one_path([N|Ns], W, Cont, Xs, Ps, Connections) -> 
    case lists:member(N, Xs) of
        true  -> % That neuron were evluated before
            one_path(Ns, W, Cont, Xs, Ps, Connections);
        false -> % That neuron out neighbours can be check firts
            Nexts = out_neighbours(Connections, N, sequential),
            one_path(Nexts, W, [{Ns,Ps}|Cont], [N|Xs], [N|Ps], Connections)
    end;
one_path([], W, [{Ns,Ps}|Cont], Xs, _, Connections) -> % End of neighbours
    one_path(Ns, W, Cont, Xs, Ps, Connections);
one_path([], _,             [],  _, _,  _) -> % No seq path
    false.


% Inserts a sequential link on the nodes ----------------------------
insert_seq_link(#network{cn=CN} = Connections, N1, N2) ->
    #{N1:= ConnN1, N2:=ConnN2} = CN,
    Connections#network{cn = CN#{
        N1 := ConnN1#connections{
            seq = {
                       in_neighbours(Connections, N1, sequential),
                [N2 | out_neighbours(Connections, N1, sequential)]
            }},
        N2 := ConnN2#connections{
            seq = {
                [N1 |  in_neighbours(Connections, N2, sequential)],
                      out_neighbours(Connections, N2, sequential)
            }}
    }}.

% Inserts a recurrent link on the nodes -----------------------------
insert_rcc_link(#network{type=sequential}, _, _, Path) ->
    error({bad_link, Path});
insert_rcc_link(#network{cn=CN} = Connections, N1, N2, _) ->
    #{N1:= ConnN1, N2:=ConnN2} = CN,
    Connections#network{cn = CN#{
        N1 := ConnN1#connections{
            rcc = {
                       in_neighbours(Connections, N1, recurrent),
                [N2 | out_neighbours(Connections, N1, recurrent)]
            }},
        N2 := ConnN2#connections{
            rcc = {
                [N1 |  in_neighbours(Connections, N2, recurrent)],
                      out_neighbours(Connections, N2, recurrent)
            }}
    }}.

% Inserts a recurrent link on the nodes -----------------------------
remove_link(#network{cn=CN} = Connections, N1, N2) ->
    #{N1:= ConnN1, N2:=ConnN2} = CN,
    Connections#network{cn = CN#{
        N1 := ConnN1#connections{
            seq = { 
                                  in_neighbours(Connections, N1, recurrent),
                lists:delete(N2, out_neighbours(Connections, N1, recurrent))
            },
            rcc = { 
                                  in_neighbours(Connections, N1, recurrent),
                lists:delete(N2, out_neighbours(Connections, N1, recurrent))
            }},
        N2 := ConnN2#connections{
            seq = {
                lists:delete(N1,  in_neighbours(Connections, N2, recurrent)),
                                 out_neighbours(Connections, N2, recurrent)
            },
            rcc = {
                lists:delete(N1,  in_neighbours(Connections, N2, recurrent)),
                                 out_neighbours(Connections, N2, recurrent)
            }}
    }}.

