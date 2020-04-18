%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(nn_node).

-export([new/0, info/1]).

-export([add_sequential_in/2, add_sequential_out/2]).
-export([ add_recurrent_in/2,  add_recurrent_out/2]).
-export([remove_sequential_in/2, remove_sequential_out/2]).
-export([ remove_recurrent_in/2,  remove_recurrent_out/2]).

-export([is_sink/1, is_bias/1]).
-export([out_neighbours/1, out_neighbours/2, out_degree/1]).
-export([ in_neighbours/1,  in_neighbours/2,  in_degree/1]).

-export_type([connections/0]).

-record(inout, {
    in  = [] :: [network:d_node()],
    out = [] :: [network:d_node()]
}).
-type inout() :: #inout{}.
-define( IN(InOut), InOut#inout.in).
-define(OUT(InOut), InOut#inout.out).

-record(connections, {
    seq = #inout{} :: inout(),
    rcc = #inout{} :: inout()
}).
-type connections() :: #connections{}.
-define(SEQ(Connections), Connections#connections.seq).
-define(RCC(Connections), Connections#connections.rcc).


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
%% @doc Adds a node id to the sequential-in connections.  
%% @end
%%-------------------------------------------------------------------
-spec add_sequential_in(Connections, N) -> connections() when
    Connections :: connections(),
    N :: network:d_node().
add_sequential_in(Connections, N) ->
    Connections#connections{seq = add_in(?SEQ(Connections), N)}.
 
%%-------------------------------------------------------------------
%% @doc Adds a node id to the sequential-out connections.  
%% @end
%%-------------------------------------------------------------------
-spec add_sequential_out(Connections, N) -> connections() when
    Connections :: connections(),
    N :: network:d_node().
add_sequential_out(Connections, N) ->
    Connections#connections{seq = add_out(?SEQ(Connections), N)}.

%%-------------------------------------------------------------------
%% @doc Adds a node id to the recurrent-in connections.  
%% @end
%%-------------------------------------------------------------------
-spec add_recurrent_in(Connections, N) -> connections() when
    Connections :: connections(),
    N :: network:d_node().
add_recurrent_in(Connections, N) ->
    Connections#connections{rcc = add_in(?RCC(Connections), N)}.
 
%%-------------------------------------------------------------------
%% @doc Adds a node id to the recurrent-out connections.  
%% @end
%%-------------------------------------------------------------------
-spec add_recurrent_out(Connections, N) -> connections() when
    Connections :: connections(),
    N :: network:d_node().
add_recurrent_out(Connections, N) ->
    Connections#connections{rcc = add_out(?RCC(Connections), N)}.

%%-------------------------------------------------------------------
%% @doc Removes a node id from the sequential-in connections.  
%% @end
%%-------------------------------------------------------------------
-spec remove_sequential_in(Connections, N) -> connections() when
    Connections :: connections(),
    N :: network:d_node().
remove_sequential_in(Connections, N) ->
    Connections#connections{seq = remove_in(?SEQ(Connections), N)}.
 
%%-------------------------------------------------------------------
%% @doc Removes a node id from the sequential-out connections.  
%% @end
%%-------------------------------------------------------------------
-spec remove_sequential_out(Connections, N) -> connections() when
    Connections :: connections(),
    N :: network:d_node().
remove_sequential_out(Connections, N) ->
    Connections#connections{seq = remove_out(?SEQ(Connections), N)}.

%%-------------------------------------------------------------------
%% @doc Removes a node id from the recurrent-in connections.  
%% @end
%%-------------------------------------------------------------------
-spec remove_recurrent_in(Connections, N) -> connections() when
    Connections :: connections(),
    N :: network:d_node().
remove_recurrent_in(Connections, N) ->
    Connections#connections{rcc = remove_in(?RCC(Connections), N)}.
 
%%-------------------------------------------------------------------
%% @doc Removes a node id from the recurrent-out connections.  
%% @end
%%-------------------------------------------------------------------
-spec remove_recurrent_out(Connections, N) -> connections() when
    Connections :: connections(),
    N :: network:d_node().
remove_recurrent_out(Connections, N) ->
    Connections#connections{rcc = remove_out(?RCC(Connections), N)}.

%%-------------------------------------------------------------------
%% @doc Returns true if no outputs, otherwise false.  
%% @end
%%-------------------------------------------------------------------
-spec is_sink(Connections :: connections()) -> boolean().
is_sink(#connections{seq={_,[]},rcc={_,[]}}) -> true;
is_sink(#connections{})                      -> false.

%%-------------------------------------------------------------------
%% @doc Returns true if no inputs, otherwise false.   
%% @end
%%-------------------------------------------------------------------
-spec is_bias(Connections :: connections()) -> boolean().
is_bias(#connections{seq={[],_},rcc={[],_}}) -> true;
is_bias(#connections{})                      -> false.

%%-------------------------------------------------------------------
%% @doc Returns the in-degree of connections.  
%% @end
%%-------------------------------------------------------------------
-spec in_degree(Connections) -> non_neg_integer() when
      Connections :: connections().
in_degree(Connections) ->
    length(in_neighbours(Connections)).

%%-------------------------------------------------------------------
%% @doc Returns the out-degree of connections.  
%% @end
%%-------------------------------------------------------------------
-spec out_degree(Connections) -> non_neg_integer() when
      Connections :: connections().
out_degree(Connections) ->
    length(out_neighbours(Connections)).

%%-------------------------------------------------------------------
%% @doc Returns a list of all in-neighbors of N connections. 
%% @end
%%-------------------------------------------------------------------
-spec in_neighbours(Connections) -> Nodes when
      Connections :: connections(),
      Nodes :: [network:d_node()].
in_neighbours(Connections) ->
    in_neighbours(Connections, sequential) ++ 
    in_neighbours(Connections,  recurrent).

-spec in_neighbours(Connections, Type) -> Nodes when
      Connections :: connections(),
      Type  :: network:d_type(),
      Nodes :: [network:d_node()].
in_neighbours(#connections{seq=Seq}, sequential) -> ?IN(Seq);
in_neighbours(#connections{rcc=Rcc},  recurrent) -> ?IN(Rcc).

%%-------------------------------------------------------------------
%% @doc Returns a list of all out-neighbors of N connections. 
%% @end
%%-------------------------------------------------------------------
-spec out_neighbours(Connections) -> Nodes when
      Connections :: connections(),
      Nodes :: [network:d_node()].
out_neighbours(Connections) ->
    out_neighbours(Connections, sequential) ++ 
    out_neighbours(Connections,  recurrent).

-spec out_neighbours(Connections, Type) -> Nodes when
      Connections :: connections(),
      Type  :: network:d_type(),
      Nodes :: [network:d_node()].
out_neighbours(#connections{seq=Seq}, sequential) -> ?OUT(Seq);
out_neighbours(#connections{rcc=Rcc},  recurrent) -> ?OUT(Rcc).


%%====================================================================
%% Internal functions
%%====================================================================

%% Adds and input to the inout record -------------------------------
add_in(#inout{} = InOut, N) ->  
    InOut#inout{ in=[N|InOut#inout.in ]}.

%% Adds and output to the inout record ------------------------------
add_out(#inout{} = InOut, N) ->  
    InOut#inout{out=[N|InOut#inout.out]}.

%% Removes and input from the inout record --------------------------
remove_in(#inout{} = InOut, N) ->  
    InOut#inout{ in=lists:delete(N, InOut#inout.in )}.

%% Removes and output from the inout record -------------------------
remove_out(#inout{} = InOut, N) ->  
    InOut#inout{out=lists:delete(N, InOut#inout.out)}.

