%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% -TODO: Check if it is better to act as well on the bias.
%%% -TODO: Might be interesting to test in "divide_neuron" to test 
%%%        dividing only the inputs or only the outputs.
%%% @end
%%%-------------------------------------------------------------------
-module(enn_edit).
-compile([export_all, nowarn_export_all]). %%TODO: To delete after build

%% API
-export([]).
-export_type([]).


%%%===================================================================
%%% API: Network transaction edit function
%%%===================================================================

%%-------------------------------------------------------------------
%% @doc Performs in a transaction the operations defined inside the 
%% passed function. This function must be of arity one where the 
%% passed argument is the network itself (not the id) which is always 
%% the first argument or return of the following functions.
%% @end
%%-------------------------------------------------------------------
-spec transaction(Network, Function) -> {atomic, ok} when
    Network  :: network:id(),
    Function :: function().
transaction(ENN_id, Function) -> 
    mnesia:transaction(
        fun() -> 
            [ENN_0] = mnesia:read(network, ENN_id),
             ENN_1  = apply(Function, [ENN_0]),
             ok     = mnesia:write(ENN_1)
        end
    ).


%%%===================================================================
%%% API: Neuron modifications
%%%===================================================================

%%-------------------------------------------------------------------
%% @doc Reinitialises the neuron bias.
%% Should run inside a network transaction.
%% @end
%%-------------------------------------------------------------------
-spec reinitialise_bias(ENN, N) -> ok when 
    ENN :: network:network(),
    N   :: neuron:id().
reinitialise_bias(ENN, N_id) -> 
    [N] = mnesia:read(neuron, N_id),
    ok  = mnesia:write(neuron:bias(N, undefined)),
    ENN.

%%-------------------------------------------------------------------
%% @doc Changes the activation function of a neuron.
%% Should run inside a network transaction.
%% @end
%%-------------------------------------------------------------------
-spec switch_activation(ENN, N, Function_name) -> ok when 
    ENN :: network:network(),
    N   :: neuron:id(),
    Function_name :: activation:func().
switch_activation(ENN, N_id, Func) -> 
    [N] = mnesia:read(neuron, N_id),
    ok  = mnesia:write(neuron:activation(N, Func)),
    ENN.


%%%===================================================================
%%% API: Network modifications by neuron
%%%===================================================================

%%-------------------------------------------------------------------
%% @doc Reinitialises the weights of a neuron. A second parameter 
%% might indicate a random percentage of weights to reinitialise.
%% Should run inside a network transaction.
%% @end
%%-------------------------------------------------------------------
-spec reinitialise_weights(ENN, N) -> ok when 
    ENN :: network:network(),
    N   :: neuron:id().
reinitialise_weights(ENN, N_id) -> 
    Links = network:in_links(ENN, N_id),
    [link:delete(Link) || Link <- Links],
    ENN.

-spec reinitialise_weights(Network, N, Percentage) -> ok when 
    Network    :: enn:id(),
    N          :: neuron:id(),
    Percentage :: float().
reinitialise_weights(ENN, N_id, Percentage) -> 
    Links = ltools:rand(network:in_links(ENN, N_id), Percentage),
    [link:delete(Link) || Link <- Links],
    ENN.

%%-------------------------------------------------------------------
%% @doc Splits a neuron into 2 sharing the inputs and outpus. The 
%% bias, activation/aggregation functions are cloned. 
%% Should run inside a network transaction.
%% @end
%%-------------------------------------------------------------------
-spec divide_neuron(ENN, N) -> ok when 
    ENN :: network:network(),
    N   :: neuron:id().
divide_neuron(ENN, N1_id) -> 
    [N1] = mnesia:read(neuron, N1_id),
     N2  = neuron:clone(N1),
     ok  = mnesia:write(N2),
    Map = #{N1_id => neuron:id(N2)},
    OutMove = ltools:rand(network:out_links(ENN, N1_id), 0.5),
    [ok = link:move(Link, Map) || Link <- OutMove],
    InMove  = ltools:rand(network:in_links( ENN, N1_id), 0.5),
    [ok = link:move(Link, Map) || Link <- InMove ],
    network:move_links(
        network:add_neuron(ENN, neuron:id(N2)),
        lists:append(InMove, OutMove), Map).

%%-------------------------------------------------------------------
%% @doc Merges all inputs and outputs from neuron 1 into neuron 2, 
%% the bias, activation/aggregation functions from neuron 2 prevail.
%% Should run inside a network transaction.
%% @end
%%-------------------------------------------------------------------
-spec merge_neurons(ENN, N1, N2) -> ok when 
    ENN :: network:network(),
    N1  :: neuron:id(),
    N2  :: neuron:id().
merge_neurons(ENN, N1_id, N2_id) -> 
    Map = #{N1_id => N2_id},
    [link:merge(Link, Map) || Link <- network:out_links(ENN, N1_id)],
    [link:merge(Link, Map) || Link <- network:in_links( ENN, N1_id)],
    ok = mnesia:delete(neuron, N1_id, write),
    network:del_neuron( 
        network:move_links(ENN, network:links(ENN, N1_id), Map),
        N1_id).


%%%===================================================================
%%% API: Network modifications by groups
%%%===================================================================

%%-------------------------------------------------------------------
%% @doc Creates links from all neurons in From to all neurons in To.
%% If the link existed alreay, it is not modified.
%% Should run inside a network transaction.
%% @end
%%-------------------------------------------------------------------
-spec connect_all(ENN, From, To) -> ok when 
    ENN  :: network:network(),
    From :: [neuron:id()],
    To   :: [neuron:id()].
connect_all(ENN, From, To) -> 
    network:add_links(ENN, [{N1,N2} || N1 <- From, N2 <- To]).

%%-------------------------------------------------------------------
%% @doc Creates only the allowed links from neurons in From to To.
%% Should run inside a network transaction.
%% @end
%%-------------------------------------------------------------------
-spec connect_allowed(ENN, From, To) -> ok when 
    ENN  :: network:network(),
    From :: [neuron:id()],
    To   :: [neuron:id()].
connect_allowed(ENN, From, To) -> 
    connect_allowed(ENN, [{N1,N2} || N1 <- From, N2 <- To]).

connect_allowed(ENN_0, [L|Lx]) -> 
    try network:add_link(ENN_0, L) of 
          ENN_1               -> connect_allowed(ENN_1, Lx)
    catch error:{bad_link, _} -> connect_allowed(ENN_0, Lx)
    end;
connect_allowed(ENN, []) -> ENN.

%%-------------------------------------------------------------------
%% @doc Deletes links from all neurons in From to all neurons in To.
%% If the link did not existed nothing happens.
%% Should run inside a network transaction.
%% @end
%%-------------------------------------------------------------------
-spec disconnect_all(ENN, From, To) -> ok when 
    ENN  :: network:network(),
    From :: [neuron:id()],
    To   :: [neuron:id()].
disconnect_all(ENN, From, To) -> 
    network:del_links(ENN, [{N1,N2} || N1 <- From, N2 <- To]).


%%%===================================================================
%%% API: Network modifications
%%%===================================================================

%%-------------------------------------------------------------------
%% @doc Increasses the connections sqrt/proportionally to the size
%% Should run inside a network transaction.
%% @end
%%-------------------------------------------------------------------
increase_connections(ENN) ->
    #{size:=Size} = network:info(ENN),
    Size_factor   = math:sqrt(Size),
    From = ltools:rand(network:neurons(ENN), 1/Size_factor),
    To   = ltools:rand(network:neurons(ENN), 1/Size_factor),
    connect_allowed(ENN, From, To).

%%-------------------------------------------------------------------
%% @doc Decreasses the connections sqrt/proportionally to the size.
%% Should run inside a network transaction.
%% @end
%%-------------------------------------------------------------------
reduce_connections(ENN, From, To) -> 
    #{size:=Size} = network:info(ENN),
    Size_factor   = math:sqrt(Size),
    From = ltools:rand(network:neurons(ENN), 1/Size_factor),
    To   = ltools:rand(network:neurons(ENN), 1/Size_factor),
    disconnect_all(ENN, From, To).


%%====================================================================
%% Internal functions
%%====================================================================


%%====================================================================
%% Eunit white box tests
%%====================================================================

% --------------------------------------------------------------------
% TESTS DESCRIPTIONS -------------------------------------------------

% --------------------------------------------------------------------
% SPECIFIC SETUP FUNCTIONS -------------------------------------------

% --------------------------------------------------------------------
% ACTUAL TESTS -------------------------------------------------------

% --------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS ------------------------------------------

