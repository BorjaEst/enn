%%%-------------------------------------------------------------------
%% @doc enn top level supervisor.
%%
%% TODO: Use a non named_table for the nn_pool
%% @end
%%%-------------------------------------------------------------------
-module(enn_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_nn/1, terminate_nn/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-define(SPECS_CX_SUP(Network), #{
    id       => cx_sup:id(Network),
    start    => {cx_sup, start_link, [Network]},
    restart  => temporary,
    type     => supervisor,
    modules  => [supervisor]
}).

%%====================================================================
%% API functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts the supervisor
%% @end
%%--------------------------------------------------------------------
% TODO: To make description and specs
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%--------------------------------------------------------------------
%% @doc Starts the neural network supervisor
%% @end
%%--------------------------------------------------------------------
% TODO: To make description and specs
start_nn(Network) ->
    true = enn_pool:register(Network),
    supervisor:start_child(?SERVER, ?SPECS_CX_SUP(Network)).

%%--------------------------------------------------------------------
%% @doc Stops the neural network supervisor
%% @end
%%--------------------------------------------------------------------
% TODO: To make description and specs
terminate_nn(Network) ->
    true = enn_pool:unregister(Network),
    supervisor:terminate_child(?SERVER, cx_sup:id(Network)).


%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    SupFlags = #{strategy  => one_for_all,
                 intensity => 10,
                 period    => 36},
    ChildSpecs = [],
    enn_pool:start(),
    {ok, {SupFlags, ChildSpecs}}.

    
%%====================================================================
%% Internal functions
%%====================================================================

