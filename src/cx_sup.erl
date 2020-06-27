%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
-module(cx_sup).
-behaviour(supervisor).

%% API
-export([id/1, start_link/1]).
-export_type([id/0]).

%% Supervisor callbacks
-export([init/1]).

-type id() :: {cx_sup, Ref :: reference()}.

-define(SPECS_NN_SUP(Network), #{
    id       => nn_sup:id(Network),
    start    => {nn_sup, start_link, [Network]},
    restart  => permanent,
    type     => supervisor,
    modules  => [supervisor]
}).

-define(SPECS_CORTEX(Network), #{
    id       => cortex:id(Network),
    start    => {cortex, start_link, [Network]},
    restart  => permanent,
    shutdown => 1000,
    modules  => [gen_statem]
}).


%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns the supervisor id of from a network id. 
%% @end
%%--------------------------------------------------------------------
-spec id(Network :: netwrok:id()) -> Supervisor :: id().
id(Network) -> {cx_sup, element(2, Network)}.

%%--------------------------------------------------------------------
%% @doc Starts the supervisor
%% @end
%%--------------------------------------------------------------------
% TODO: To make description and specs
start_link(Network) ->
    supervisor:start_link(?MODULE, [Network]).


%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([Network]) ->
    SupFlags = #{strategy  => one_for_all, %% All down if one down
                 intensity => 0,   %% Restart is not allowed
                 period    => 10}, %% Any as intensity = 0
    ChildSpecs = [
        ?SPECS_NN_SUP(Network),
        ?SPECS_CORTEX(Network)
    ],
    true = nn_pool:new(Network),
    true = enn_pool:register_as_cx_supervisor(Network),
    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

