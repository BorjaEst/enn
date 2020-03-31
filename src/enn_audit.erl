%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%% 
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(enn_audit).
-author("borja").
-compile([export_all, nowarn_export_all]). %%TODO: To delete after build

%% API
-export([]).


%%%===================================================================
%%% Audit filters
%%%===================================================================

%%% ==== metadata ====================================================
%%%   #{pid       => pid(),
%%%     gl        => pid(),
%%%     time      => timestamp(),
%%%     mfa       => {module(), atom(), integer() >= 0},
%%%     file      => file:filename(),
%%%     line      => integer() >= 0,
%%%     domain    => [atom()],
%%%     report_cb => report_cb(),
%%%     atom()    => term()}

%%-------------------------------------------------------------------
%% @doc Discards any message comming from any of those modules.
%% @end
%-------------------------------------------------------------------
-spec not_from_modules(Metadata, Modules) -> Result when
    Metadata :: logger:metadata(),
    Modules  :: [atom()],
    Result   :: logger:filter_return().
not_from_modules(#{meta:=#{mfa:={M,_,_}}},[M| _]) -> 
    stop;
not_from_modules(                LogEvent,[_|Mx]) -> 
    not_from_modules(LogEvent, Mx);
not_from_modules(                LogEvent,    []) -> 
    LogEvent.

%%-------------------------------------------------------------------
%% @doc Passes logs comming only from any of these modules.
%% @end
%-------------------------------------------------------------------
-spec only_from_modules(Metadata, Modules) -> Result when
    Metadata :: logger:metadata(),
    Modules  :: [atom()],
    Result   :: logger:filter_return().
only_from_modules(#{meta:=#{mfa:={M,_,_}}}= LogEvent,[M| _]) -> 
    LogEvent;
only_from_modules(                          LogEvent,[_|Mx]) -> 
    only_from_modules(LogEvent, Mx);
only_from_modules(                                 _,    []) -> 
    stop.


%%%===================================================================
%%% Report call backs
%%%===================================================================

report_in_csv(#{what:=What, pid:=Pid, id:=Id, details:=Details}) -> 
    {";~0p;~0p;~0p;~0p;~n", [Pid, What, Id, Details]}.
