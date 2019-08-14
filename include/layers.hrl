%%%-------------------------------------------------------------------
%%% @author borja
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Feb 2019 17:31
%%%-------------------------------------------------------------------

-define(dense(Units, Properties), layer:dense(Units, Properties)).
-define(dense(Units), layer:dense(Units, #{})).

-define(input(Units, Properties), layer:input(Units, Properties)).
-define(input(Units), layer:input(Units, #{})).

-define(output(Units, Properties), layer:output(Units, Properties)).
-define(output(Units), layer:output(Units, #{})).



