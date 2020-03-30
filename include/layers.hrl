%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-define(dense(Units, Properties), layer:dense(Units, Properties)).
-define(dense(Units), layer:dense(Units, #{})).

-define(input(Units, Properties), layer:input(Units, Properties)).
-define(input(Units), layer:input(Units, #{})).

-define(output(Units, Properties), layer:output(Units, Properties)).
-define(output(Units), layer:output(Units, #{})).



