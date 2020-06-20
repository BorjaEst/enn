%%%-------------------------------------------------------------------
%%% @doc Macro for easy layer definition.
%%% @end
%%%-------------------------------------------------------------------

-define(  input(Units, Connections),   layer:input(Units, Connections)).
-define( output(Units, Connections),  layer:output(Units, Connections)).
-define(  dense(Units, Connections),   layer:dense(Units, Connections)).
-define(sigmoid(Units, Connections), layer:sigmoid(Units, Connections)).
-define(    elu(Units, Connections),     layer:elu(Units, Connections)).
-define(   tanh(Units, Connections),    layer:tanh(Units, Connections)).

