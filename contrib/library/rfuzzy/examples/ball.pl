:- module(ball,_,[rfuzzy, clpr]).

valid_angle(X) :- X .>=. 0, X .=<. 181.

rfuzzy_type_for(selected_angle/1, [valid_angle/1]).
rfuzzy_default_value_for(selected_angle/1,0).

best_angle :# ([(0,0),(50,0),(71,1),(100,0)]).
