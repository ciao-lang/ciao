:- module(trains, _, [rfuzzy, clpr]).

% Activate/Deactivate debug.
% :- activate_rfuzzy_debug.

% Do not use this to define valid natural numbers.
%speed(0). % Units are kilometres per hour
%speed(X) :-
%	speed(Y), number(Y),
%	X .=. Y + 1,
%	(   X =< 500 ; ( X > 500, !, fail) ).
speed(X) :- X .>=. 0.

speed_function :# ([(0, 0), (100, 0.5), (200, 1), (250, 0.5), (300, 0)]).
current_speed(X, X). 
rfuzzy_define_fuzzification(fast/1, current_speed/2, speed_function/2).

rfuzzy_quantifier(not_so/1, under, 0.4).
rfuzzy_quantifier(very/1, over, 0.5).
rfuzzy_quantifier(too_much/1, over, 0.7).

%distance(0). % Units are kilometres.
%distance(X) :-
%	distance(Y), number(Y),
%	X .=. Y + 1,
%	(   X =< 500 ; ( X > 500, !, fail) ).
distance(X) :- X .>=. 0.

far_function :# ([(0, 1), (1, 0.75), (2, 0.5), (3, 0.25), (5, 0)]).
current_distance(X, X). 
rfuzzy_define_fuzzification(far/1, current_distance/2, far_function/2).
rfuzzy_antonym(far/1, close/1, prod, 1).

% D is distance, S is speed.
rfuzzy_type_for(reduce_speed/2, [distance/1, speed/1]).
rfuzzy_default_value_for(reduce_speed/2, 1).
reduce_speed(D, S) :~ prod((far(D), very(fast(S)))).

rfuzzy_type_for(activate_brakes/2, [distance/1, speed/1]).
rfuzzy_default_value_for(activate_brakes/2, 0.5) .
activate_brakes(D, S) :~ max((close(D), reduce_speed(D, S))).

rfuzzy_antonym(activate_brakes/2, disable_brakes/2, prod, 1).
% disable_brakes(D, S) cred (complement, 1) :~ max((activate_brakes(D, S), activate_brakes(D, S))).

rfuzzy_type_for(accelerate/2, [distance/1, speed/1]).
rfuzzy_default_value_for(accelerate/2, 1) .
accelerate(D, S)  :~ min((disable_brakes(D, S), very(far(D)))).


