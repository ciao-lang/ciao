:- module(age, _, [rfuzzy, clpr]).
:- use_module(library(write),[write/1]).

age(X) :- X .>=. 0, X .<. 150.
% Do not use this to define valid natural numbers.
% age(0).
% age(X) :-
%	age(Y),
%	number(Y),
%	X .=. Y + 1,
%	(   X < 150 ;
%	    ( X >= 150, !, fail)
%	).

age_of(victor, 30).
age_of(samuel, 32).
age_of(oscar, 28).
age_of(alejandro, 12).

amount_of_descendants(victor, 0).
amount_of_descendants(susana, 2).
has_children(Person) :- amount_of_descendants(Person, X), X .>. 0.


child_age_function :# ([ (0, 1), (10, 1), (20, 0) ]).
teenager_age_function :# ([ (9, 0), (10, 1) , (19, 1), (20, 0) ]).
young_age_function :# ([ (20, 1), (30, 0.5) ]). 

rfuzzy_type_for(child/1,  [age/1]).
rfuzzy_default_value_for(child/1, 0).
rfuzzy_define_fuzzification(child/1, age_of/2, child_age_function/2).
child(alejandro) value 1.

rfuzzy_type_for(teenager/1, [age/1]).
rfuzzy_default_value_for(teenager/1, 0).
rfuzzy_define_fuzzification(teenager/1, age_of/2, teenager_age_function/2).

rfuzzy_type_for(young/1, [age/1]).
rfuzzy_default_value_for(young/1, 0).
rfuzzy_define_fuzzification(young/1, age_of/2, young_age_function/2).
% I'll be young forever, rules do not apply to me ... ;-)
young(victor) value 1.
% Susana says the same, but she is not as young as me ... ;-)
young(susana) value 0.9.

rfuzzy_type_for(adult/1, [age/1]).
rfuzzy_default_value_for(adult/1, 0.5).
rfuzzy_default_value_for(adult/1, 0.7) if has_children/1.

rfuzzy_type_for(old/1, [age/1]).
rfuzzy_default_value_for(old/1, 0.5).
rfuzzy_antonym(young/1, old/1, prod, 1).

test_child :- child(X,V), test_print(X,V).
test_young :- young(X,V), test_print(X,V).
test_adult :- adult(X,V), test_print(X,V).
test_old :- old(X,V), test_print(X,V).

test_print(X, V) :- write(X), write(' -> '), write(V), write('   '), nl, fail.

%:- use_module(library(write),[write/1]).
test(X) :- X .>. 0.8, 
	write(X), nl.
%	functor(X, Name, Arity), 
%	write(Name),  nl,
%	write(Arity), nl, 
%	X =..[Name|Args],
%	write(Args), nl.
