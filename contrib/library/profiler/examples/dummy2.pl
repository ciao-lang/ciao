:- module(dummy2, _, [profiler, expander]).
%:- module(dummy2,_,[]).
:- use_module(library(profiler/profiler_utils)).

choice1(x) :- display(choice1_x), nl.
choice1(y) :- display(choice1_y), nl.
choice1(z) :- display(choice1_z), nl.
choice1(k) :- display(choice1_k), nl.
choice1(l) :- display(choice1_l), nl.
choice1(m) :- display(choice1_m), nl.

choice2(_, x) :- display(choice2_x), nl.
choice2(_, y) :- display(choice2_y), nl.
choice2(_, z) :- display(choice2_z), nl.
choice2(_, k) :- display(choice2_k), nl.
choice2(_, l) :- display(choice2_l), nl.
choice2(_, m) :- display(choice2_m), nl.

list1 :-
	choice1(A),
	display(A), nl,
	fail.
list1.

list2 :-
	choice2(_, A),
	display(A), nl,
	fail.
list2.

main0 :-
	list1,
	list2,
	choice1(l),
	choice2(_, l).
%	findall(A,choice1(A),L),
%	display(L).

t0 :-
	profile_reset, main0, profile_dump.
