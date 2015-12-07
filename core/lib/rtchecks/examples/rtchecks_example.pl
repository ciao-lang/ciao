:- module(rtchecks_example, [
		is_animal/1,
		animal/1,
		animals/1,
		tcollapse/2,
		create_pairwise_mutex_classes/3,
		call_mf/0,
		must_fail/1,
		must_not_fail/0,
		animal_type/1,
		what_kind/2,
		show_if_domestic/1,
		display_any/1,
		get_domestic_list/1,
		display_any2/1,
		test_any/1,
		test2/2,
		test3/2,
		test4/2,
		test5/2,
		test_det/2,
		test_det2/1],
	    [assertions, basicmodes, isomodes, nativeprops, hiord,
		regtypes, rtchecks, expander]).

:- use_module(library(aggregates)).

:- doc(author, "Edison Mera").

:- doc(module, "Examples of assertions that can be processed by
	the run-time checker.").

:- set_prolog_flag(rtchecks_inline,  yes).
:- set_prolog_flag(rtchecks_predloc, yes).
:- set_prolog_flag(rtchecks_callloc, predicate).
:- set_prolog_flag(rtchecks_asrloc,  yes).

%:- check pred is_animal(+animal(A)).

:- check pred is_animal(-Animal) :: animal(Animal) + (not_fails, non_det)
	# "This is a way to call is_animal/1".

:- check pred is_animal(+Anim) :: animal(Anim) + (not_fails, is_det).
% :- check pred is_animal(+Anim) :: animal(Anim) + (not_fails).

:- test is_animal(An) : (An = dogg) + not_fails.

is_animal(An) :- animal(An).

:- check pred animals(+Anim) : list(animal).

animals(Animals) :- list(Animals, animal).

:- export(ptt/0).
ptt :- fail.
ptt :- is_animal(a).

:- pred create_pairwise_mutex_classes/3 :: (int * list * list)
	+ (not_fails, is_det).

create_pairwise_mutex_classes(0, L, L) :- !.

:- pred tcollapse(A, B) : (int(A), int(B)).
:- pred tcollapse(A, B) :: (int(A), int(B)) => (int(A), int(B)).
:- pred tcollapse(A, B) => (int(A), int(B)).
:- pred tcollapse(A, B) : (int(A), int(B)) + (not_fails, is_det).

tcollapse(_A, _B).

call_mf :-
	must_fail(aaa).

:- check comp must_fail/1 + not_fails.

must_fail(aaa) :-
	display('hello world'(aaa)),
	nl,
	fail.

:- true comp must_not_fail/0 + not_fails.

must_not_fail :-
	display('not fail\n').

:- regtype animal/1.

animal(dog).
animal(cat).
animal(tiger).
animal(lion).

:- regtype animal_type/1.

animal_type(wild).
animal_type(domestic).

% :- check pred what_kind(A, B) :: (animal(A) => animal_type(B)).
% :- check pred what_kind(+A, -B)
% 	:: (animal(A), var(B)) => (animal(A), animal_type(B)).

:- check pred what_kind(A, B) :: (animal(A), animal_type(B)) + not_fails.

what_kind(dog,   domestic).
what_kind(cat,   domestic).
what_kind(tiger, wild).
what_kind(lion,  wild).

:- check pred show_if_domestic(A) : animal(A).

show_if_domestic(A) :-
	what_kind(A, domestic),
	display(A),
	nl.

:- check pred get_domestic_list(A) : var(A) => list(A, animal).

get_domestic_list(A) :-
	findall(Animal, what_kind(Animal, domestic), A).

:- check pred display_any(A) : animal(A) + not_fails.
% :- check pred display_any(A) : animal_type(A) + not_fails.

display_any(A) :-
	display(A),
	nl.

:- check pred display_any2(A) : animal(A) + not_fails.
:- check pred display_any2(A) : animal_type(A) + not_fails.

display_any2(A) :-
	display(A),
	nl,
	fail.

:- check pred test_any(A) :: animal(A).

test_any(A) :-
	display(A),
	nl.

:- pred test2(+A, +B) : (animal(A), animal_type(B)).
:- pred test2(+A, +B) : (animal(B), animal_type(A)).
:- pred test2(+A, +B) :: (animal(A), animal(B)) => (animal(A), animal(B)).
:- pred test2(+A, +B) : (animal(A), animal(B)).
:- pred test2(-, -).
:- pred test2(-, _B).

:- pred test2(+A, +B) :: (animal(A), animal(B)) + not_fails.

% repeated tests should be instrumented only once

test2(A, B) :-
	display('A='(A)), nl,
	display('B='(B)), nl.

:- pred test3(_A, B) => gnd(B).
:- pred test3(_A, B) => gnd(B).

test3(a, b).


% throw rtcheck error on backtracking
:- pred test_det(?, +) + non_det.

test_det(_, a).
test_det(_, b).
test_det(_, c).

% throw rtcheck error
:- pred test_det2(+) + non_det.

test_det2(a).
test_det2(b).
test_det2(c).

:- check calls test4(A, B)
	: ((var(A), var(B)) ; var(A) ; (nonvar(A), nonvar(B))).

test4(A, B) :-
	display('A='(A)), nl,
	display('B='(B)), nl.

:- check calls test5(A, B) : (var(A), var(B)).
:- check calls test5(A, B) : var(A).
:- check calls test5(A, B) : (nonvar(A), nonvar(B)).

test5(A, B) :-
	display('A='(A)), nl,
	display('B='(B)), nl.


:- pred p(+A) + no_choicepoints.
:- export(p/1).

p(a).
p(a).
p(b).
p(c).

:- pred q(+A) + is_det.
:- export(q/1).

q(a).
q(a).
q(b).
q(c).
