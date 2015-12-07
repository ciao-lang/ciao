:- module(_, _, [profiler, assertions]).

:- all_cost_center.

:- doc(author, "Edison Mera").

:- doc(module, "This example test that the time is associated
	correctly to the predicates.  Note that the cost of prepare
	the call is assigned to the caller, but not to the predicate
	being called.  For example, p1/1, p2/1 and p3/1 spent more or
	less the same time, independently of the complexity of their
	input arguments.").

p1(_).

p2(_).

p3(_).

q1 :-
	p1([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40]),
	p2([]),
	p3([]).

q2 :-
	p1([]),
	p2([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40]),
	p3([]).

q3 :-
	p1([]),
	p2([]),
	p3([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40]).

:- use_module(library(profiler/profiler_utils)).

test1 :-
	profile_reset,
	profile(q1),
	profile_dump.

test2 :-
	profile_reset,
	profile(q2),
	profile_dump.

test3 :-
	profile_reset,
	profile(q3),
	profile_dump.
