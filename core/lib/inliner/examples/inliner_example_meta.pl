:- module(_, [test/0], [inliner, hiord, expander]).

:- use_module(engine(io_basic)).

:- unfold_meta.

:- inline test1/1.

:- meta_predicate test1(addmodule(addterm(goal))).
test1(A, B, C) :-
	display(A),
	nl,
	display(B),
	nl,
	display(C),
	nl,
	call(A).

test :-
	test1(display(a)).
