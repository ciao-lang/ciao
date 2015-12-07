:- module(_, _, [inliner, expander]).

:- unfold p(yes, no).

:- unfold p(yes, no).

p(a, b).
p(a, c).
p(b, d).
p(b, e).

:- renamer q/1.

q(A) :-
	p(a, A).

r(B) :-
	p(b, B).

:- unfold output_data_example(yes, no).

:- inline output_data_examples/2.
:- unfold output_data_examples(yes, no).

output_data_example(_, _).

output_data_examples(Examples, FileName) :-
	list(Examples, output_data_example(FileName)).

:- export(output_data_example_1/1).

output_data_example_1(FileName) :- output_data_example(1, FileName).

t(A,B) :-
	output_data_examples(A, B),
	q(A).
