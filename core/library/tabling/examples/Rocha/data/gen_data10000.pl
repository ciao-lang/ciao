:- module(_, [main/0], []).

:- use_module(library(between)).
:- use_module(library(write)).

% NOTE: Execute this program to generate the data10000.pl benchmark

main :-
	( % failure driven loop
	  p(A,B,C),
	    write(p(A,B,C)),
	    display('.'),
	    nl,
	    fail
	; true
	).

p(A, t(B), p('c', C)) :-
	between(0,999,A),
	between(0,999,B),
	between(0,1,C).
