:- module(_, [main/0], [condcomp]).

% Try to comment or uncomment the following line:
:- compilation_fact(use_write).

:- if(defined(use_write)).
:- use_module(engine(io_basic), [nl/0]).
:- use_module(library(write)).
pr(X) :-
	write(using_write(X)), nl.
:- else.
:- use_module(engine(io_basic)).
pr(X) :-
	display(using_display(X)), nl.
:- endif.

main :-
	pr('hello world').
