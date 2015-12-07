:- module(_, _, []).

:- op(1150, fx, [dummy]).

gsl_imports_def((:- dummy A/N), C, M) :-
	functor(F, A, N),
	atom_concat(M,  ':', P1),
	atom_concat(P1, A,   P ),
	C = [(F :- throw_dummy_error(P/N))].
