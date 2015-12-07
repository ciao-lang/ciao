:- use_package(fd).
:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(format)).


smm(N) :-
	statistics(runtime,_),
	do_smm_loop(N),
	statistics(runtime,[_, Time]),
%	RTime is Time/N,
	format("Used ~d milliseconds~n", Time).

do_smm_loop(0) .
do_smm_loop(N) :-
	do_smm,
	N1 is N - 1,
	do_smm_loop(N1).

do_smm :-
	X = [S,E,N,D,M,O,R,Y],
	X in 0 .. 9,
	all_different(X),
	M .>. 0,
	S .>. 0,
	1000*S + 100*E + 10*N + D +
        1000*M + 100*O + 10*R + E .=.
        10000*M + 1000*O + 100*N + 10*E + Y,
%	display('***************************'), nl,
	labeling(X), !.
