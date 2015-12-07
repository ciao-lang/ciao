:- use_package(fd).
:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(format)).


smm(SMM) :-
	statistics(runtime,_),
	do_smm(SMM),
	statistics(runtime,[_, Time]),
	format("Used ~d milliseconds~n", Time).

do_smm(X) :-
	X = [S,E,N,D,M,O,R,Y],
	X in 0 .. 9,
	all_different(X),
	M .>. 0,
	S .>. 0,
	1000*S + 100*E + 10*N + D + 1000*M + 100*O + 10*R + E
        .=. 10000*M + 1000*O + 100*N + 10*E + Y,
	labeling(X).
