:- use_package(fd).
:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(format)).

dgr(DGR) :-
	statistics(runtime,_),
	do_dgr(DGR),
	statistics(runtime,[_, Time]),
	format("Used ~d milliseconds~n", Time).

do_dgr(DGR) :-	
	DGR = [D,G,R,O,E,N,B,A,L,T],
	DGR in 0..9,
	D .>. 0,
	G .>. 0,
	all_different(DGR),
	100000*D + 10000*O + 1000*N + 100*A + 10*L + D +
       100000*G + 10000*E + 1000*R + 100*A + 10*L + D .=.
       100000*R + 10000*O + 1000*B + 100*E + 10*R + T,
       labeling(DGR).
