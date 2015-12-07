:- use_package(fd).
:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(format)).


pythagoras(L) :-
	statistics(runtime,_),
	do_pythagoras(L),
	statistics(runtime,[_, Time]),
	format("Used ~d milliseconds~n", Time).


do_pythagoras(L) :-
	L = [A,B,C],
	L in 1..1000,
 	AA .=. A*A,
 	BB .=. B*B,
 	CC .=. C*C,
 	AA + BB .=. CC,
 	A .=<. B,
	B .=<. C,
	2*BB .>=. CC,				% redundant
%	BB + BB .>=. CC,			% redundant
	labeling(L).
