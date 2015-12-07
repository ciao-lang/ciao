 %%  FORMA DE USO!
 %%  use_module('EXAMPLES/gen_list').

:- use_package(and_sim_plan).
:- use_module(library(lists), [append/3]).
:- op(950, xfy, [&]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Versions without granularity control
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

qsort_par_ndet([], []).
qsort_par_ndet([X|L], R) :-
	partition(L, X, L1, L2),
	qsort_par_ndet(L2, R2) '&' qsort_par_ndet(L1, R1), 
        append(R1, [X|R2], R).

partition([], _B, [], []).
partition([E|R], C, [E|Left1], Right) :- 
	E < C, !,
	partition(R, C, Left1, Right).
partition([E|R], C, Left, [E|Right1]) :-
 %% 	E >= C,
	partition(R, C, Left, Right1).

