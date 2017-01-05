:- ['aux_time2.pl'].

:- table p/3.
:- use_retroactive_tabling p/3.

:- file_name(F), consult(F).

window(A) :- size(X), A is X // 10.

compute_retro(Goal) :- call(Goal), fail.
compute_retro(_).

compute_all(A, A, _) :- !.
compute_all(A, B, X) :-
	compute_retro(p(A, t(X), _)),
	A1 is A + 1,
	compute_all(A1, B, X).

compute_all2(A, A) :- !.
compute_all2(A, B) :-
	A1 is A + 1,
	window(S),
	compute_all(0, S, A),
	compute_all2(A1, B).

query :- compute_all2(0, 10),
	p(_, t(_), _),
	p(A, B, C).

debug_query :- query,
	time_subsumed_collect(Time),
	write(Time), nl.

time_query :- query.

:- time, (benchmark_execution ; true), halt.

