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

launch(A, A, _) :- !.
launch(A, B, X) :-
	A1 is A + 1,
	p(A, t(X), _),
	launch(A1, B, X).

launch2(A, A) :- !.
launch2(A, B) :-
	A1 is A + 1,
	window(S),
	launch(0, S, A),
	launch2(A1, B).

query :- launch2(0, 10),
				 p(A, B, C).
debug_query :- query,
						time_subsumed_collect(T),
						write(T), nl.

time_query :- query.

:- time, (benchmark_execution ; true), halt.
