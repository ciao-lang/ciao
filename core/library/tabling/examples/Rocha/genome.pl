
:- ['aux_time2.pl'].

:- table anc_l/2.
:- table genome_l/1.
:- use_retroactive_tabling anc_l/2.
:- use_retroactive_tabling genome_l/1.

anc_l(X, Y) :- edge(X, Y).
anc_l(X, Y) :- anc_l(X, Z), edge(Z, Y).

genome_l(X) :- anc_l(1, X), anc_l(2, X).

time_query :- genome_l(X), fail.
