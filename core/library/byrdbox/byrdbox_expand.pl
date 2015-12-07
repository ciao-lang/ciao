
:- module(byrdbox_expand,[ expand_byrdbox/3 ],[ ]).

%% Assumed this expansion treats one module at a time!

expand_byrdbox((:-D0),(:-D),M):- !,
	expand_directive(D0,M,D).
expand_byrdbox((H:-B),Cls,M):- !,
	first_clause(H,B,M,Cls).
expand_byrdbox(end_of_file,end_of_file,M):- !,
	retractall_fact(clause_seen(_,M)).
expand_byrdbox(H,Cls,M):-
	first_clause(H,true,M,Cls).

expand_directive(spy(F/A),M,initialization(byrd:spy(M:F/A))):- !.
expand_directive(D,_M,D).

:- data clause_seen/2.

first_clause(H,B,M,Cls):-
	clause_seen(H,M), !,
	Cls=(H:-B).
first_clause(H,B,M,[(P:-byrd:flipflop,!,byrd:trace(P)),(H:-B)]):-
	functor(H,F,A),
	functor(P,F,A),
	asserta_fact(clause_seen(P,M)).
