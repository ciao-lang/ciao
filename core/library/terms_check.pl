:- module(terms_check,
	[ ask/2, instance/2, subsumes_term/2, variant/2,
	  most_general_instance/3,
	  most_specific_generalization/3,
	  unifier/1, unifiable/3
	],
	[assertions, regtypes, nortchecks]).

:- doc(title,"Term checking utilities").

:- doc(author,"The CLIP Group").

:- doc(module,"This module implements a basic set of term checking
   utilities.").

:- doc(appendix,"Currently, @pred{ask/2} and @pred{instance/2} are
   exactly the same. However, @pred{ask/2} is intended to be more
   general, since it is also applicable to constraint domains
   (although not yet implemented): i.e., for the particular case of
   Herbrand terms, @pred{ask/2} is just @pred{instance/2}.").

%-------------------------------------------------------------------------

:- doc(variant(Term1,Term2),"@var{Term1} and @var{Term2} are
   identical up to renaming.").

/* Not safe! E.g.: variant(p('$VAR'(0),X),p(Y,'$VAR'(0)))
variant(Term1,Term2) :-
	\+ \+
        (  numbervars(Term1,0,N),
	   numbervars(Term2,0,N),
	   Term1 == Term2
        ).
*/

variant(Term1,Term2) :-
	samevarpositions(Term1,Term2,VarDic,[]),
	\+ \+ numbervarpairs(VarDic,0).

samevarpositions(X,Y,Dic,Dic0):- var(X), !, var(Y), Dic=[X=Y|Dic0].
samevarpositions(X,Y,Dic,Dic0):- atomic(X), !, X==Y, Dic=Dic0.
samevarpositions(X,Y,Dic,Dic0):-
	nonvar(Y),
        functor(X,F,A),
        functor(Y,F,A),
	samevarpositions_(0,A,X,Y,Dic,Dic0).

samevarpositions_(A,A,_,_,Dic,Dic):- !.
samevarpositions_(I,A,X,Y,Dic,Dic1):-
	I1 is I+1,
	arg(I1,X,X1),
	arg(I1,Y,Y1),
	samevarpositions(X1,Y1,Dic,Dic0),
        samevarpositions_(I1,A,X,Y,Dic0,Dic1).

numbervarpairs([X=Y|VarDic],N):-
	varpair(X,Y,N,N1),
	numbervarpairs(VarDic,N1).
numbervarpairs([],_).

varpair(X,Y,N,N1):- var(X), !, X=N, X=Y, N1 is N+1.
varpair(X,Y,N,N1):- var(Y), !, Y=N, X=Y, N1 is N+1.
varpair(X,X,N,N).

%-------------------------------------------------------------------------
:- doc(ask(Term1,Term2),"@var{Term1} and @var{Term2} unify without
	producing bindings for the variables of @var{Term1}. I.e.,
	@tt{instance(@var{Term1},@var{Term2})} holds.").

:- true prop instance(Term1,Term2) + native
	# "@var{Term1} is an instance of @var{Term2}.".

:- pred subsumes_term(Term1,Term2) + iso
	# "@var{Term2} is an instance of @var{Term1}.".

ask(A, B) :- '$instance'(A, B).

instance(A, B) :- '$instance'(A, B).

subsumes_term(A, B) :- '$instance'(B, A).


/*
% This is the version implemented in Prolog (10 times slower)
% In the future, compilation to C should be able to generate
% much better code for these predicates.
% -- jfran
%
% update: This code buggy!!
%
%   instance(f(X,0), f(X,X)) success (WRONG)

ask(Goal1,Goal2) :-
        \+ \+ (mynumbervars(Goal1,Goal2,0,_N)), !.

instance(Goal1,Goal2):-
	\+ \+ (mynumbervars(Goal1,Goal2,0,_N)), !.

mynumbervars(X,Y,N,N1) :- var(X), !, var(Y), N1 is N+1, X=N,Y=N.
mynumbervars(X,Y,N,N) :- var(Y), !, X = Y.
mynumbervars(A,B,N,N) :- atomic(A),!, A=B.
mynumbervars(F1,F2,N,N1) :-
        functor(F1,F,A),
        functor(F2,F,A),
        mynumbervars6(0,A,F1,F2,N,N1).

mynumbervars6(A,A,_,_,N,N):- !.
mynumbervars6(I,A,F1,F2,N,N1) :-
         I1 is I+1,
         arg(I1,F1,X),
         arg(I1,F2,Y),
         mynumbervars(X,Y,N,N0),
         mynumbervars6(I1,A,F1,F2,N0,N1).
*/

:- impl_defined('$instance'/2).

%-------------------------------------------------------------------------
:- doc(most_specific_generalization(Term1,Term2,Term),"@var{Term}
	satisfies @tt{instance(@var{Term1},@var{Term})} and
	@tt{instance(@var{Term2},@var{Term})} and there is no term less
	general than @var{Term} (modulo variants) that satisfies it.").

most_specific_generalization(T1,T2,T3) :-
	msg1(T1,T2,T3,_).

msg1(T1,T2,T3,S) :-
	msgDiffer(T1,T2),
	msgOccur(subst(T1,T2,T3),S),
	!.
msg1(T1,T2,T3,S) :-
	nonvar(T1),
	nonvar(T2),
	!,
	T1 =.. [F|Args1],
	T2 =.. [F|Args2],
	msgArgs(Args1,Args2,Args3,S),
	T3 =.. [F|Args3].
msg1(T1,_,T1,_).

msgArgs([X|Xs],[Y|Ys],[Z|Zs],S) :-
	msg1(X,Y,Z,S),
	msgArgs(Xs,Ys,Zs,S).
msgArgs([],[],[],_).

msgDiffer(X,Y) :-
	var(X),
	!,
	X \== Y.
msgDiffer(X,Y) :-
	var(Y),
	!,
	X \== Y.
msgDiffer(X,Y) :-
	functor(X,F,N),
	functor(Y,G,M),
	msgDiffTerm(F,G,N,M).

msgDiffTerm(F,G,_,_) :-
	F \== G,
	!.
msgDiffTerm(_,_,N,M) :-
	N =\= M.

msgOccur(subst(X,Y,Z),[U|_]) :-
	var(U),
	U = subst(X,Y,Z),
	!.
msgOccur(subst(X,Y,Z),[subst(U,V,Z)|_]) :-
	X == U,
	Y == V,
	!.
msgOccur(subst(X,Y,Z),[_|S]) :-
	msgOccur(subst(X,Y,Z),S).

/*
% Old version (buggy)

most_specific_generalization(T1,_T2,T):-
	var(T1), !,
	var(T).
most_specific_generalization(_T1,T2,T):-
	var(T2), !,
	var(T).
most_specific_generalization(T1,T2,T):-
	functor(T1,F,A),
	functor(T2,F,A), !,
	functor(T,F,A),
	msg_each_arg(A,T1,T2,T).
most_specific_generalization(_T1,_T2,T):-
	var(T).

msg_each_arg(0,_T1,_T2,_T):- !.
msg_each_arg(N,T1,T2,T):-
	arg(N,T1,A1),
	arg(N,T2,A2),
	arg(N,T,A),
	N1 is N-1,
	most_specific_generalization(A1,A2,A),
	msg_each_arg(N1,T1,T2,T).
*/

:- doc(most_general_instance(Term1,Term2,Term),"@var{Term}
	satisfies @tt{instance(@var{Term},@var{Term1})} and
	@tt{instance(@var{Term},@var{Term2})} and there is no term more
	general than @var{Term} (modulo variants) that satisfies it.").

most_general_instance(T1,T2,T):-
	copy_term(T1,T),
	copy_term(T2,T).

/*
most_general_instance(T1,T2,T):-
	var(T1), !,
	copy_term(T2,T).
most_general_instance(T1,T2,T):-
	var(T2), !,
	copy_term(T1,T).
most_general_instance(T1,T2,T):-
	functor(T1,F,A),
	functor(T2,F,A), !,
	functor(T,F,A),
	mgi_each_arg(A,T1,T2,T).

mgi_each_arg(0,_T1,_T2,_T):- !.
mgi_each_arg(N,T1,T2,T):-
	arg(N,T1,A1),
	arg(N,T2,A2),
	arg(N,T,A),
	N1 is N-1,
	most_general_instance(A1,A2,A),
	mgi_each_arg(N1,T1,T2,T).
*/


:- regtype unifier_elem/1.
:- doc(doinclude, unifier_elem/1).

unifier_elem(X=Term) :- var(X), term(Term).

:- regtype unifier(X) # "@var{X} is a unifier.".
:- doc(unifier/1, "@includedef{unifier/1}").

unifier(Unifier) :- list(Unifier, unifier_elem).

:- true pred unifiable(X, Y, Unifier) :
	(term(X), term(Y)) => unifier(Unifier)

	# "Unifies @var{Unifier} with the most general unifier between
            the terms @var{X} and @var{Y}. Fails if such unifier does
            not exit.".

:- impl_defined(unifiable/3).
