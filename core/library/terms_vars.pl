:- module(terms_vars, [varset/2, intersect_vars/3, member_var/2, diff_vars/3,
		varsbag/3, varset0/2, varset_in_args/2, term_variables/2,
		term_variables/3],
	    [assertions]).

:- use_module(library(idlists), [memberchk/2, union_idlists/3]).
:- use_module(library(sort)).

:- doc(title, "Sets of variables in terms").

:- doc(author, "The CLIP Group").

:- doc(module, "This module implements predicates to handle sets of
   variables in terms.").

%-------------------------------------------------------------------------

:- pred term_variables(Term, Vars) + iso #
   "@var{Vars} is the list of all the variables in @var{Term}, ordered
   as they appear in @var{Term} right-to-left depth-first (without
   duplicates).".

% TODO: check that second argument is partial list or list.

term_variables(Term, Vars) :- 
	term_variables(Term, Vars, []).

:- doc(term_variables(Term, Vars, Tail), "@var{Vars}-@var{Tail} is
   the difference list of all the variables in @var{Term}, ordered as
   they appear in @var{Term} right-to-left depth-first (without
   duplicates).").

term_variables(Term, Vars, Tail) :- 
	term_variables_2(Term, Vars0, Vars0, Tail), 
	Vars = Vars0.
 
term_variables_2(X, Vars, Tail0, Tail) :-
	var(X), !,
	push_var(Vars, X, Tail0, Tail).
term_variables_2(Term, Vars, Tail0, Tail) :-
	term_variables_3(1, Term, Vars, Tail0, Tail).

term_variables_3(N0, Term, Vars, Tail0, Tail) :-
	arg(N0, Term, Arg),
	!,
	term_variables_2(Arg, Vars, Tail0, Tail1),
	N is N0 + 1,
	term_variables_3(N, Term, Vars, Tail1, Tail).
term_variables_3(_, _, _, Tail, Tail).

push_var(Vars, X, _, Tail) :-
	var(Vars),
	!,
	Vars = [X|Tail].
push_var([E|Vars], X, Tail0, Tail) :-
	E==X -> Tail0 = Tail ; push_var(Vars, X, Tail0, Tail).

%-------------------------------------------------------------------------

:- doc(varset(Term, Xs), "@var{Xs} is the sorted list of all the
   variables in @var{Term}.").

varset(X, Xs) :-
	varsbag(X, Xs_uns, []),
	sort(Xs_uns, Xs).

:- doc(varsbag(Term, Vs, Xs), "@var{Vs} is the list of all the
   variables in @var{Term} ordered as they appear in @var{Term}
   right-to-left depth-first (including duplicates) plus @var{Xs}.").

varsbag(X, Vars, Tail) :-
	var(X), !,
	Vars = [X|Tail].
varsbag([H|T], Vars, Tail) :- !,
	varsbag(H, Vars,  Tail0),
	varsbag(T, Tail0, Tail).
varsbag(Term, Vars, Tail) :-
	functor(Term, _, A),
	go_inside(A, Term, Vars, Tail).

go_inside(0, _, Tail, Tail) :- !.
go_inside(N, T, Bag,  Tail) :-
	Nth is N-1,
	arg(N, T, ARG),
	varsbag(ARG, Bag, Tail0),
	go_inside(Nth, T, Tail0, Tail).

%-------------------------------------------------------------------------
:- doc(hide, varset0/2).

varset0(X,    [X]) :- var(X), !.
varset0(Term, Vars) :-
	functor(Term, _, N),
	extract_vartype(N, Term, Vars), !.

extract_vartype(0, _,    []) :- !.
extract_vartype(N, Term, Bag) :-
	Nth is N-1,
	extract_vartype(Nth, Term, Ts),
	arg(N, Term, T),
	( var(T) ->
	    ( memberchk(T, Ts) ->
		Bag = Ts
	    ; Bag = [T|Ts]
	    )
	; atomic(T) ->
	    Bag = Ts
	; varset0(T, TVs),
	    union_idlists(TVs, Ts, Bag)
	).

%-------------------------------------------------------------------------

:- pred varset_in_args(T, LL) : nonvar(T) => list(LL, list(var)) # "Each
   list of @var{LL} contains the variables of an argument of @var{T},
   for each argument, and in left to right order.".

varset_in_args(Term, Xss) :-
	Term =.. [_|Args],
	vars_in_args(Args, Xss).

vars_in_args([],         []).
vars_in_args([Arg|Rest], [Arg_list|Rest_list]) :-
	varset(Arg, Arg_list),
	vars_in_args(Rest, Rest_list).

intersect_vars([],     _,  []).
intersect_vars([X|S1], S2, S) :-
	( member_var(S2, X) ->
	    S = [X|SList] ;
	    S = SList ),
	intersect_vars(S1, S2, SList).

diff_vars([],     _L, []).
diff_vars([H|L1], L2, L3) :-
	member_var(L2, H),
	!,
	diff_vars(L1, L2, L3).
diff_vars([H|L1], L2, [H|L3]) :-
	diff_vars(L1, L2, L3).

% member_var([],       _) :- fail.
member_var([E|List], Ele) :-
	E == Ele -> true ; member_var(List, Ele).
