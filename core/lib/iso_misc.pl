:- module(iso_misc, [once/1, compound/1, sub_atom/5,
                     unify_with_occurs_check/2],
         [assertions, isomodes]).

:- use_module(library(between)).

:- doc(title, "Miscellaneous ISO Prolog predicates").

:- doc(author, "Daniel Cabeza").
:- doc(author, "Jose F. Morales").

:- doc(module, "This module implements some miscellaneous ISO
   Prolog predicates.").

:- meta_predicate(once(goal)).

:- doc(once(G),"Finds the first solution of goal @var{G} (if any).
   @pred{once/1} behaves as @pred{call/1}, except that no further
   solutions are explored on backtracking.").

:- pred once(+callable) + iso.

once(G) :- call(G), !.

:- doc(bug, "There is a naive implementation of compound/1,
	perhaps is better to implement it as a builtin -- EMM.").

:- doc(compound(T),"@var{T} is currently instantiated to a compound
        term.").

:- pred compound(?term) => struct + iso.

%:- trust success compound(A) => struct(A). % no way to be detected at CT

compound(T) :-
        nonvar(T),
        functor(T, _, A), A > 0.

:- doc(sub_atom(Atom, Before, Length, After, Sub_atom), "Is true
   iff atom @var{Atom} can be broken into three pieces, @var{AtomL},
   @var{Sub_atom} and @var{AtomR} such that @var{Before} is the number
   of characters of the name of @var{AtomL}, @var{Length} is the
   number of characters of the name of @var{Sub_atom} and @var{After}
   is the number of characters of the name of @var{AtomR}").

:- pred sub_atom(+atm, ?int, ?int, ?int, ?atm) + iso.

sub_atom(Atom, Before, Lenght, After, Sub_atom) :-
        ( atom(Atom) ->
          ( var(Sub_atom) ->
            atom_length(Atom, L),
            between(0, L, Before),
            L1 is L-Before,
            between(0, L1, Lenght),
            After is L1-Lenght,
            sub_atom(Atom, Before, Lenght, Sub_atom)
          ; atom(Sub_atom) ->
            atom_length(Atom, L),
            atom_length(Sub_atom, SL),
            Lenght = SL,
            R is L-Lenght,
            R > 0,
            between(0, R, Before),
            After is R-Before,
            sub_atom(Atom, Before, Lenght, Sub_atom)
          )
        ; var(Atom) ->
          throw(error(instantiation_error, sub_atom/5-1))
        ; throw(error(type_error(atom,Atom), sub_atom/5-1))
        ).

:- doc(unify_with_occurs_check(X, Y), "Attempts to compute and
   apply a most general unifier of the two terms @var{X} and @var{Y}.
   Is true iff @var{X} and @var{Y} are unifiable.").

:- pred unify_with_occurs_check(?term, ?term) + iso.

% NOTE: This version is faster than the Prolog version (and probably
% slower than a native implementation)
% unify_with_occurs_check(X, X) :- \+ cyclic_term(X).
:- impl_defined([unify_with_occurs_check/2]). % term_support.c

%% unify_with_occurs_check(X,Y) :- var(X), !, uwoc_var(Y, X).
%% unify_with_occurs_check(X,Y) :- atomic(X), !, X=Y.
%% unify_with_occurs_check(X,Y) :- uwoc_struct(Y, X).
%% 
%% uwoc_var(V1, V) :- var(V1), !, V1 = V.
%% uwoc_var(A, V) :- atomic(A), !, A = V.
%% uwoc_var(S, V) :- nooccurs(S, V), S = V.
%% 
%% uwoc_struct(V, S) :- var(V), !, nooccurs(S, V), S = V.
%% uwoc_struct(A,_S) :- atomic(A), !, fail.
%% uwoc_struct(S1, S2) :-
%%         functor(S1, F, A),
%%         functor(S2, F, A),
%%         uwoc_args(A, S1, S2).
%% 
%% uwoc_args(0, _, _) :- !.
%% uwoc_args(A, S1, S2) :-
%%         arg(A, S1, S1a),
%%         arg(A, S2, S2a),
%%         unify_with_occurs_check(S1a,S2a),
%%         A1 is A-1,
%%         uwoc_args(A1, S1, S2).
%% 
%% nooccurs(S, V) :-
%%         functor(S, _, A),
%%         noocurrs_args(A, S, V).
%% 
%% noocurrs_args(0, _, _) :- !.
%% noocurrs_args(A, S, V) :-
%%         arg(A, S, Sa),
%%         noocurrs_(Sa, V),
%%         A1 is A-1,
%%         noocurrs_args(A1, S, V).
%% 
%% noocurrs_(V1, V) :- var(V1), !, V1 \== V.
%% noocurrs_(A, _V) :- atomic(A), !.
%% noocurrs_(S, V) :-
%%         functor(S, _, A),
%%         noocurrs_args(A, S, V).
