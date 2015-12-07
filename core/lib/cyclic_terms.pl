:- module(cyclic_terms,
        [acyclic_term/1, uncycle_term/2, recycle_term/2],
        [assertions]).

:- use_module(library(lists), [nocontainsx/2, list_lookup/3]).
:- use_module(engine(term_basic), [cyclic_term/1]).

:- doc(title, "Cyclic terms handling").
:- doc(author, "Daniel Cabeza").
:- doc(author, "Remy Haemmerle").

:- doc(module, "This module implements predicates related to cyclic
   terms.  Cyclic (or infinite) terms are produced when unifying a
   variable with a term which contains that variable.").

:- pred cyclic_term(T) # "True if @var{T} is cyclic (infinite).".

:- reexport(engine(term_basic), [cyclic_term/1]).

:- pred acyclic_term(T) + iso # "True if @var{T} is acyclic (finite).".

acyclic_term(T) :-
	\+ term_basic:cyclic_term(T).

:- pred uncycle_term(T, U) # "Given a term @var{T}, @var{U} is a
   finite representation of @var{T} as an acyclic term.  This
   representation can be converted back to @var{T} using
   @pred{recycle_term/2}.".

uncycle_term(T, (U,Pairs)) :-
        uncycle_term_(T, [], Pairs, U).

uncycle_term_(T,_Seen,_Pairs, U) :-
        var(T), !,
        U = T.
uncycle_term_(T,_Seen,_Pairs, U) :-
        atomic(T), !,
        U = T.
uncycle_term_(T, Seen, Pairs, U) :-
        already_seen(Seen, T, TU), !,
        list_lookup(Pairs, TU, U).
uncycle_term_(T, Seen, Pairs, U) :-
        functor(T, F, A),
        functor(U, F, A),
        uncycle_term_args(A, T, [(T,U)|Seen], Pairs, U).

uncycle_term_args(0, _, _, _, _) :- !.
uncycle_term_args(A, T, Seen, Pairs, U) :-
        A1 is A-1,
        arg(A, T, Ta),
        arg(A, U, Ua),
        uncycle_term_(Ta, Seen, Pairs, Ua),
        uncycle_term_args(A1, T, Seen, Pairs, U).

already_seen([(T,U)|_], Term, U) :-
        T == Term, !.
already_seen([_|Ts], Term, U) :-
        already_seen(Ts, Term, U).

:- pred recycle_term(U, T) # "Given @var{U}, a finite
   representation of a term as an acyclic term as @pred{uncycle_term/2}
   produces, @var{T} is the represented term.  @var{U} is modified by
   the predicate, thus to maintain it untouched @pred{copy_term/2}
   should be used.".

recycle_term((U,Pairs), U) :-
        unify_pairs(Pairs).

unify_pairs(L) :- var(L), !.
unify_pairs([U-U|Pairs]) :-
        unify_pairs(Pairs).
