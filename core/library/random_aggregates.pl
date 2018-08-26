:- module(random_aggregates, [
    random_findall/4
   ], [assertions, isomodes, nativeprops, hiord]).

:- doc(title, "Randomized aggregates").
:- doc(author, "Jose F. Morales").

:- doc(module, "This module implements randomized version of
   @concept{aggregates} predicates.

   Example 1:
@begin{verbatim}
?- random_findall(4, X, between(1,10000,X), Xs).

Xs = [6659,7135,7871,9864] ? 

yes
@end{verbatim}

   Example 2:
@begin{verbatim}
?- random_findall(4, X, member(X, [the,sequel,will,not,happen]), Xs).

Xs = [the,sequel,will,happen] ? 

yes
@end{verbatim}
").

:- doc(bug, "weighted random sampling may not be hard to implement").

:- use_module(library(random), [random/3]).
:- use_module(library(aggregates), [findall/3]).

:- data sample/2.
:- data index/1.

:- meta_predicate random_findall(?, ?, goal, ?).
:- pred random_findall(+K, +X, +Goal, ?Ys) # "Obtains a list @var{Ys}
   of length @var{K} of random solutions uniformly distributed from
   @var{X} for all solutions to @var{Goal}. Fails if there are less
   than @var{K} solutions.".

:- doc(random_findall(K, X, Goal, Ys), "This predicate implements the
   @href{https://en.wikipedia.org/wiki/Reservoir_sampling}{reservoir
   sampling algorithm}. It needs to evaluate all solutions to
   @var{Goal} but only need to keeps @var{K} of those solutions in
   memory simultaneously.").

random_findall(K, X, Goal, Ys) :-
	% cleanup
	reset_index,
	retractall_fact(sample(_,_)),
	% process solutions
	sample_sols(K, X, Goal),
	retract_fact(index(I)),
	% make sure that we got enough samples
	I >= K,
	% get all samples
	findall(X, retract_fact(sample(_,X)), Ys).

sample_sols(K, X, Goal) :-
	( % (for all solutions)
          call(Goal),
	    inc_index(Index),
	    sample_i(Index, K, X),
	    fail
	; true
	).

% Process solution at Index
sample_i(Index, K, X) :-
	( Index < K -> set_sample(Index, X)
	; random(0, Index, R),
	  ( R < K -> % replace with decreasing probabilty
	      set_sample(R, X)
	  ; true
	  )
	).

% set index to 0
reset_index :-
	retractall_fact(index(_)),
	assertz_fact(index(0)).

% get current index and increment it
inc_index(I) :-
	retract_fact(index(I)),
	I1 is I + 1,
	assertz_fact(index(I1)).

% set sample at index I
set_sample(I, V) :-
	retractall_fact(sample(I, _)),
	assertz_fact(sample(I, V)).
