/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2008, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

%% Migration to Ciao by Edison Mera 2009-12-26
%% Backport of recent version by Jose Morales 2011-8-16

:- module(apply,
	  [ include/3,			% :Pred, +List, -Ok
	    exclude/3,			% :Pred. +List, -NotOk
	    partition/4,		% :Pred, +List, -Included, -Excluded
	    partition/5,		% :Pred, +List, ?Less, ?Equal, ?Greater
	    maplist/2,			% :Pred, +List
	    maplist/3,			% :Pred, ?List, ?List
	    maplist/4,			% :Pred, ?List, ?List, ?List
	    maplist/5			% :Pred, ?List, ?List, ?List, ?List
	  ], [assertions, basicmodes, nativeprops, hiord]).
% :- use_module(library(error)).

/** <module> Apply predicates on a list

This module defines meta-predicates  that  apply   a  predicate  on  all
members of a list.

@see	apply_macros.pl provides compile-time expansion for part of this
	library.
@see	http://www.cs.otago.ac.nz/staffpriv/ok/pllib.htm
@tbd	Move maplist/N from boot/apply.pl to here.
@tbd	Add include/4, include/5, exclude/4, exclude/5
*/

:- doc(module, "
<module> Apply predicates on a list

This module defines meta-predicates that apply a predicate on all
members of a list.

Original code from SWI-Prolog. Migrated to Ciao by Edison Mera.
Backport of recent changes by Jose Morales.
").

:- pred include(+Goal, +List1, ?List2)
	:: (callable(Goal), list(List1), list(List2)) + is_det #
"
	Filter elements for which Goal succeed.   True if List2 contains
	those elements Xi of List1 for which Goal(Xi) succeeds.
".

% 	@see	Older versions of SWI-Prolog had sublist/3 with the same
% 		arguments and semantics.

:- meta_predicate include(pred(1), +, ?).
include(Goal, List, Included) :-
	include_(List, Goal, Included).

include_([], _, []).
include_([X1|Xs1], P, Included) :-
	(   call(P, X1)
	->  Included = [X1|Included1]
	;   Included = Included1
	),
	include_(Xs1, P, Included1).


:- pred exclude(+Goal, +List1, ?List2) :: ( callable(Goal),
	    list(List1), list(List2) ) + is_det # "
	Filter elements for which Goal fails.  True if List2 contains
	those elements Xi of List1 for which Goal(Xi) fails.
".

:- meta_predicate exclude(pred(1), +, ?).
exclude(Goal, List, Included) :-
	exclude_(List, Goal, Included).

:- meta_predicate exclude_(+, pred(1), ?).
exclude_([], _, []).
exclude_([X1|Xs1], P, Included) :-
	(   call(P, X1)
	->  Included = Included1
	;   Included = [X1|Included1]
	),
	exclude_(Xs1, P, Included1).


:- pred partition(+Pred, +List, ?Included, ?Excluded) :: ( callable(Pred),
	    list(List), list(Included), list(Excluded) ) + is_det # "
	Filter elements of List according  to   Pred.  True  if Included
	contains all elements  for  which   Pred(X)  succeeds and
	Excluded contains the remaining elements.
".

:- meta_predicate partition(pred(1), +, ?, ?).
partition(Pred, List, Included, Excluded) :-
	partition_(List, Pred, Included, Excluded).

:- meta_predicate partition_(+, pred(1), ?, ?).
partition_([], _, [], []).
partition_([H|T], Pred, Incl, Excl) :-
	(   call(Pred, H)
	->  Incl = [H|I],
	    partition_(T, Pred, I, Excl)
	;   Excl = [H|E],
	    partition_(T, Pred, Incl, E)
	).


:- pred partition(+Pred, +List, ?Less, ?Equal, ?Greater) ::
	(callable(Pred), list(List), list(Less), list(Equal), list(Greater)) %is semidet.
# "
	Filter list according to Pred in three sets. For each element Xi
	of List, its destination is determined by Pred(Xi, Place),
	where Place must be unified to  one   of  =|<|=, =|=|= or =|>|=.
	Pred must be deterministic.
".

:- meta_predicate partition(pred(2), +, ?, ?, ?).
partition(Pred, List, Less, Equal, Greater) :-
	partition_(List, Pred, Less, Equal, Greater).

:- push_prolog_flag(multi_arity_warnings, off).

:- meta_predicate partition_(+, pred(2), ?, ?, ?).
partition_([], _, [], [], []).
partition_([H|T], Pred, L, E, G) :-
	call(Pred, H, Diff),
	partition_(Diff, H, Pred, T, L, E, G).

partition_(<, H, Pred, T, [H|L], E, G) :- !,
	partition_(T, Pred, L, E, G).
partition_(=, H, Pred, T, L, [H|E], G) :- !,
	partition_(T, Pred, L, E, G).
partition_(>, H, Pred, T, L, E, [H|G]) :- !,
	partition_(T, Pred, L, E, G).
% partition_(Diff, _, _, _, _, _, _) :-
% 	must_be(oneof([<.=,>]), Diff).

		 /*******************************
		 *	    MAPLIST/2...	*
		 *******************************/

:- pred maplist(+Goal, ?List) ::
	(callable(Goal), list(List))
# "
	True if Goal can succesfully be applied on all elements of List.
	Arguments are reordered to gain performance as well as to make
	the predicate deterministic under normal circumstances.
".

:- meta_predicate maplist(pred(1), ?).
maplist(Goal, List) :-
	maplist_(List, Goal).

:- meta_predicate maplist_(?, pred(1)).
maplist_([], _).
maplist_([Elem|Tail], Goal) :-
	call(Goal, Elem),
	maplist_(Tail, Goal).

:- pred maplist(+Goal, ?List1, ?List2) ::
	(callable(Goal), list(List1), list(List2))
# "
	True if Goal can succesfully be applied to all succesive pairs
	of elements of List1 and List2.
".

:- meta_predicate maplist(pred(2), ?, ?).
maplist(Goal, List1, List2) :-
	maplist_(List1, List2, Goal).

:- meta_predicate maplist_(?, ?, pred(2)).
maplist_([], [], _).
maplist_([Elem1|Tail1], [Elem2|Tail2], Goal) :-
	call(Goal, Elem1, Elem2),
	maplist_(Tail1, Tail2, Goal).

:- pred maplist(+Goal, ?List1, ?List2, ?List3) ::
	(callable(Goal), list(List1), list(List2), list(List3))
# "
	True if Goal can succesfully be applied to all succesive triples
	of elements of List1..List3.
".

:- meta_predicate maplist(pred(3), ?, ?, ?).
maplist(Goal, List1, List2, List3) :-
	maplist_(List1, List2, List3, Goal).

:- meta_predicate maplist_(?, ?, ?, pred(3)).
maplist_([], [], [], _).
maplist_([Elem1|Tail1], [Elem2|Tail2], [Elem3|Tail3], Goal) :-
	call(Goal, Elem1, Elem2, Elem3),
	maplist_(Tail1, Tail2, Tail3, Goal).

:- pred maplist(+Goal, ?List1, ?List2, ?List3, ?List4) ::
	(callable(Goal), list(List1), list(List2), list(List3), list(List4))
# "
	True if Goal  can  succesfully  be   applied  to  all  succesive
	quadruples of elements of List1..List4
".

:- meta_predicate maplist(pred(4), ?, ?, ?, ?).
maplist(Goal, List1, List2, List3, List4) :-
	maplist_(List1, List2, List3, List4, Goal).

:- meta_predicate maplist_(?, ?, ?, ?, pred(4)).
maplist_([], [], [], [], _).
maplist_([Elem1|Tail1], [Elem2|Tail2], [Elem3|Tail3], [Elem4|Tail4], Goal) :-
	call(Goal, Elem1, Elem2, Elem3, Elem4),
	maplist_(Tail1, Tail2, Tail3, Tail4, Goal).

:- pop_prolog_flag(multi_arity_warnings).

