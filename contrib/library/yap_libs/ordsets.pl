% This file has been included as an YAP library by Vitor Santos Costa, 1999

%   File   : ORDSET.PL
%   Author : R.A.O'Keefe
%   Updated: 22 May 1983
%   Purpose: Ordered set manipulation utilities

%   In this module, sets are represented by ordered lists with no
%   duplicates.  Thus {c,r,a,f,t} would be [a,c,f,r,t].  The ordering
%   is defined by the @< family of term comparison predicates, which
%   is the ordering used by sort/2 and setof/3.

%   The benefit of the ordered representation is that the elementary
%   set operations can be done in time proportional to the Sum of the
%   argument sizes rather than their Product.  Some of the unordered
%   set routines, such as member/2, length/2, select/3 can be used
%   unchanged.  The main difficulty with the ordered representation is
%   remembering to use it!

:- module(ordsets, [
	list_to_ord_set/2,	%  List -> Set
	merge/3,		%  OrdList x OrdList -> OrdList
	ord_add_element/3,	%  Set x Elem -> Set
	ord_del_element/3,	%  Set x Elem -> Set
	ord_disjoint/2,		%  Set x Set ->
	ord_insert/3,		%  Set x Elem -> Set
        ord_member/2,           %  Set -> Elem
	ord_intersect/2,	%  Set x Set ->
	ord_intersect/3,	%  Set x Set -> Set
	ord_intersection/3,	%  Set x Set -> Set
	ord_intersection/4,	%  Set x Set -> Set x Set
	ord_seteq/2,		%  Set x Set ->
	ord_setproduct/3,	%  Set x Set -> Set
	ord_subset/2,		%  Set x Set ->
	ord_subtract/3,		%  Set x Set -> Set
	ord_symdiff/3,		%  Set x Set -> Set
        ord_union/2,            %  Set^2 -> Set
	ord_union/3,		%  Set x Set -> Set
	ord_union/4,		%  Set x Set -> Set x Set,
	ord_empty/1,             % -> Set
	ord_memberchk/2             % Element X Set
      ]).
:- include(library(dialect/yap)).

/*
:- mode
	list_to_ord_set(+, ?),
	merge(+, +, -),
	ord_disjoint(+, +),
	ord_disjoint(+, +, +, +, +),
	ord_insert(+, +, ?),
	ord_insert(+, +, +, +, ?),
	ord_intersect(+, +),
	ord_intersect(+, +, +, +, +),
	ord_intersect(+, +, ?),
	ord_intersect(+, +, +, +, +, ?),
	ord_seteq(+, +),
	ord_subset(+, +),
	ord_subset(+, +, +, +, +),
	ord_subtract(+, +, ?), 
	ord_subtract(+, +, +, +, +, ?),
	ord_symdiff(+, +, ?),
	ord_symdiff(+, +, +, +, +, ?),
	ord_union(+, +, ?),
	ord_union(+, +, +, +, +, ?).
*/


%   list_to_ord_set(+List, ?Set)
%   is true when Set is the ordered representation of the set represented
%   by the unordered representation List.  The only reason for giving it
%   a name at all is that you may not have realised that sort/2 could be
%   used this way.

list_to_ord_set(List, Set) :-
	sort(List, Set).


%   merge(+List1, +List2, -Merged)
%   is true when Merged is the stable merge of the two given lists.
%   If the two lists are not ordered, the merge doesn't mean a great
%   deal.  Merging is perfectly well defined when the inputs contain
%   duplicates, and all copies of an element are preserved in the
%   output, e.g. merge("122357", "34568", "12233455678").  Study this
%   routine carefully, as it is the basis for all the rest.

merge([Head1|Tail1], [Head2|Tail2], [Head2|Merged]) :-
	Head1 @> Head2, !,
	merge([Head1|Tail1], Tail2, Merged).
merge([Head1|Tail1], List2, [Head1|Merged]) :-
	List2 \== [], !,
	merge(Tail1, List2, Merged).
merge([], List2, List2) :- !.
merge(List1, [], List1).



%   ord_disjoint(+Set1, +Set2)
%   is true when the two ordered sets have no element in common.  If the
%   arguments are not ordered, I have no idea what happens.

ord_disjoint([], _) :- !.
ord_disjoint(_, []) :- !.
ord_disjoint([Head1|Tail1], [Head2|Tail2]) :-
	compare(Order, Head1, Head2),
	ord_disjoint(Order, Head1, Tail1, Head2, Tail2).

ord_disjoint(<, _, Tail1, Head2, Tail2) :-
	ord_disjoint(Tail1, [Head2|Tail2]).
ord_disjoint(>, Head1, Tail1, _, Tail2) :-
	ord_disjoint([Head1|Tail1], Tail2).



%   ord_insert(+Set1, +Element, ?Set2)
%   ord_add_element(+Set1, +Element, ?Set2)
%   is the equivalent of add_element for ordered sets.  It should give
%   exactly the same result as merge(Set1, [Element], Set2), but a bit
%   faster, and certainly more clearly.

ord_add_element([], Element, [Element]).
ord_add_element([Head|Tail], Element, Set) :-
	compare(Order, Head, Element),
	ord_insert(Order, Head, Tail, Element, Set).


ord_insert([], Element, [Element]).
ord_insert([Head|Tail], Element, Set) :-
	compare(Order, Head, Element),
	ord_insert(Order, Head, Tail, Element, Set).


ord_insert(<, Head, Tail, Element, [Head|Set]) :-
	ord_insert(Tail, Element, Set).
ord_insert(=, Head, Tail, _, [Head|Tail]).
ord_insert(>, Head, Tail, Element, [Element,Head|Tail]).



%   ord_intersect(+Set1, +Set2)
%   is true when the two ordered sets have at least one element in common.
%   Note that the test is == rather than = .

ord_intersect([Head1|Tail1], [Head2|Tail2]) :-
	compare(Order, Head1, Head2),
	ord_intersect(Order, Head1, Tail1, Head2, Tail2).

ord_intersect(=, _, _, _, _).
ord_intersect(<, _, Tail1, Head2, Tail2) :-
	ord_intersect(Tail1, [Head2|Tail2]).
ord_intersect(>, Head1, Tail1, _, Tail2) :-
	ord_intersect([Head1|Tail1], Tail2).

ord_intersect(L1, L2, L) :-
	ord_intersection(L1, L2, L).


%   ord_intersection(+Set1, +Set2, ?Intersection)
%   is true when Intersection is the ordered representation of Set1
%   and Set2, provided that Set1 and Set2 are ordered sets.

ord_intersection([], _, []) :- !.
ord_intersection([_|_], [], []) :- !.
ord_intersection([Head1|Tail1], [Head2|Tail2], Intersection) :-
	( Head1 == Head2 ->
	    Intersection = [Head1|Tail],
	    ord_intersection(Tail1, Tail2, Tail)
	;
	    Head1 @< Head2 ->
	    ord_intersection(Tail1, [Head2|Tail2], Intersection)
	;
	    ord_intersection([Head1|Tail1], Tail2, Intersection)
	).

%   ord_intersection(+Set1, +Set2, ?Intersection, ?Difference)
%   is true when Intersection is the ordered representation of Set1
%   and Set2, provided that Set1 and Set2 are ordered sets.

ord_intersection([], L, [], L) :- !.
ord_intersection([_|_], [], [], []) :- !.
ord_intersection([Head1|Tail1], [Head2|Tail2], Intersection, Difference) :-
	( Head1 == Head2 ->
	    Intersection = [Head1|Tail],
	    ord_intersection(Tail1, Tail2, Tail, Difference)
	;
	    Head1 @< Head2 ->
	    ord_intersection(Tail1, [Head2|Tail2], Intersection, Difference)
	;
	    Difference = [Head2|HDifference],
	    ord_intersection([Head1|Tail1], Tail2, Intersection, HDifference)
	).


%   ord_seteq(+Set1, +Set2)
%   is true when the two arguments represent the same set.  Since they
%   are assumed to be ordered representations, they must be identical.


ord_seteq(Set1, Set2) :-
	Set1 == Set2.



%   ord_subset(+Set1, +Set2)
%   is true when every element of the ordered set Set1 appears in the
%   ordered set Set2.

ord_subset([], _) :- !.
ord_subset([Head1|Tail1], [Head2|Tail2]) :-
	compare(Order, Head1, Head2),
	ord_subset(Order, Head1, Tail1, Head2, Tail2).

ord_subset(=, _, Tail1, _, Tail2) :-
	ord_subset(Tail1, Tail2).
ord_subset(>, Head1, Tail1, _, Tail2) :-
	ord_subset([Head1|Tail1], Tail2).



%   ord_subtract(+Set1, +Set2, ?Difference)
%   is true when Difference contains all and only the elements of Set1
%   which are not also in Set2.


ord_subtract(Set1, [], Set1) :- !.
ord_subtract([], _, []) :- !.
ord_subtract([Head1|Tail1], [Head2|Tail2], Difference) :-
	compare(Order, Head1, Head2),
	ord_subtract(Order, Head1, Tail1, Head2, Tail2, Difference).

ord_subtract(=, _,     Tail1, _,     Tail2, Difference) :-
	ord_subtract(Tail1, Tail2, Difference).
ord_subtract(<, Head1, Tail1, Head2, Tail2, [Head1|Difference]) :-
	ord_subtract(Tail1, [Head2|Tail2], Difference).
ord_subtract(>, Head1, Tail1, _,     Tail2, Difference) :-
	ord_subtract([Head1|Tail1], Tail2, Difference).


%   ord_del_element(+Set1, Element, ?Rest)
%   is true when Rest contains the elements of Set1
%   except for Set1


ord_del_element([], _, []).
ord_del_element([Head1|Tail1], Head2, Rest) :-
	compare(Order, Head1, Head2),
	ord_del_element(Order, Head1, Tail1, Head2, Rest).

ord_del_element(=, _,     Tail1, _, Tail1).
ord_del_element(<, Head1, Tail1, Head2, [Head1|Difference]) :-
	ord_del_element(Tail1, Head2, Difference).
ord_del_element(>, Head1, Tail1, _, [Head1|Tail1]).



%   ord_symdiff(+Set1, +Set2, ?Difference)
%   is true when Difference is the symmetric difference of Set1 and Set2.

ord_symdiff(Set1, [], Set1) :- !.
ord_symdiff([], Set2, Set2) :- !.
ord_symdiff([Head1|Tail1], [Head2|Tail2], Difference) :-
	compare(Order, Head1, Head2),
	ord_symdiff(Order, Head1, Tail1, Head2, Tail2, Difference).

ord_symdiff(=, _,     Tail1, _,     Tail2, Difference) :-
	ord_symdiff(Tail1, Tail2, Difference).
ord_symdiff(<, Head1, Tail1, Head2, Tail2, [Head1|Difference]) :-
	ord_symdiff(Tail1, [Head2|Tail2], Difference).
ord_symdiff(>, Head1, Tail1, Head2, Tail2, [Head2|Difference]) :-
	ord_symdiff([Head1|Tail1], Tail2, Difference).



%   ord_union(+Set1, +Set2, ?Union)
%   is true when Union is the union of Set1 and Set2.  Note that when
%   something occurs in both sets, we want to retain only one copy.

ord_union(Set1, [], Set1) :- !.
ord_union([], Set2, Set2) :- !.
ord_union([Head1|Tail1], [Head2|Tail2], Union) :-
	compare(Order, Head1, Head2),
	ord_union(Order, Head1, Tail1, Head2, Tail2, Union).

ord_union(=, Head,  Tail1, _,     Tail2, [Head|Union]) :-
	ord_union(Tail1, Tail2, Union).
ord_union(<, Head1, Tail1, Head2, Tail2, [Head1|Union]) :-
	ord_union(Tail1, [Head2|Tail2], Union).
ord_union(>, Head1, Tail1, Head2, Tail2, [Head2|Union]) :-
	ord_union([Head1|Tail1], Tail2, Union).


%   ord_union(+Set1, +Set2, ?Union, ?Difference)
%   is true when Union is the union of Set1 and Set2 and Difference is the
%   difference between Set2 and Set1.

ord_union(Set1, [], Set1, []) :- !.
ord_union([], Set2, Set2, Set2) :- !.
ord_union([Head1|Tail1], [Head2|Tail2], Union, Diff) :-
	compare(Order, Head1, Head2),
	ord_union(Order, Head1, Tail1, Head2, Tail2, Union, Diff).

ord_union(=, Head,  Tail1, _,  Tail2, [Head|Union], Diff) :-
	ord_union(Tail1, Tail2, Union, Diff).
ord_union(<, Head1, Tail1, Head2, Tail2, [Head1|Union], Diff) :-
	ord_union(Tail1, [Head2|Tail2], Union, Diff).
ord_union(>, Head1, Tail1, Head2, Tail2, [Head2|Union], [Head2|Diff]) :-
	ord_union([Head1|Tail1], Tail2, Union, Diff).



%   ord_setproduct(+Set1, +Set2, ?Product)
%   is in fact identical to setproduct(Set1, Set2, Product).
%   If Set1 and Set2 are ordered sets, Product will be an ordered
%   set of x1-x2 pairs.  Note that we cannot solve for Set1 and
%   Set2, because there are infinitely many solutions when
%   Product is empty, and may be a large number in other cases.

ord_setproduct([], _, []).
ord_setproduct([H|T], L, Product) :-
	ord_setproduct(L, H, Product, Rest),
	ord_setproduct(T, L, Rest).

ord_setproduct([], _, L, L).
ord_setproduct([H|T], X, [X-H|TX], TL) :-
	ord_setproduct(T, X, TX, TL).


ord_member(El,[H|T]):-
    compare(Op,El,H),
    ord_member(Op,El,T).

ord_member(=,_,_).
ord_member(>,El,[H|T]) :-
    compare(Op,El,H),
    ord_member(Op,El,T).

ord_union([], []).
ord_union([Set|Sets], Union) :-
    length([Set|Sets], NumberOfSets),
    ord_union_all(NumberOfSets, [Set|Sets], Union, []).

ord_union_all(N,Sets0,Union,Sets) :-
    (  N=:=1  -> Sets0=[Union|Sets]
    ;  N=:=2  -> Sets0=[Set1,Set2|Sets], 
                 ord_union(Set1,Set2,Union)
    ;  A is N>>1,
       Z is N-A,
       ord_union_all(A, Sets0, X, Sets1),
       ord_union_all(Z, Sets1, Y, Sets),
       ord_union(X, Y, Union)
    ).

ord_empty([]).

ord_memberchk(Element, [E|_]) :- E == Element, !.
ord_memberchk(Element, [_|Set]) :-
	ord_memberchk(Element, Set).

