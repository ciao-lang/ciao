:- module(sets,
	[ insert/3, ord_delete/3,
	  ord_member/2, ord_test_member/3, ord_subtract/3,
	  ord_intersection/3, ord_intersection_diff/4, ord_intersect/2,
	  ord_subset/2, ord_subset_diff/3,
	  ord_union/3, ord_union_diff/4, ord_union_symdiff/4,
	  ord_union_change/3, merge/3,
	  ord_disjoint/2, setproduct/3
	],
	[ assertions,basicmodes
	]).

:- use_module(library(sort)).

:- doc(title, "Set Operations").

:- doc(author, "Lena Flood").

:- doc(module, "This module implements set operations. Sets are
   just ordered lists.").

:- pred insert(+Set1, +Element, -Set2) # "It is true when @var{Set2}
   is @var{Set1} with @var{Element} inserted in it, preserving the
   order.".

% Author : Lena Flood %
% Updated: 9 September 1988 %

insert([], Element, [Element]).
insert([Head|Tail], Element, Set) :-
	compare(Order, Head, Element),
	insert_comp(Order, Head, Tail, Element, Set).

insert_comp(<, Head, Tail, Element, [Head|Set]) :-
	insert(Tail, Element, Set).
insert_comp(=, Head, Tail, _, [Head|Tail]).
insert_comp(>, Head, Tail, Element, [Element,Head|Tail]).

:- pred ord_delete(+Set0,+X,-Set)
	# "It succeeds if @var{Set} is @var{Set0} without element @var{X}.".

ord_delete([Y|Ys],X,Zs) :-
	X @> Y, !,
	Zs = [Y|NewYs],
	ord_delete(Ys,X,NewYs).
ord_delete([Y|Ys],X,Zs) :-
	X == Y, !,
	Zs = Ys.
ord_delete([Y|Ys],_X,Zs) :-
	/* X @< Y */
	Zs = [Y|Ys].
ord_delete([],_,[]).

:- pred ord_member(+X,+Set)
	# "It succeeds if @var{X} is member of @var{Set}.".

ord_member(X,[Y|Ys]) :-
	compare(D,X,Y),
	ord_member1(D,X,Ys).

ord_member1(=,_,_).
ord_member1(>,X,[Y|Ys]):-
	compare(D,X,Y),
	ord_member1(D,X,Ys).

:- pred ord_test_member(+Set,+X,-Result)
	# "If @var{X} is member of @var{Set} then @var{Result}=@tt{yes}.
           Otherwise @var{Result}=@tt{no}.".

ord_test_member([],_Y,no).
ord_test_member([X|Xs],Y,Flag) :-
	compare(D,X,Y),
	ord_test_member_(D,Xs,Y,Flag).

ord_test_member_(=,_,_,yes).
ord_test_member_(<,Xs,Y,Flag):-
	ord_test_member(Xs,Y,Flag).
ord_test_member_(>,_Xs,_Y,no).

:- pred ord_subtract(+Set1, +Set2, ?Difference)
	# "It is true when @var{Difference} contains all and only the elements
	   of @var{Set1} which are not also in @var{Set2}.".
%   Author : Lena Flood							      %
%   Updated: 9 September 1988						      %

ord_subtract([], _, []) :- !.
ord_subtract(Set1, [], Set1) :- !.
ord_subtract([Head1|Tail1], [Head2|Tail2], Difference) :-
	compare(Order, Head1, Head2),
	ord_subtract_(Order, Head1, Tail1, Head2, Tail2, Difference).

ord_subtract_(<, Head1, [], _, _, [Head1]) :- !.
ord_subtract_(<, Head0, [Head1|Tail1], Head2, Tail2, [Head0|Difference]) :-
	compare(Order, Head1, Head2),
	ord_subtract_(Order, Head1, Tail1, Head2, Tail2, Difference).
ord_subtract_(=, _, Tail1, _, Tail2, Difference) :-
	ord_subtract(Tail1, Tail2, Difference).
ord_subtract_(>, Head1, Tail1, _, [], [Head1|Tail1]) :- !.
ord_subtract_(>, Head1, Tail1, _, [Head2|Tail2], Difference) :-
	compare(Order, Head1, Head2),
	ord_subtract_(Order, Head1, Tail1, Head2, Tail2, Difference).

:- pred ord_intersect(+Xs,+Ys)
	# "Succeeds when the two ordered lists have at least one element in 
	   common.".

ord_intersect([Head1|Tail1], [Head2|Tail2]) :-
	compare(Order, Head1, Head2),
	ord_intersect_(Order, Head1, Tail1, Head2, Tail2).

ord_intersect_(<, _, [Head1|Tail1], Head2, Tail2) :-
	compare(Order, Head1, Head2),
	ord_intersect_(Order, Head1, Tail1, Head2, Tail2).
ord_intersect_(=, _, _, _, _).
ord_intersect_(>, Head1, Tail1, _, [Head2|Tail2]) :-
	compare(Order, Head1, Head2),
	ord_intersect_(Order, Head1, Tail1, Head2, Tail2).

:- pred ord_intersection(+Set1, +Set2, ?Intersection)
	# "It is true when @var{Intersection} is the ordered
	representation of @var{Set1} and @var{Set2}, provided that
	@var{Set1} and @var{Set2} are ordered lists.".

%   Author : Lena Flood							      %
%   Updated: 9 September 1988						      %

ord_intersection([], _, []) :- !.
ord_intersection(_, [], []) :- !.
ord_intersection([Head1|Tail1], [Head2|Tail2], Intersection) :-
	compare(Order, Head1, Head2),
	ord_intersection_(Order, Head1, Tail1, Head2, Tail2, Intersection).

ord_intersection_(<, _, [], _, _, []) :- !.
ord_intersection_(<, _, [Head1|Tail1], Head2, Tail2, Intersection) :-
	compare(Order, Head1, Head2),
	ord_intersection_(Order, Head1, Tail1, Head2, Tail2, Intersection).
ord_intersection_(=, Head, Tail1, _, Tail2, [Head|Intersection]) :-
	ord_intersection(Tail1, Tail2, Intersection).
ord_intersection_(>, _, _, _, [], []) :- !.
ord_intersection_(>, Head1, Tail1, _, [Head2|Tail2], Intersection) :-
	compare(Order, Head1, Head2),
	ord_intersection_(Order, Head1, Tail1, Head2, Tail2, Intersection).

:- pred ord_intersection_diff(+Set1,+Set2,-Intersect,-NotIntersect)
	# "@var{Intersect} contains those elements which are both in
	@var{Set1} and @var{Set2}, and @var{NotIntersect} those which
	are in @var{Set1} but not in @var{Set2}.".

ord_intersection_diff([], _, [], []) :- !.
ord_intersection_diff(Xs, [], [], Xs) :- !.
ord_intersection_diff([O|Os], [N|Ns], Int, NotInt) :-
	compare(C, O, N), 
	ord_intersection_diff_(C, O, Os, N, Ns, Int, NotInt).
	
ord_intersection_diff_(<, O, [], _, _, [], [O]) :- !.
ord_intersection_diff_(<, O1, [O|Os], N, Ns, Int, [O1|NotInt]) :-
	compare(C, O, N), 
	ord_intersection_diff_(C, O, Os, N, Ns, Int, NotInt).
ord_intersection_diff_(=, _, Os, N, Ns, [N|Int], NotInt) :-
	ord_intersection_diff(Os, Ns, Int, NotInt).
ord_intersection_diff_(>, O, Os, _, [], [], [O|Os]) :- !.
ord_intersection_diff_(>, O, Os, _, [N|Ns], Int, NotInt) :-
	compare(C, O, N), 
	ord_intersection_diff_(C, O, Os, N, Ns, Int, NotInt).

:- pred ord_subset(+Xs,+Ys)
	# "Succeeds when every element of @var{Xs} appears in @var{Ys}.".

ord_subset([], _).
ord_subset([Head1|Tail1], [Head2|Tail2]) :-
	compare(Order, Head1, Head2),
	ord_subset_(Order, Head1, Tail1, Tail2).

ord_subset_(=, _, Tail1, Tail2) :-
	ord_subset(Tail1, Tail2).
ord_subset_(>, Head1, Tail1, [Head2|Tail2]) :-
	compare(Order, Head1, Head2),
	ord_subset_(Order, Head1, Tail1, Tail2).

:- pred ord_subset_diff(+Set1,+Set2,-Difference)
	# "It succeeds when every element of @var{Set1} appears in
	@var{Set2} and @var{Difference} has the elements of @var{Set2}
	which are not in @var{Set1}.".

ord_subset_diff([], Ys, Ys).
ord_subset_diff([Head1|Tail1], [Head2|Tail2], Diff) :-
	compare(Order, Head1, Head2),
	ord_subset_diff_(Order, Head1, Tail1, Head2,Tail2, Diff).

ord_subset_diff_(=, _, Tail1, _, Tail2, Diff) :-
	ord_subset_diff(Tail1, Tail2, Diff).
ord_subset_diff_(>, Head1, Tail1, Head2, [Head3|Tail2], [Head2|Diff]) :-
	compare(Order, Head1, Head3),
	ord_subset_diff_(Order, Head1, Tail1, Head3, Tail2, Diff).

:- pred ord_union(+Set1, +Set2, ?Union) 
	# "It is true when @var{Union} is the union of @var{Set1} and
	@var{Set2}.  When some element occurs in both sets,
	@var{Union} retains only one copy.".

%   Author : Lena Flood							      %
%   Updated: 9 September 1988						      %

ord_union([], Set2, Set2) :- !.
ord_union(Set1, [], Set1) :- !.
ord_union([Head1|Tail1], [Head2|Tail2], Union) :-
	compare(Order, Head1, Head2),
	ord_union_(Order, Head1, Tail1, Head2, Tail2, Union).

ord_union_(<, Head0, [], Head2, Tail2, [Head0,Head2|Tail2]) :- !.
ord_union_(<, Head0, [Head1|Tail1], Head2, Tail2, [Head0|Union]) :-
	compare(Order, Head1, Head2),
	ord_union_(Order, Head1, Tail1, Head2, Tail2, Union).
ord_union_(=, Head,  Tail1, _,	  Tail2, [Head|Union]) :-
	ord_union(Tail1, Tail2, Union).
ord_union_(>, Head1, Tail1, Head0, [], [Head0,Head1|Tail1]) :- !.
ord_union_(>, Head1, Tail1, Head0, [Head2|Tail2], [Head0|Union]) :-
	compare(Order, Head1, Head2),
	ord_union_(Order, Head1, Tail1, Head2, Tail2, Union).

:- pred merge(+Set1, +Set2, ?Union)
	# "See @tt{ord_union/3}.".

merge(Set1,Set2,Union):- ord_union(Set1,Set2,Union).

:- pred ord_union_change(+Set1, +Set2, -Union)
	# "@var{Union} is the union of @var{Set1} and @var{Set2} and
	@var{Union} is different from @var{Set2}.".

ord_union_change(Set1, [], Set1) :- !,
	nonempty(Set1).
ord_union_change([Head1|Tail1], [Head2|Tail2], Union) :-
	compare(Order, Head1, Head2),
	ord_union_change_(Order, Head1, Tail1, Head2, Tail2, Union).

ord_union_change_(<, Head0, [], Head2, Tail2, [Head0,Head2|Tail2]) :- !.
ord_union_change_(<, Head0, [Head1|Tail1], Head2, Tail2, [Head0|Union]) :-
	compare(Order, Head1, Head2),
	ord_union_(Order, Head1, Tail1, Head2, Tail2, Union).
ord_union_change_(=, Head,  Tail1, _, Tail2, [Head|Union]) :-
	ord_union_change(Tail1, Tail2, Union).
ord_union_change_(>, Head1, Tail1, Head0, [], [Head0,Head1|Tail1]) :- !.
ord_union_change_(>, Head1, Tail1, Head0, [Head2|Tail2], [Head0|Union]) :-
	compare(Order, Head1, Head2),
	ord_union_change_(Order, Head1, Tail1, Head2, Tail2, Union).

nonempty([_|_]).

:- pred ord_union_diff(+Set1,+Set2,-Union,-Difference)
	# "It succeeds when @var{Union} is the union of @var{Set1} and
        @var{Set2}, and @var{Difference} is @var{Set2} set-minus
        @var{Set1}.".

ord_union_diff([], Set, Set, Set) :- !.
ord_union_diff(Set, [], Set, []) :- !.
ord_union_diff([O|Os], [N|Ns], Set, New) :-
	compare(C, O, N), 
	ord_union_diff_(C, O, Os, N, Ns, Set, New).
	
ord_union_diff_(<, O, [], N, Ns, [O,N|Ns], [N|Ns]) :- !.
ord_union_diff_(<, O1, [O|Os], N, Ns, [O1|Set], New) :-
	compare(C, O, N), 
	ord_union_diff_(C, O, Os, N, Ns, Set, New).
ord_union_diff_(=, _, Os, N, Ns, [N|Set], New) :-
	ord_union_diff(Os, Ns, Set, New).
ord_union_diff_(>, O, Os, N, [], [N,O|Os], [N]) :- !.
ord_union_diff_(>, O, Os, N1, [N|Ns], [N1|Set], [N1|New]) :-
	compare(C, O, N), 
	ord_union_diff_(C, O, Os, N, Ns, Set, New).

:- pred ord_union_symdiff(+Set1, +Set2, -Union, -Diff)
	# "It is true when Diff is the symmetric difference of Set1
           and Set2, and Union is the union of Set1 and Set2.".

ord_union_symdiff(Set1,Set2,Union,Diff):-
	ord_symdiff_union(Set1,Set2,Diff,Union).

ord_symdiff_union([],Set2,Set2,Set2) :- !.
ord_symdiff_union(Set1,[],Set1,Set1) :- !.
ord_symdiff_union([H1|Tail1],[H2|Tail2],Diff,Union) :-
	compare(Order,H1,H2),
	ord_symdiff_union_(Order,H1,Tail1,H2,Tail2,Diff,Union).

ord_symdiff_union_(<,H0,[],H2,Tail2,Diff,Union) :- !,
	Diff = [H0,H2|Tail2],
	Union = Diff.
ord_symdiff_union_(<,H0,[H1|Tail1],H2,Tail2,[H0|Diff],[H0|Union]) :-
	compare(Order,H1,H2),
	ord_symdiff_union_(Order,H1,Tail1,H2,Tail2,Diff,Union).
ord_symdiff_union_(=,H1,Tail1,_,Tail2,Diff,[H1|Union]) :-
	ord_symdiff_union(Tail1,Tail2,Diff,Union).
ord_symdiff_union_(>,H1,Tail1,H0,[],Diff,Union) :- !,
	Diff = [H0,H1|Tail1],
	Union = Diff.
ord_symdiff_union_(>,H1,Tail1,H0,[H2|Tail2],[H0|Diff],[H0|Union]) :-
	compare(Order,H1,H2),
	ord_symdiff_union_(Order,H1,Tail1,H2,Tail2,Diff,Union).

:- pred ord_disjoint(+Set1, +Set2)
	# "@var{Set1} and @var{Set2} have no element in common.".

ord_disjoint([], _) :- !.
ord_disjoint(_, []) :- !.
ord_disjoint([Head1|Tail1], [Head2|Tail2]) :-
	compare(Order, Head1, Head2),
	ord_disjoint_(Order, Head1, Tail1, Head2, Tail2).

ord_disjoint_(<, _, [], _, _) :- !.
ord_disjoint_(<, _, [Head1|Tail1], Head2, Tail2) :-
	compare(Order, Head1, Head2),
	ord_disjoint_(Order, Head1, Tail1, Head2, Tail2).
ord_disjoint_(>, _, _, _, []) :- !.
ord_disjoint_(>, Head1, Tail1, _, [Head2|Tail2]) :-
	compare(Order, Head1, Head2),
	ord_disjoint_(Order, Head1, Tail1, Head2, Tail2).

:- pred setproduct(+Set1,+Set2,-Product)
% setproduct(Xs,Ys,Xss)                                                  |
	# "@var{Product} has all two element sets such that one element
	   is in @var{Set1} and the other in @var{set2}, except that if
	   the same element belongs to both, then the corresponding one
	   element set is in @var{Product}.".

setproduct([], _, []).
setproduct([Head|Tail], Set, SetProduct)  :-
	setproduct_(Set, Head, SetProduct, Rest),
	setproduct(Tail, Set, Rest).

setproduct_([], _, Set, Set).
setproduct_([Head|Tail], X, [Set|TailX], Tl) :-
	sort([Head,X],Set),
	setproduct_(Tail, X, TailX, Tl).
