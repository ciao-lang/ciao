:- module(idlists,
	[ member_0/2, memberchk/2,
	  list_insert/2, add_after/4, add_before/4, delete/3,
	  subtract/3, union_idlists/3
        ],
	[ assertions, isomodes
	]).

:- doc(author,"Francisco Bueno").

:- doc(title,"Identity lists").

:- doc(module,"The operations in this module handle lists by performing
    equality checks via identity instead of unification.").

:- doc(memberchk(X,Xs),
	"Checks that @var{X} is an element of (list) @var{Xs}.").

memberchk(X, Ys) :- member_0(X, Ys).

:- doc(member_0(X,Xs),"True iff memberchk/2 is true.").

member_0(X, [Y|_]) :- X == Y, !.
member_0(X, [_|L]) :- member_0(X, L).

:- pred list_insert(-List,+Term)
	# "Adds @var{Term} to the end of (tail-opened) @var{List} 
           if there is not an element in @var{List} identical to @var{Term}.".

list_insert(List, Term) :-
	var(List), !,
	List=[Term|_].
list_insert([Term0|_], Term) :-
	Term0==Term, !.
list_insert([_|List], Term) :-
	list_insert(List, Term).

:- pred add_after(+L0, +E0, +E, -L)
	# "Adds element @var{E} after the first element identical to @var{E0} 
           (or at end) of list @var{L0}, returning in @var{L} the new list.".

add_after([], _, E, [E]).
add_after([E|Es], E0, E1, NEs) :-
        E == E0, !,
        NEs = [E0,E1|Es].
add_after([E|Es], E0, E1, [E|NEs]) :-
        add_after(Es, E0, E1, NEs).

:- pred add_before(+L0, +E0, +E, -L)
	# "Adds element @var{E} before the first element identical to @var{E0}
           (or at start) of list @var{L0}, returning in @var{L} the new list.".

add_before(L, E0, E, NL) :-
        add_before_existing(L, E0, E, NL), !.
add_before(L, _, E, [E|L]).

add_before_existing([E|Es], E0, E1, NEs) :-
        E == E0, !,
        NEs = [E1,E0|Es].
add_before_existing([E|Es], E0, E1, NEEs) :-
        add_before_existing(Es, E0, E1, NEs), !,
        NEEs = [E|NEs].

:- pred delete(+List,+Element,-Rest)
	# "@var{Rest} has the same elements of @var{List} except for all the 
           occurrences of elements identical to @var{Element}.".

delete([], _, []).
delete([Head|Tail], Element, Rest) :-
	Head==Element, !,
	delete(Tail, Element, Rest).
delete([Head|Tail], Element, [Head|Rest]) :-
	delete(Tail, Element, Rest).

:- pred subtract(+Set, +Set0, -Difference)
	# "@var{Difference} has the same elements of @var{Set} except those
	   which have an identical occurrence in @var{Set0}.".

subtract([], _, []).
subtract([Element|Residue], Set, Difference) :-
	memberchk(Element, Set), !, 
	subtract(Residue, Set, Difference).
subtract([Element|Residue], Set, [Element|Difference]) :-
	subtract(Residue, Set, Difference).

:- pred union_idlists(+List1,+List2,-List)
	# "@var{List} has the elements which are in @var{List1} but are not
	   identical to an element in @var{List2} followed by the elements
	   in @var{List2}.".

union_idlists([],Ys,Ys).
union_idlists([X|Xs],Ys,Zs) :- 
	( memberchk(X,Ys) ->
	  Zs = Ws
        ; Zs = [X|Ws]
        ),
	union_idlists(Xs,Ys,Ws).
