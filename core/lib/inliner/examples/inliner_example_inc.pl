:- module(_, _, [hiord]).

dummypred(_).


:- meta_predicate mymap(?, pred(2), ?).

mymap([],    _,   []).
mymap([E|L], Map, [M|R]) :-
	Map(E, M),
	mymap(L, Map, R).
