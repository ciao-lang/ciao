:- module(_, _, [det_hook]).

get(Location, X, Y) :-
	display(opening(Location)), nl,
	OnCut = (display(closing(cut, Location)), nl),
	OnFail = (display(closing(fail, Location)), nl),
	det_try(get_2(X, Y), OnCut, OnFail).

get_2(X, _) :- X =< 0, '!!', fail.  
get_2(X, X).
get_2(X, Y) :- X1 is X - 1, get_2(X1, Y).
	
search1(A, B) :-
	get(x, 4, X), get(y, 4, Y), X = A, Y = B, '!!'.

search2(A, B) :-
	get(x, 4, X), X = A, get(y, 4, Y), Y = B, '!!'.

search3(X, Y) :-
	get(x, 4, X), get(y, 4, Y), '!!'.
