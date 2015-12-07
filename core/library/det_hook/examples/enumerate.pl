:- module(_, _, [det_hook]).



enumerate(X):-
        display(enumerating), nl,
	OnCut = (display('goal cut'), nl),
	OnFail = (display('goal failed'), nl),
	det_try(enum(X), OnCut, OnFail).

enum(1).
enum(2).
enum(3).
