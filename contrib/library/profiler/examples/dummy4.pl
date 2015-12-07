:- module(dummy4, _, [profiler]).

aa(X) :-
	X is 2.55.

bb(X) :-
	display(X).

cc :- display(op1), nl.
cc :- display(op2), nl.

main1 :-
	(
	    aa(X),
	    (true ; bb(X)),
	    cc
	) ; true.

main2 :-
	(
	    (aa(X) ; bb(X)),
	    cc
	) ; true.
