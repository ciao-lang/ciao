% OLD EXAMPLE - Won't work!

:- module(_, _, [det_hook]).

:- use_module(engine(internals)).

pr(I, X, Y) :-
	display(open(I)), nl,
	'$metachoice'(C),
	det_try(pr2(X, Y, C),
	(display(close_done(I)), nl),
	(display(close_abort(I)), nl)).

pr2(X, _, _) :- X =< 0, !, fail.  
pr2(2, 2, C) :- '$metacut'(C).
pr2(X, Y, C) :- (X = Y ; X1 is X - 1, pr2(X1, Y, C)).
	
test1 :-
	pr(x, 4, X),
	pr(y, 4, Y),
	display(X),
	display(Y),
	nl, X = 1,
	Y = 3, !.
