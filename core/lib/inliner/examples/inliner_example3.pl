:- module(_, _, [hiord, inliner, expander]).

:- inline concat/3.
:- unfold concat(yes, yes, no).
:- export(concat/3).
concat([],    X, X).
concat([X|Y], Z, [X|T]) :-
	concat(Y, Z, T).

:- inline prbtext/1.

prbtext(T) :-
	concat("hh", "mno", T).

showprbtext :-
	display(A),
	prbtext(A),
	display(A).

% :- inline showprbtext1/0.
showprbtext1 :-
	A= "hola",
	concat(A, "mundo", B),
	display(B).

% :- unfold othertest1.
% othertest1 :- showprbtext1.

showprbtext2 :-
	display(A),
	concat("hh", "mno", A),
	display(A),
	display(A).

showprbtext3 :-
	concat("hh", "mno", A),
	display(A),
	display(A).

:- inline testd/2.
:- unfold testd(yes, no).

testd((A ; B), C) :-
	!,
	(
	    testd(A, C)
	;
	    testd(B, C)
	).
testd(A, A).

test2(C, D) :-
	(
	    C = true ->
	    display(true)
	;
	    display(fail),
	    D = fail
	).

test(C) :-
	(
	    var(C) ->
	    display(good)
	;
	    display(bad)
	),
	nl,
	testd((one ; two), C).

:- inline ppp/2.
:- unfold ppp(yes, no).

ppp(a, 1).
ppp(a, 2).
ppp(b, 3).
ppp(b, 4).

testa(X) :-
	ppp(a, X).

testb(X) :-
	ppp(b, X).
