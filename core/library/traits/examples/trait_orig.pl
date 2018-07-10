:- module(trait_orig, [], [traits, assertions]).

% :- trait gadget { ... }.
:- discontiguous 'gadget.p1'/1.
:- multifile 'gadget.p1'/1.
:- discontiguous 'gadget.p2'/1.
:- multifile 'gadget.p2'/1.
:- discontiguous 'gadget.q'/2.
:- multifile 'gadget.q'/2.
:- discontiguous 'gadget.r'/3.
:- multifile 'gadget.r'/3.

% :- impl gadget for datum0.
'gadget.p1'(datum0) :- '<datum0 as gadget>.p1'.
'gadget.p2'(datum0) :- '<datum0 as gadget>.p2'.
'gadget.q'(datum0, X) :- '<datum0 as gadget>.q'(X).
'gadget.r'(datum0, X, Y) :- '<datum0 as gadget>.r'(X, Y).

'<datum0 as gadget>.p1' :- true.

'<datum0 as gadget>.p2' :- fail.

'<datum0 as gadget>.q'(X) :- X = r1.

'<datum0 as gadget>.r'(X, Y) :- X = r1, Y = r2.

% :- impl gadget for datum1/1.
'gadget.p1'(M) :- M = datum1(D), '<datum1/1 as gadget>.p1'(D).
'gadget.p2'(M) :- M = datum1(D), '<datum1/1 as gadget>.p2'(D).
'gadget.q'(M, X) :- M = datum1(D), '<datum1/1 as gadget>.q'(X, D).
'gadget.r'(M, X, Y) :- M = datum1(D), '<datum1/1 as gadget>.r'(X, D, Y).

'<datum1/1 as gadget>.p1'(_E1) :- true.

'<datum1/1 as gadget>.p2'(_E1) :- fail.

'<datum1/1 as gadget>.q'(X, E1) :- X = r1(E1).

'<datum1/1 as gadget>.r'(X, E1, Y) :- X = r1(E1), Y = r2.

% :- impl gadget for datum2/2.
'gadget.p1'(M) :- M = datum2(_,_), '<datum2/2 as gadget>.p1'(M).
'gadget.p2'(M) :- M = datum2(_,_), '<datum2/2 as gadget>.p2'(M).
'gadget.q'(M, X) :- M = datum2(_,_), '<datum2/2 as gadget>.q'(X, M).
'gadget.r'(M, X, Y) :- M = datum2(_,_), '<datum2/2 as gadget>.r'(X, M, Y).

'<datum2/2 as gadget>.p1'(datum2(_E1, _E2)) :- true.

'<datum2/2 as gadget>.p2'(datum2(_E1, _E2)) :- fail.

'<datum2/2 as gadget>.q'(X, datum2(E1, _E2)) :- X = r1(E1).

'<datum2/2 as gadget>.r'(X, datum2(E1, E2), Y) :- X = r1(E1), Y = r2(E2).

% (tests)

:- use_module(library(aggregates), [findall/3]).

:- test trait_test(X) =>
     X = [[true,false,r1,    [r1,    r2]],
          [true,false,r1(e1),[r1(e1),r2]],
          [true,false,r1(e1),[r1(e1),r2(e2)]]].

:- export(trait_test/1).
trait_test([Y1,Y2,Y3]) :-
	row(datum0,Y1),
	row(datum1(e1),Y2),
	row(datum2(e1,e2),Y3).

row(A,Xs) :- findall(X,col(A,X),Xs).

col(A,X) :- ( 'gadget.p1'(A) -> X = true ; X = false ).
col(A,X) :- ( 'gadget.p2'(A) -> X = true ; X = false ).
col(A,X) :- 'gadget.q'(A, X).
col(A,[X,Y]) :- 'gadget.r'(A, X, Y).
