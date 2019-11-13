:- module(trait_test2, [], [traits, assertions]).
% Version of trait_test.pl using default definitions

:- trait(gadget, [
    p1,
    % (A.p2 :- fail),
    (p2 :- fail),
    q/1,
    r/2
]).

:- impl(gadget, datum0).

(datum0 as gadget).q(X) :- X = r1.

(datum0 as gadget).r(X, Y) :- X = r1, Y = r2.

:- impl(gadget, datum1/1).

(datum1(E1) as gadget).q(X) :- X = r1(E1).

(datum1(E1) as gadget).r(X, Y) :- X = r1(E1), Y = r2.

:- impl(gadget, datum2/2).

% (datum2(_,_) as gadget).p2 :- fail. % ok
% (A as gadget).p2 :- A = datum2(_,_), fail. % ok
% (A as gadget).p2 :- fail. % won't compile

(datum2(E1,_) as gadget).q(X) :- X = r1(E1).

(datum2(E1,E2) as gadget).r(X, Y) :- X = r1(E1), Y = r2(E2).

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

col(A,X) :- ( (A as gadget).p1 -> X = true ; X = false ).
col(A,X) :- ( (A as gadget).p2 -> X = true ; X = false ).
col(A,X) :- (A as gadget).q(X).
col(A,[X,Y]) :- (A as gadget).r(X, Y).
