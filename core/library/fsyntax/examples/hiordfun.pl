:- module(hiordfun, _, [fsyntax,hiordx,assertions,regtypes,isomodes]).

%! \title Tests for hiord and fsyntax
%  \author Jose F. Morales
%
%  \module Testing hiord and functions

:- use_module(engine(io_basic)).

:- fun_eval arith(true).
:- fun_eval hiord(true).

test :-
    T = 1 + 3,
    display(begin(T)), nl,
    P = { Inc -> ''(A, R) :- R = Inc+A },
    Inc = 1000,
    P2 = { ''(A) := R :- R = 100+A },
    P3 = { ''(A) := 10 + A },
    display(p(P)), nl,
    display(p2(P2)), nl,
    display(p3(P3)), nl,
    X = ~P(~P2(~P3(1))),
    display(X), nl. % it should say 1111
