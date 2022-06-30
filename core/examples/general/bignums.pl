:- module(bignums, [test_bignums/0], []).

% Computes a 13^16000, which requires bignums, with a naive algorithm
% and a divide-and-conquer.

:- use_module(library(streams)).
:- use_module(library(write)).

test_bignums :-
    N = 13,
    Exp = 16000,
    write('Naively calculating '), write(N),
    write('^'), write(Exp), flush_output,
    exponential_naive(N, Exp,R),
    write('  Done'), nl,
    write('Divide-and-conquer calculating '), write(N),
    write('^'), write(Exp), flush_output,
    exponential_div(N, Exp, Res),
    write('  Done'), nl,
    (
        Res = R ->
        write('Both results are equal'),
        nl
    ;
        write('*********************** Results differ ******************')
    ).

%% exponential(Base, Exp, Res): Be smart and split Exp in halves

exponential_div(_Base, 0, 1).
exponential_div(Base, Exp, Res):-
    Exp > 0,
    HalfExp is Exp // 2,
    exponential_div(Base, HalfExp, HalfRes),
    (
        Exp mod 2 =:= 0 ->
        Res is HalfRes*HalfRes
    ;
        Res is HalfRes*HalfRes*Base
    ).

exponential_naive(_Base, 0, 1).
exponential_naive(Base, Exp, Res):-
    Exp > 0,
    NewExp is Exp - 1,
    exponential_naive(Base, NewExp, PartRes),
    Res is PartRes * Base.
