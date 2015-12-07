:- module(bignums, [do_bignums/0]).

:- use_module(library(write)).
:- use_module(library(prolog_sys)).

do_bignums:-
        statistics(runtime, _),
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
        ),
        statistics(runtime, [_,T]),
        write('Used '),
        write(T),
        write(' milliseconds in total'),
        nl.


        

        


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
