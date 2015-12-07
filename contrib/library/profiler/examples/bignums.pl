:- module(bignums, [do_bignums/0]).

do_bignums:-
        N = 13,
        Exp = 16000,
        exponential_naive(N, Exp,R),
        exponential_div(N, Exp, Res),
        (
             Res = R -> true
        ; 
              true
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