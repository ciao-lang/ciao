:- module(_, [go/1]).

:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(write)).

:- use_package(fd).

go(Vars):-
    statistics(runtime,_),
    top(Vars),
    statistics(runtime,[_,T]),
    write('execution time is '),write(T), write(' milliseconds'),nl.

top(Vars):-
    vars_constraints(Vars),
    labeling(Vars).

vars_constraints(Vars):-
    Vars = [X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16],
    Vars in 1..16,
    all_different(Vars),

    X1+X2+X3+X4 .=. 34,
    X5+X6+X7+X8 .=. 34,
    X9+X10+X11+X12 .=. 34,
    X13+X14+X15+X16 .=. 34,

    X1+X5+X9+X13 .=. 34,
    X2+X6+X10+X14 .=. 34,
    X3+X7+X11+X15 .=. 34,
    X4+X8+X12+X16 .=. 34,

    X1+X6+X11+X16 .=. 34,
    X4+X7+X10+X13 .=. 34.
