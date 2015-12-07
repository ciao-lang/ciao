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
    Vars=[X1,X2,X3,X4,X5,X6,X7,X8,X9],
    Vars in 1..9,
    all_different(Vars),
    X1+X2+X3 .=. 15,
    X4+X5+X6 .=. 15,
    X7+X8+X9 .=. 15,
    X1+X4+X7 .=. 15,
    X2+X5+X8 .=. 15,
    X3+X6+X9 .=. 15,
    X1+X5+X9 .=. 15,
    X3+X5+X7 .=. 15.
