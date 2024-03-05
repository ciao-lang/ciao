:- module(_, [go/1], [clpfd]).

:- use_module(engine(runtime_control), [statistics/2]).
:- use_module(engine(io_basic), [nl/0]).
:- use_module(library(write)).

go(Vars):-
    statistics(runtime,_),
    top(Vars),
    statistics(runtime,[_,T]),
    write('execution time is '),write(T), write(' milliseconds'),nl.

top(Vars):-
    vars_constraints(Vars),
    labeling([ff], Vars).

vars_constraints(Vars):-
    Vars=[X1,X2,X3,X4,X5,X6,X7,X8,X9],
    domain(Vars, 1, 9),
    all_different(Vars),
    X1+X2+X3 #= 15,
    X4+X5+X6 #= 15,
    X7+X8+X9 #= 15,
    X1+X4+X7 #= 15,
    X2+X5+X8 #= 15,
    X3+X6+X9 #= 15,
    X1+X5+X9 #= 15,
    X3+X5+X7 #= 15.
