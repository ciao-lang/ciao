:- module(_, [mm/1], [clpfd]).

:- use_module(engine(runtime_control), [statistics/2]).
:- use_module(engine(io_basic)).
:- use_module(library(format)).

mm([S,E,N,D,M,O,R,Y]) :-
    statistics(runtime,_),
    domain([S,E,N,D,M,O,R,Y], 0, 9),
    0 #< S, 0 #< M,
    all_different([S,E,N,D,M,O,R,Y]),
    S*1000 + E*100 + N*10 + D +
    M*1000 + O*100 + R*10 + E #=
    M*10000 + O*1000 + N*100 + E*10 + Y,
    labeling([], [S,E,N,D,M,O,R,Y]),
    statistics(runtime,[_, Time]),
    format("Used ~d milliseconds~n", [Time]).

