
:- use_package(fuzzy).

:- aggr myaggr.

myaggr(X,Y,Z):- Z .=. X*Y.

main:- p(T), T=0.1.

p(T) :~ myaggr q(T1), r(T2).

q(0.5):~ .

r(0.2):~ .
