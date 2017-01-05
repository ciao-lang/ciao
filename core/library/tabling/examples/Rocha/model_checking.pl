:- ['aux_time2.pl'].
:- table reach/2.
:- use_retroactive_tabling reach/2.

reach(X,Z):- reach(X,Y), trans(Y,_,Z).
reach(X,Z):- trans(X,_,Z).
