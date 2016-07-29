:- module(block_example, [merge/3], [block]).

:- block merge(-,?,-), merge(?,-,-). 
      
merge([], Y, Y). 
merge(X, [], X).  
merge([H|X], [E|Y], [H|Z]) :- H @< E,  merge(X, [E|Y], Z).
merge([H|X], [E|Y], [E|Z]) :- H @>= E, merge([H|X], Y, Z).
