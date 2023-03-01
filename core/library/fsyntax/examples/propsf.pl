:- module(_,_,[hiord,functional,assertions,regtypes,sr/bfall]).

:- regtype color/1. color := red | blue | green.

:- regtype slist/1. slist := [] | [ _ | slist].

:- regtype list_of/2. list_of(T) := [] | [~T | list_of(T)].
