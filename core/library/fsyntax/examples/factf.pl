:- module(_,_,[functional]).

fact(N) := N=0 ? 1
         | N>0 ? N * fact(--N).
