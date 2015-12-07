%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- class(concur,[],[]).

:- use_module(library(concurrency)). 
:- use_module(library(write)).

:- export([main/1]). 

:- concurrent message/1. 

main(S) :-
	asserta_fact(message(a)),
	retract_fact(message(A)),
	write(S,A),write(S,'.'),nl(S),
	retract_fact(message(B)),
	write(S,B),write(S,'.'),nl(S).
