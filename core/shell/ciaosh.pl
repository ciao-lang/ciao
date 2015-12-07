:- module(ciaosh, [main/1], []).

:- use_module(library(toplevel)).

main(Args) :-
	toplevel(Args).
