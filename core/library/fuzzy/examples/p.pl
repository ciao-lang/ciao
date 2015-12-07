
:- use_module(dicesum5).
%:- use_module(library(clpr/clp_dump)).
:- use_module('/home/clip/Systems/ciao/library/clpr/clp_dump0').

:- multifile dump_constraints/3. % Hook in the toplevel
:- multifile dump_print/1. % Hook in the toplevel

main:-
	small(3,X),
	write(X),
%	dump_internal(X,Copy,Cs),
	dump_constraints(X,Copy,Cs),
	writeq(internal(Copy,Cs)),
	nl.
%	dump_print(X).
