:- module(_, [], [dynmod_holder]).
% (Holder for doccfg modules)

:- export(call_unknown/1).
% TODO: Use interfaces?
call_unknown(G) :-
	prolog_flag(unknown, Old,  fail),
	prolog_flag(quiet,   QOld, error),
	( call(G),
	  prolog_flag(unknown, _, Old),
	  prolog_flag(quiet, _, QOld)
	; prolog_flag(unknown, _, Old),
	  prolog_flag(quiet, _, QOld),
	  fail
	).

