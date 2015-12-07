:- module(resources_tr, [add_mod_to_directives/3],
	      [assertions, nortchecks]).

:- doc(bug, "We are adding the actual module as the module
	qualifier of F.  This is wrong, because we should add the
	Module where F was defined, and could be different.").

add_mod_to_directives(':-'(head_cost(Approx, Res, F)),
	    ':-'(head_cost(Approx, Res, NF)), M) :-
	atom(F),
	!,
	concat_module(M, F, NF).
add_mod_to_directives(':-'(literal_cost(Approx, Res, F)),
	    ':-'(literal_cost(Approx, Res, NF)), M) :-
	atom(F),
	!,
	concat_module(M, F, NF).


concat_module(Mod, Pred, ModPred) :-
	atom_concat(Mod,  ':',  Mod0),
	atom_concat(Mod0, Pred, ModPred).
