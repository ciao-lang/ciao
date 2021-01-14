% (included file)
:- doc(section, "Side effects"). % (coarse, for partial evaluation)

:- doc(bug,"Still missing more specific side effects such as dynamic
   predicates, mutables, I/O, etc.").

:- doc(bug,"These need to be unified with the sideff(pure) ones!").

:- export(sideff_pure/1).
:- meta_predicate sideff_pure(goal).
:- prop sideff_pure(X) + no_rtcheck
   # "@var{X} is pure, i.e., has no side-effects.".

:- if(defined(optim_comp)).
:- '$props'(sideff_pure/1, [impnat=indefinable]).
:- else.
sideff_pure(Goal) :- call(Goal).
:- endif.

:- export(sideff_soft/1).
:- meta_predicate sideff_soft(goal).
:- prop sideff_soft(X) + no_rtcheck
   # "@var{X} has @index{soft side-effects}, i.e., those not affecting
   program execution (e.g., input/output).".

:- if(defined(optim_comp)).
:- '$props'(sideff_soft/1, [impnat=indefinable]).
:- else.
sideff_soft(Goal) :- call(Goal).
:- endif.

:- export(sideff_hard/1).
:- meta_predicate sideff_hard(goal).
:- prop sideff_hard(X) + no_rtcheck
   # "@var{X} has @index{hard side-effects}, i.e., those that might affect
   program execution (e.g., assert/retract).".

:- if(defined(optim_comp)).
:- '$props'(sideff_hard/1, [impnat=indefinable]).
:- else.
sideff_hard(Goal) :- call(Goal).
:- endif.
