% (included file)
:- doc(section, "Cardinality and exact solutions").

:- if(defined(optim_comp)).
:- else.
:- export(cardinality/3). % TODO:[new-resources]
:- meta_predicate cardinality(goal,?,?).
:- prop cardinality(Goal,Lower,Upper) + no_rtcheck
   # "@var{Goal} has a number of solutions between
   @var{Lower} and @var{Upper}.".
:- impl_defined(cardinality/3).
:- endif.

:- if(defined(optim_comp)).
:- else.
:- export(num_solutions/2).
:- prop num_solutions(X, N) : callable * int # "Calls of the form
   @var{X} have @var{N} solutions, i.e., @var{N} is the cardinality of
   the solution set of @var{X}.".
:- endif.

:- if(defined(optim_comp)).
:- else.
% TODO: change name (this is not correct)
:- meta_predicate num_solutions(goal, addterm(pred(1))).
:- prop num_solutions(Goal, Check) : callable * callable
   # "For a call to @var{Goal}, @pred{Check(X)} succeeds, where
   @var{X} is the number of solutions.".

:- impl_defined(num_solutions/3).
:- endif.

% --------------------------------------------------------------------------

:- doc(bug, "relations/2 is the same as num_solutions/2!").

:- if(defined(optim_comp)).
:- else.
:- export(relations/2).
:- doc(relations(X, N), "Calls of the form @var{X} produce @var{N} solutions,
   i.e., @var{N} is the cardinality of the solution set of @var{X}.").

:- meta_predicate relations(goal, ?).
:- prop relations(X, N) : callable * int + rtcheck(unimplemented)
   # "Goal @var{X} produces @var{N} solutions.".

:- impl_defined(relations/2).
:- endif.

% --------------------------------------------------------------------------

:- export(finite_solutions/1).
:- doc(finite_solutions(X), "Calls of the form @var{X} produce a
   finite number of solutions @cite{non-failure-iclp97}.").

:- meta_predicate finite_solutions(goal).
:- prop finite_solutions(X) + no_rtcheck
   # "All the calls of the form @var{X} have a finite number of
   solutions.".

:- if(defined(optim_comp)).
:- '$props'(finite_solutions/1, [impnat=indefinable]).
:- else.
finite_solutions(Goal) :- call(Goal).
:- endif.

% --------------------------------------------------------------------------

:- if(defined(optim_comp)).
:- else.
:- export(solutions/2).
:- meta_predicate solutions(addterm(goal), ?).
:- prop solutions(Goal, Sols) : callable * list
   # "Goal @var{Goal} produces the solutions listed in @var{Sols}.".

:- impl_defined(solutions/3).
:- endif.
