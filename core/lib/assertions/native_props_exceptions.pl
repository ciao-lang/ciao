% (included file)
:- doc(section, "Exceptions and signals").

:- if(defined(optim_comp)).
:- else.
:- export(exception/1).
:- meta_predicate exception(goal, ?).
:- prop exception(Goal, E)
   # "Calls to @var{Goal} will throw an exception that unifies with @var{E}.".

:- impl_defined(exception/2).
:- endif.

:- if(defined(optim_comp)).
:- else.
:- export(exception/2).
:- meta_predicate exception(goal).
:- prop exception(Goal)
   # "Calls of the form @var{Goal} will throw an (unspecified) exception.".

:- impl_defined(exception/1).
:- endif.

:- if(defined(optim_comp)).
:- else.
:- export(possible_exceptions/2).
:- meta_predicate possible_exceptions(goal, ?).
:- prop possible_exceptions(Goal, Es) : list(Es) + rtcheck(unimplemented)
   # "Calls of the form @var{Goal} may throw exceptions, but only the
   ones that unify with the terms listed in @var{Es}.".

possible_exceptions(Goal, _E) :- call(Goal).
:- endif.

:- if(defined(optim_comp)).
:- else.
:- export(no_exception/1).
:- meta_predicate no_exception(goal).
:- prop no_exception(Goal)
   # "Calls of the form @var{Goal} do not throw any exception.".

:- impl_defined(no_exception/1).
:- endif.

:- if(defined(optim_comp)).
:- else.
:- export(no_exception/2).
:- meta_predicate no_exception(goal, ?).
:- prop no_exception(Goal, E)
   # "Calls of the form @var{Goal} do not throw any exception that
   unifies with @var{E}.".

:- impl_defined(no_exception/2).
:- endif.

% ---------------------------------------------------------------------------

:- if(defined(optim_comp)).
:- else.
:- export(signal/1).
:- meta_predicate signal(goal).
:- prop signal(Goal) # "Calls to @var{Goal} will send an (unspecified)
   signal.".

:- impl_defined(signal/1).
:- endif.

:- if(defined(optim_comp)).
:- else.
:- export(signal/2).
:- meta_predicate signal(goal, ?).
:- prop signal(Goal, E) # "Calls to @var{Goal} will send a signal that
   unifies with @var{E}.".

:- impl_defined(signal/2).
:- endif.

:- if(defined(optim_comp)).
:- else.
:- export(possible_signals/2).
:- meta_predicate possible_signals(goal, ?).
:- prop possible_signals(Goal, Es) + rtcheck(unimplemented)
   # "Calls of the form @var{Goal} may generate signals, but only the
   ones that unify with the terms listed in @var{Es}.".

possible_signals(Goal, _E) :- call(Goal).
:- endif.

:- if(defined(optim_comp)).
:- else.
:- export(no_signal/1).
:- meta_predicate no_signal(goal).
:- prop no_signal(Goal)
   # "Calls of the form @var{Goal} do not send any signal.".

:- impl_defined(no_signal/1).
:- endif.

:- if(defined(optim_comp)).
:- else.
:- export(no_signal/2).
:- meta_predicate no_signal(goal, ?).
:- prop no_signal(Goal, E)
   # "Calls of the form @var{Goal} do not send any signals that unify
   with @var{E}.".

:- impl_defined(no_signal/2).
:- endif.
