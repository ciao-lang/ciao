% (included file)
:- doc(section, "Data sizes, cost, termination").

:- doc(bug, "Should probably find a better name...").

:- if(defined(optim_comp)).
:- else.
:- export(bound/1).
:- doc(bound/1,"The types approximation (bounding) supported in size
   and cost analysis (see also @lib{resources_basic.pl}).
   @begin{itemize}
   @item @tt{upper}: an upper bound.
   @item @tt{lower}: a lower bound.
   @end{itemize}
").
:- regtype bound(X) # "@var{X} is a direction of approximation (upper
   or lower bound).".

bound(upper).
bound(lower).
:- endif.

% --------------------------------------------------------------------------

:- doc(bug,"size/3 vs. size_ub, size_lb redundant...").

:- if(defined(optim_comp)).
:- else.
:- export(size/2).
:- prop size(X, Y) + no_rtcheck
   # "@var{Y} is the size of argument @var{X}, for any approximation.".

size(_, _).
% :- impl_defined(size/2).
:- endif.

:- if(defined(optim_comp)).
:- else.
:- export(size/3).
:- prop size(A, X, Y) : bound(A) + no_rtcheck
   # "@var{Y} is the size of argument @var{X}, for the approximation @var{A}.".

size(_, _, _).
% :- impl_defined(size/3).
:- endif.

:- export(size_lb/2).
:- doc(size_lb(X, Y), "The minimum size of the terms to which the
   argument @var{Y} is bound is given by the expression
   @var{Y}. Various measures can be used to determine the size of an
   argument, e.g., list-length, term-size, term-depth, integer-value,
   etc. @cite{caslog,granularity-jsc}. See @pred{measure_t/1}.").

:- prop size_lb(X, Y) + no_rtcheck
   # "@var{Y} is a lower bound on the size of argument @var{X}.".

:- if(defined(optim_comp)).
:- '$props'(size_lb/2, [impnat=indefinable]).
:- else.
:- impl_defined(size_lb/2).
:- endif.

:- export(size_ub/2).
:- doc(size_ub(X, Y), "The maximum size of the terms to which the
   argument @var{Y} is bound is given by the expression
   @var{Y}. Various measures can be used to determine the size of an
   argument, e.g., list-length, term-size, term-depth, integer-value,
   etc. @cite{caslog,granularity-jsc}. See @pred{measure_t/1}.").

:- prop size_ub(X, Y) + no_rtcheck
   # "@var{Y} is a upper bound on the size of argument @var{X}.".

:- if(defined(optim_comp)).
:- '$props'(size_ub/2, [impnat=indefinable]).
:- else.
:- impl_defined(size_ub/2).
:- endif.

:- if(defined(optim_comp)).
:- else.
:- export(size_o/2).
:- prop size_o(X, Y) + no_rtcheck
   # "The size of argument @var{X} is in the order of the expression
   @var{Y}.".

:- impl_defined(size_o/2).
:- endif.

% --------------------------------------------------------------------------

:- if(defined(optim_comp)).
:- else.
:- export(size_metric/3).
:- meta_predicate size_metric(goal, ?, ?).
:- prop size_metric(Head, Var, Metric)
   :: measure_t(Metric) + no_rtcheck
   # "@var{Metric} is the measure used to determine the size of the
   terms that @var{Var} is bound to, for any type of approximation.".

size_metric(Goal, _, _) :- call(Goal).
:- endif.

:- if(defined(optim_comp)).
:- else.
:- export(size_metric/4).
:- meta_predicate size_metric(goal, ?, ?, ?).
:- prop size_metric(Head, Approx, Var, Metric)
   :: (bound(Approx), measure_t(Metric)) + no_rtcheck
   # "@var{Metric} is the measure used to determine the size of the
   terms that variable @var{Var} bound to, for the approximation
   @var{Approx}.". % depth/1 See resources_basic

size_metric(Goal, _, _, _) :- call(Goal).
:- endif.

% --------------------------------------------------------------------------

:- if(defined(optim_comp)).
:- else.
:- export(measure_t/1).
:- doc(measure_t/1,"The types of term size measures currently
   supported in size and cost analysis (see also in
   @lib{resources_basic.pl}).
   @begin{itemize}
   @item @tt{int}: The size of the term (which is an integer) is the
     integer value itself.
   @item @tt{length}: The size of the term (which is a list) is its
     length.
   @item @tt{size}: The size is the overall of the term (number of
     subterms).
   @item @tt{depth([_|_])}: The size of the term is its depth.
   @item @tt{void}: Used to indicate that the size of this argument
     should be ignored.
   @end{itemize}
").
%% @item @tt{depth([_|_])}:
%%    The size of the term is the number of rule
%%    applications of its type definition.

:- regtype measure_t(X) # "@var{X} is a term size metric.".

measure_t(void).
measure_t(int).
measure_t(length).
measure_t(size).
measure_t(depth([_|_])).
:- endif.

% --------------------------------------------------------------------------

:- export(steps_lb/2).
:- doc(steps_lb(X, Y), "The minimum computation (in resolution steps)
   spent by any call of the form @var{X} is given by the expression
   @var{Y} @cite{low-bounds-ilps97,granularity-jsc}").

:- meta_predicate steps_lb(goal, ?).
:- prop steps_lb(X, Y) + no_rtcheck
   # "@var{Y} is a lower bound on the cost of any call of the form @var{X}.".

:- if(defined(optim_comp)).
:- '$props'(steps_lb/2, [impnat=indefinable]).
:- else.
steps_lb(Goal, _) :- call(Goal).
:- endif.

:- export(steps_ub/2).
:- doc(steps_ub(X, Y), "The maximum computation (in resolution steps)
   spent by any call of the form @var{X} is given by the expression
   @var{Y} @cite{caslog,granularity-jsc}.").

:- meta_predicate steps_ub(goal, ?).
:- prop steps_ub(X, Y) + no_rtcheck
   # "@var{Y} is a upper bound on the cost of any call of the form @var{X}.".

:- if(defined(optim_comp)).
:- '$props'(steps_ub/2, [impnat=indefinable]).
:- else.
steps_ub(Goal, _) :- call(Goal).
:- endif.

:- export(steps/2).
:- doc(steps(X, Y), "The computation (in resolution steps) spent by
   any call of the form @var{X} is given by the expression @var{Y}").

:- meta_predicate steps(goal, ?).
:- prop steps(X, Y) + no_rtcheck
   # "@var{Y} is the cost (number of resolution steps) of any call of the
   form @var{X}.".

:- if(defined(optim_comp)).
:- '$props'(steps/2, [impnat=indefinable]).
:- else.
steps(Goal, _) :- call(Goal).
:- endif.

:- if(defined(optim_comp)).
:- else.
:- export(steps_o/2).
:- meta_predicate steps_o(goal, ?).
:- prop steps_o(X, Y) + no_rtcheck
   # "@var{Y} is the complexity order of the cost of any call of the form
   @var{X}.".

steps_o(Goal, _) :- call(Goal).
:- endif.

% --------------------------------------------------------------------------

:- if(defined(optim_comp)).
:- else.
:- export(rsize/2). % TODO:[new-resources]
:- prop rsize(Var,SizeDescr) + no_rtcheck
   # "@var{Var} has its size defined by @var{SizeDescr}.".
:- impl_defined(rsize/2).
:- endif.

% --------------------------------------------------------------------------

:- if(defined(optim_comp)).
:- else.
:- export(costb/4). % TODO:[new-resources]
% TODO: missing meta_predicate
:- prop costb(Goal,Resource,Lower,Upper) + no_rtcheck 
   # "@var{Lower} (resp. @var{Upper}) is a (safe) lower (resp. upper)
     bound on the cost of the computation of @var{Goal} expressed in
     terms of @var{Resource} units.".
:- impl_defined(costb/4).
:- endif.

% --------------------------------------------------------------------------

:- export(terminates/1).
:- doc(terminates(X), "Calls of the form @var{X} always terminate.").

:- meta_predicate terminates(goal).
:- prop terminates(X) + no_rtcheck
   # "All calls of the form @var{X} terminate.".

:- if(defined(optim_comp)).
:- '$props'(terminates/1, [impnat=indefinable]).
:- else.
terminates(Goal) :- call(Goal).
:- endif.
