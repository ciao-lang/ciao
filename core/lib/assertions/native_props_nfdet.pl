% (included file)
:- doc(section, "Determinacy, failure, choice-points").

:- export(det/1).
:- doc(det(X), "Calls of the form @var{X} are deterministic, i.e.,
   produce exactly one solution (or do not terminate).

   Note that it can still leave choice points after its execution, but
   when backtracking into these, it can only fail or go into an
   infinite loop.

   These properties are inferred and checked natively by CiaoPP using
   the domains and techniques of
   @cite{determ-lopstr04,determinacy-ngc09,non-failure-iclp97,nfplai-flops04}.
   ").

:- meta_predicate det(goal).
:- prop det(X) # "Calls of the form @var{X} are deterministic.".

:- if(defined(optim_comp)).
:- '$props'(det/1, [impnat=indefinable]).
:- else.
:- impl_defined(det/1).
:- endif.

:- export(semidet/1).
:- doc(semidet(X), "Calls of the form @var{X} are semi-deterministic,
   i.e., produce at most one solution (or do not terminate).

   Note that it can still leave choice points after its execution, but
   when backtracking into these, it can only fail or go into an
   infinite loop.

   These properties are inferred and checked natively by CiaoPP using
   the domains and techniques of
   @cite{determ-lopstr04,determinacy-ngc09,non-failure-iclp97,nfplai-flops04}.
   ").

:- meta_predicate semidet(goal).
:- prop semidet(X) # "Calls of the form @var{X} are semi-deterministic.".

:- if(defined(optim_comp)).
:- '$props'(semidet/1, [impnat=indefinable]).
:- else.
:- impl_defined(semidet/1).
:- endif.

:- export(multi/1).
:- doc(multi(X), "Calls of the form @var{X} are multi-deterministic,
   i.e., they produce one or more solutions and do not fail.").

:- meta_predicate multi(goal).
:- prop multi(X) # "Calls of the form @var{X} are multi-deterministic.".

:- if(defined(optim_comp)).
:- '$props'(multi/1, [impnat=indefinable]).
:- else.
:- impl_defined(multi/1).
:- endif.

:- export(nondet/1).
:- doc(nondet(X), "Nothing is ensured about failure and determinacy of
   calls to @var{X}. This is the default when no information is given
   for a predicate, so this property does not need to be stated
   explicitly. ").

:- meta_predicate nondet(goal).
:- prop nondet(X) + no_rtcheck
   # "Calls of the form @var{X} are non-deterministic.".

:- if(defined(optim_comp)).
:- '$props'(nondet/1, [impnat=indefinable]).
:- else.
nondet(Goal) :- call(Goal).
:- endif.

% ---------------------------------------------------------------------------
% (Internal for determinism)

:- export(mut_exclusive/1).
:- doc(mut_exclusive(X), "For any call of the form @var{X} there is at
   most one clause whose test (guard) succeeds, i.e., clause tests are
   pairwise exclusive. Note that determinacy is the transitive closure
   (to all called predicates) of this property. This property is
   inferred and checked natively by CiaoPP using the domains and
   techniques of @cite{determ-lopstr04,determinacy-ngc09}.").

:- meta_predicate mut_exclusive(goal).
:- prop mut_exclusive(X) + rtcheck(unimplemented)
   # "For any call of the form @var{X} at most one clause test succeeds.".

:- if(defined(optim_comp)).
:- '$props'(mut_exclusive/1, [impnat=indefinable]).
:- else.
mut_exclusive(Goal) :- call(Goal).
:- endif.

% --------------------------------------------------------------------------

:- export(not_mut_exclusive/1).
:- doc(not_mut_exclusive(X), "For calls of the form @var{X} more than
   one clause test may succeed. I.e., clause tests are not disjoint
   for some call.").

:- meta_predicate not_mut_exclusive(goal).
:- prop not_mut_exclusive(X) + rtcheck(unimplemented)
   # "For some calls of the form @var{X} more than one clause test may
   succeed.".

:- if(defined(optim_comp)).
:- '$props'(not_mut_exclusive/1, [impnat=indefinable]).
:- else.
not_mut_exclusive(Goal) :- call(Goal). 
:- endif.

% --------------------------------------------------------------------------

:- export(possibly_not_mut_exclusive/1).
:- doc(possibly_not_mut_exclusive(X), "Mutual exclusion of the clause
   tests for calls of the form @var{X} cannot be ensured. This is the
   default when no information is given for a predicate, so this
   property does not need to be stated explicitly.").

:- meta_predicate possibly_not_mut_exclusive(goal).
:- prop possibly_not_mut_exclusive(X) + no_rtcheck
   # "Mutual exclusion is not ensured for calls of the form @var{X}.".

:- if(defined(optim_comp)).
:- '$props'(possibly_not_mut_exclusive/1, [impnat=indefinable]).
:- else.
possibly_not_mut_exclusive(Goal) :- call(Goal).
:- endif.

% ---------------------------------------------------------------------------
% (Internal for nonfailure)

:- export(covered/1).
:- doc(covered(X), "For any call of the form @var{X} there is at least
   one clause whose test (guard) succeeds (i.e., all the calls of the
   form @var{X} are covered).  Note that nonfailure is the transitive
   closure (to all called predicates) of this
   property. @cite{non-failure-iclp97,nfplai-flops04}.").

:- meta_predicate covered(goal).
:- prop covered(X) + rtcheck(unimplemented)
   # "All the calls of the form @var{X} are covered.".

:- if(defined(optim_comp)).
:- '$props'(covered/1, [impnat=indefinable]).
:- else.
covered(Goal) :- call(Goal).
:- endif.

% --------------------------------------------------------------------------

:- export(not_covered/1).
:- doc(not_covered(X), "There is some call of the form @var{X} for
   which there is no clause whose test succeeds
   @cite{non-failure-iclp97}.").

:- meta_predicate not_covered(goal).
:- prop not_covered(X) + rtcheck(unimplemented)
   # "Not all of the calls of the form @var{X} are covered.".

:- if(defined(optim_comp)).
:- '$props'(not_covered/1, [impnat=indefinable]).
:- else.
not_covered(Goal) :- call(Goal).
:- endif.

% --------------------------------------------------------------------------

:- export(possibly_not_covered/1).
:- doc(possibly_not_covered(X), "Covering is not ensured for any call of
   the form @var{X}. In other words, nothing can be ensured about
   covering of such calls.").

:- meta_predicate possibly_not_covered(goal).
:- prop possibly_not_covered(X) + no_rtcheck
   # "Covering is not ensured for calls of the form @var{X}.".

:- if(defined(optim_comp)).
:- '$props'(possibly_not_covered/1, [impnat=indefinable]).
:- else.
possibly_not_covered(Goal) :- call(Goal).
:- endif.

% ===========================================================================
% TODO: det/1 without "pending choice points can fail"?

:- if(defined(optim_comp)).
% TODO: merge with optim_comp's version?
:- else.
:- export(no_choicepoints/1).
:- meta_predicate no_choicepoints(goal).
:- prop no_choicepoints(X)
   # "A call to @var{X} does not leave new choicepoints.".

:- impl_defined(no_choicepoints/1).
:- endif.

:- if(defined(optim_comp)).
% TODO: merge with optim_comp's version?
:- else.
:- export(leaves_choicepoints/1).
:- meta_predicate leaves_choicepoints(goal).
:- prop leaves_choicepoints(X)
   # "A call to @var{X} leaves new choicepoints.".

:- impl_defined(leaves_choicepoints/1).
:- endif.

% ===========================================================================
:- doc(subsection, "Properties for separate nf and det domains (To be deprecated)").

:- compilation_fact(old_nfdet). % comment this to disable old properties

:- if(defined(old_nfdet)).
:- export(is_det/1).
:- doc(is_det(X), "All calls of the form @var{X} are deterministic,
   i.e., produce at most one solution (or do not terminate).  In other
   words, if @var{X} succeeds, it can only succeed once. It can still
   leave choice points after its execution, but when backtracking into
   these, it can only fail or go into an infinite loop.  This property
   is inferred and checked natively by CiaoPP using the domains and
   techniques of @cite{determ-lopstr04,determinacy-ngc09}.").

:- meta_predicate is_det(goal).
:- prop is_det(X)
   # "All calls of the form @var{X} are deterministic.".

:- if(defined(optim_comp)).
:- '$props'(is_det/1, [impnat=indefinable]).
:- else.
:- impl_defined(is_det/1).
:- endif.
:- endif.

% --------------------------------------------------------------------------

:- if(defined(old_nfdet)).
:- export(non_det/1).
:- doc(non_det(X), "All calls of the form @var{X} are
   non-deterministic, i.e., they always produce more than one
   solution.").

:- meta_predicate non_det(goal).
:- prop non_det(X)
   # "All calls of the form @var{X} are non-deterministic.".

:- if(defined(optim_comp)).
:- '$props'(non_det/1, [impnat=indefinable]).
:- else.
:- impl_defined(non_det/1).
:- endif.
:- endif.

% --------------------------------------------------------------------------

:- if(defined(old_nfdet)).
:- export(possibly_nondet/1). % TODO: maybe_nondet?
:- doc(possibly_nondet(X), "Non-determinism is not ensured for calls
   of the form @var{X}. In other words, nothing can be ensured about
   determinacy of such calls. This is the default when no information
   is given for a predicate, so this property does not need to be
   stated explicitly. ").

:- meta_predicate possibly_nondet(goal).
:- prop possibly_nondet(X) + no_rtcheck
   # "Non-determinism is not ensured for calls of the form @var{X}.".

:- if(defined(optim_comp)).
:- '$props'(possibly_nondet/1, [impnat=indefinable]).
:- else.
possibly_nondet(Goal) :- call(Goal).
:- endif.
:- endif.

% ---------------------------------------------------------------------------

:- if(defined(old_nfdet)).
:- export(not_fails/1).
:- doc(not_fails(X), "Calls of the form @var{X} produce at least one
   solution (succeed), or do not terminate. This property is inferred
   and checked natively by CiaoPP using the domains and techniques of
   @cite{non-failure-iclp97,nfplai-flops04}.").

:- meta_predicate not_fails(goal).
:- prop not_fails(X) + native
   # "All the calls of the form @var{X} do not fail.".

:- if(defined(optim_comp)).
:- '$props'(not_fails/1, [impnat=indefinable]).
:- else.
:- impl_defined(not_fails/1).
:- endif.
:- endif.

% ---------------------------------------------------------------------------
% (Coincides with new nfdet)

% :- export(fails/1).
% :- doc(fails(X), "Calls of the form @var{X} fail.").
% 
% :- meta_predicate fails(goal).
% :- prop fails(X) + native
%    # "Calls of the form @var{X} fail.".
% 
% :- if(defined(optim_comp)).
% :- '$props'(fails/1, [impnat=indefinable]).
% :- else.
% :- impl_defined(fails/1).
% :- endif.

% --------------------------------------------------------------------------

:- if(defined(old_nfdet)).
:- export(possibly_fails/1). % TODO: may_fail?
:- doc(possibly_fails(X), "Non-failure is not ensured for any call of
   the form @var{X}. In other words, nothing can be ensured about
   non-failure nor termination of such calls.").

:- meta_predicate possibly_fails(goal).
:- prop possibly_fails(X) + no_rtcheck
   # "Non-failure is not ensured for calls of the form @var{X}.".

% TODO: put a note that these are incomplete/probably wrong and may
% give weird results in rtchekcs
:- if(defined(optim_comp)).
:- '$props'(possibly_fails/1, [impnat=indefinable]).
:- else.
possibly_fails(Goal) :- call(Goal).
:- endif.
:- endif.
