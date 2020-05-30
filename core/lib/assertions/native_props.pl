:- module(native_props, [], [assertions, regtypes]).

:- doc(title, "Properties which are native to analyzers").
:- doc(author, "Francisco Bueno").
:- doc(author, "Manuel Hermenegildo").
:- doc(author, "Pedro L@'{o}pez").
:- doc(author, "Edison Mera").
:- doc(author, "Amadeo Casas").

:- doc(module, "@cindex{properties, native} This library contains
   a set of properties which are natively understood by the different
   program analyzers of @apl{ciaopp}.  They are used by @apl{ciaopp}
   on output and they can also be used as properties in assertions.

   Some of the properties can also be used as runtime-checks. See
   @lib{native_props_rtc} for the runtime-check implementation of such
   properties.").

:- doc(usage, "@tt{:- use_module(library(assertions/native_props))}

   or also as a package @tt{:- use_package(nativeprops)}.

   Note the slightly different names of the library and the package.").

:- doc(bug, "EMM: specify if run-time checks of some properties are
   complete (exhaustive), incomplete, not possible, or unimplemented").

:- doc(bug, "MH: Some of these properties should be moved to rtchecks
   or testing libs.").

:- doc(bug, "MH: Missing test cases and examples.").

:- if(defined(optim_comp)).
:- else.
:- use_module(engine(hiord_rt)). % call/?
:- endif.

% ===========================================================================
:- doc(section, "Meta-properties: instance and compat").
% TODO: should be at the beginning? in assertions?

:- doc(bug, "MH: Also defined (much better) in basic_props!!").

:- if(defined(optim_comp)).
:- else.
:- export(compat/1).
:- meta_predicate compat(goal).
:- prop compat(Prop) + no_rtcheck
   # "Use @var{Prop} as a compatibility property. Normally used with
   types. See the discussion in @ref{Declaring regular types}.".

compat(_). % processed in rtchecks_basic
:- endif.

% --------------------------------------------------------------------------

:- doc(bug, "MH: The idea was to call it inst/2 (to avoid confusion
   with instance/2). Note that it is also defined --this way-- in  
   basic_props!!"). 
:- doc(bug, "MH: inst/1 not really needed since it is the default? ").

:- if(defined(optim_comp)).
:- else.
:- export(instance/1).
:- meta_predicate instance(goal).
:- prop instance(Prop) + no_rtcheck
# "Use Prop as an instantiation property. Normally used with
   types. See the discussion in @ref{Declaring regular types}.".

instance(_). % processed in rtchecks_basic
:- endif.

% --------------------------------------------------------------------------

:- doc(bug, "succeeds/1 is only used in 2 modules, its name conflicts
   with other notions in assertions, it should be renamed (e.g. to
   test_succeeds/1 or something along that line)").

:- doc(bug, "We probably need a succeeds/1 comp property. It actually
   appears in the CiaoPP tutorial at ciaopp/doc/tutorial.tex").

:- if(defined(optim_comp)).
:- else.
:- export(succeeds/1). % TODO: very crazy. % TODO: rename!
:- meta_predicate succeeds(goal).
:- prop succeeds(Goal) + no_rtcheck # "A call to @var{Goal} succeeds.".

:- impl_defined(succeeds/1).
:- endif.

% ===========================================================================
:- doc(section, "Sharing/aliasing, groundness").

:- export(mshare/1). % TODO: Read as possibly_share
:- doc(mshare(X), "@var{X} contains all @index{sharing sets}
   @cite{jacobs88,abs-int-naclp89} which specify the possible variable
   occurrences in the terms to which the variables involved in the
   clause may be bound. Sharing sets are a compact way of representing
   groundness of variables and dependencies between variables. This
   representation is however generally difficult to read for
   humans. For this reason, this information is often translated to
   @prop{ground/1}, @prop{indep/1} and @prop{indep/2} properties,
   which are easier to read.").

:- prop mshare(X) + (native(sharing(X)), no_rtcheck)
   # "The sharing pattern for the variables in the clause is @tt{@var{X}}.".

% TODO: describe valid mshare like in constraint/1 (e.g. list of lists)?
:- if(defined(optim_comp)).
:- '$props'(mshare/1, [impnat=indefinable]).
:- else.
:- impl_defined(mshare/1).
:- endif.

% --------------------------------------------------------------------------
% Amadeo
:- export(indep/2).
:- prop indep(X, Y) + native(indep([[X, Y]]))
   # "@var{X} and @var{Y} do not have variables in common.".

indep(A, B) :-
    mark(A, Ground), % Ground is var if A ground
    nonvar(Ground), % If 1st argument was ground, no need to proceed
    marked(B), !,
    fail.
indep(_, _).

mark('$$Mark', no) :- !. % Mark the variable, signal variable found
mark(Atom,     _) :- atomic(Atom), !.
mark(Complex,  GR) :- mark_(Complex, 1, GR).

mark_(Args, Mth, GR) :-
    arg(Mth, Args, ThisArg), !,
    mark(ThisArg, GR),
    Nth is Mth+1,
    mark_(Args, Nth, GR).
mark_(_, _, _).

marked(Term) :-
    functor(Term, F, A),
    ( A > 0, !, marked_(Term, 1)
    ; F = '$$Mark' ).

marked_(Args, Mth) :-
    arg(Mth, Args, ThisArg), !,
    ( marked(ThisArg) % TODO: missing cut?
    ; Nth is Mth+1,
      marked_(Args, Nth)
    ).

% --------------------------------------------------------------------------
% Amadeo
:- export(indep/1).
:- prop indep(X) + native(indep(X))
   # "The variables in the the pairs in @tt{@var{X}} are pairwise
   independent.".

indep([]).
indep([[X, Y]|L]) :- indep(X, Y), indep(L).

% --------------------------------------------------------------------------
:- export(covered/2).
:- doc(covered(X, Y), "All variables occuring in @var{X} occur also
   in @var{Y}. Used by the non-strict independence-based annotators.").

:- prop covered(X, Y) + native # "@var{X} is covered by @var{Y}.".

:- if(defined(optim_comp)).
:- '$props'(covered/2, [impnat=indefinable]).
:- else.
:- impl_defined(covered/2).
:- endif.

% --------------------------------------------------------------------------
:- export(linear/1).
:- doc(linear(X), "@var{X} is bound to a term which is linear,
   i.e., if it contains any variables, such variables appear only once
   in the term. For example, @tt{[1,2,3]} and @tt{f(A,B)} are linear
   terms, while @tt{f(A,A)} is not.").

:- prop linear(X) + native
   # "@var{X} is instantiated to a linear term.".

:- if(defined(optim_comp)).
:- '$props'(linear/1, [impnat=indefinable]).
:- else.
:- impl_defined(linear/1).
:- endif.

% --------------------------------------------------------------------------
:- export(nonground/1).
:- prop nonground(X) + native(not_ground(X))
   # "@tt{@var{X}} is not ground.".

:- if(defined(optim_comp)).
:- '$props'(nonground/1, [impnat=indefinable]).
:- else.
% TODO: use rtc_impl!
nonground(X) :- \+ ground(X).
:- endif.

% --------------------------------------------------------------------------
% Jorge 
:- export(clique/1).
:- doc(clique(X), "@var{X} is a set of variables of interest, much the
   same as a sharing group but @var{X} represents all the sharing groups in
   the powerset of those variables. Similar to a sharing group, a clique is
   often translated to @prop{ground/1}, @prop{indep/1}, and @prop{indep/2}
   properties.").

:- prop clique(X) + (native(clique(X)), no_rtcheck)
   # "The clique sharing pattern is @tt{@var{X}}.".

:- if(defined(optim_comp)).
:- '$props'(clique/1, [impnat=indefinable]).
:- else.
:- impl_defined(clique/1).
:- endif.

% --------------------------------------------------------------------------
% Jorge 
:- export(clique_1/1).
:- doc(clique_1(X), "@var{X} is a set of variables of interest, much
   the same as a sharing group but @var{X} represents all the sharing
   groups in the powerset of those variables but disregarding the
   singletons. Similar to a sharing group, a clique_1 is often translated
   to @prop{ground/1}, @prop{indep/1}, and @prop{indep/2} properties.").

:- prop clique_1(X) + (native(clique_1(X)), no_rtcheck)
   # "The 1-clique sharing pattern is @tt{@var{X}}.".

:- if(defined(optim_comp)).
:- '$props'(clique_1/1, [impnat=indefinable]).
:- else.
:- impl_defined(clique_1/1).
:- endif.

% ===========================================================================
:- doc(section, "Determinacy, failure, choice-points").

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

% --------------------------------------------------------------------------

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

% --------------------------------------------------------------------------

:- export(possibly_nondet/1). % TODO: maybe_nondet?
:- doc(possibly_nondet(X), "Non-determinism is not ensured for calls
   of the form @var{X}. In other words, nothing can be ensured about
   determinacy of such calls. This is the default when no information
   is given for a predicate, so this property does not need to be
   stated explicitly. ").

:- meta_predicate possible_nondet(goal).
:- prop possibly_nondet(X) + no_rtcheck
   # "Non-determinism is not ensured for calls of the form @var{X}.".

:- if(defined(optim_comp)).
:- '$props'(possibly_nondet/1, [impnat=indefinable]).
:- else.
possibly_nondet(Goal) :- call(Goal).
:- endif.

% --------------------------------------------------------------------------

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

% ---------------------------------------------------------------------------

:- export(fails/1).
:- doc(fails(X), "Calls of the form @var{X} fail.").

:- meta_predicate fails(goal).
:- prop fails(X) + native
   # "Calls of the form @var{X} fail.".

:- if(defined(optim_comp)).
:- '$props'(fails/1, [impnat=indefinable]).
:- else.
:- impl_defined(fails/1).
:- endif.

% --------------------------------------------------------------------------

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

% --------------------------------------------------------------------------

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

% --------------------------------------------------------------------------

:- if(defined(optim_comp)).
:- else.
:- export(test_type/2). % TODO: internal?
:- meta_predicate test_type(goal, ?).
:- prop test_type(X, T) # "Indicates the type of test that a predicate
   performs.  Required by the nonfailure analyisis.".

test_type(Goal, _) :- call(Goal). 
:- endif.

% --------------------------------------------------------------------------

:- if(defined(optim_comp)).
% TODO: merge with optim_comp's version
:- else.
:- export(no_choicepoints/1).
:- meta_predicate no_choicepoints(goal).
:- prop no_choicepoints(X)
   # "A call to @var{X} does not leave new choicepoints.".

:- impl_defined(no_choicepoints/1).
:- endif.

% --------------------------------------------------------------------------

:- if(defined(optim_comp)).
% TODO: merge with optim_comp's version
:- else.
:- export(leaves_choicepoints/1).
:- meta_predicate leaves_choicepoints(goal).
:- prop leaves_choicepoints(X)
   # "A call to @var{X} leaves new choicepoints.".

:- impl_defined(leaves_choicepoints/1).
:- endif.

% ===========================================================================
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

% ===========================================================================
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

% ===========================================================================
:- doc(section, "Exceptions").

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

% ===========================================================================
:- doc(section, "Signals").

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

% ===========================================================================
:- doc(section, "Other side effects").

:- doc(bug,"Still missing other side effects such as dynamic
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

% ===========================================================================
:- doc(section, "Polyhedral constraints").

:- if(defined(optim_comp)).
:- else.
:- export(constraint/1).
:- doc(constraint(C), "@var{C} contains a list of linear
   (in)equalities that relate variables and @tt{int} values. For
   example, @tt{[A < B + 4]} is a constraint while @tt{[A < BC + 4]}
   or @tt{[A = 3.4, B >= C]} are not. Used by polyhedra-based
   analyses.").

:- prop constraint(C) + native
   # "@var{C} is a list of linear equations.".

% TODO: should we define this here? (term structure of a valid constraint/1)
constraint([]).
constraint([Cons|Rest]) :-
    constraint_(Cons),
    constraint(Rest).

constraint_(=(Lin_Expr1, Lin_Expr2)) :-
    lin_expr(Lin_Expr1),
    lin_expr(Lin_Expr2).
constraint_(=<(Lin_Expr1, Lin_Expr2)) :-
    lin_expr(Lin_Expr1),
    lin_expr(Lin_Expr2).
constraint_(>=(Lin_Expr1, Lin_Expr2)) :-
    lin_expr(Lin_Expr1),
    lin_expr(Lin_Expr2).
constraint_(<(Lin_Expr1, Lin_Expr2)) :-
    lin_expr(Lin_Expr1),
    lin_expr(Lin_Expr2).
constraint_(>(Lin_Expr1, Lin_Expr2)) :-
    lin_expr(Lin_Expr1),
    lin_expr(Lin_Expr2).

lin_expr(PPL_Var) :-
    ppl_var(PPL_Var), !.
lin_expr(Coeff) :-
    coefficient(Coeff).
% lin_expr(+(Lin_Expr), Vars, +(New_Lin_Expr)) :-
%       lin_expr(Lin_Expr, Vars, New_Lin_Expr).
lin_expr(+(Lin_Expr)) :-
    lin_expr(Lin_Expr).
lin_expr(-(Lin_Expr)) :-
    lin_expr(Lin_Expr).
lin_expr(+(Lin_Expr1, Lin_Expr2)) :-
    lin_expr(Lin_Expr1),
    lin_expr(Lin_Expr2).
lin_expr(-(Lin_Expr1, Lin_Expr2)) :-
    lin_expr(Lin_Expr1),
    lin_expr(Lin_Expr2).
lin_expr(*(Coeff, Lin_Expr)) :-
    coefficient(Coeff),
    lin_expr(Lin_Expr).
lin_expr(*(Lin_Expr, Coeff)) :-
    coefficient(Coeff),
    lin_expr(Lin_Expr).

ppl_var(Var) :-
    var(Var).

coefficient(Coeff) :-
    ground(Coeff),
    int(Coeff). % TODO: couldn't it be a num/1?
:- endif.

% ===========================================================================
:- doc(section, "Other properties").

:- doc(bug, "JF: move somewhere else, only for oo_types and java_cha").

:- if(defined(optim_comp)).
:- else.
:- doc(tau(Types), "@var{Types} contains a list with the type associations
   for each variable, in the form @tt{V/[T1,..,TN]}.").

:- export(tau/1).
:- prop tau(TypeInfo) + native
   # "@var{Types} is a list of associations between variables and list of types".

tau([]).
tau([Var/Type|R]) :-
    var(Var),
    list(Type),
    valid_type(Type),
    tau(R).

valid_type([Type]) :-
    atom(Type).
valid_type([Type|Rest]) :-
    atom(Type),
    valid_type(Rest).
:- endif.

% --------------------------------------------------------------------------

:- doc(bug, "Should be in unittest_props library?").

:- if(defined(optim_comp)).
:- else.
:- export(user_output/2).
:- meta_predicate user_output(goal, ?).
:- prop user_output(Goal, S)
   # "Calls of the form @var{Goal} write @var{S} to standard output.".
:- impl_defined(user_output/2).
:- endif.

%% :- export(user_error/2).
%% :- prop user_error(Goal, S) #
%%      "Calls of the form @var{Goal} write @var{S} to standard error.".
%% 
%% :- meta_predicate user_error(goal, ?).
%% :- impl_defined(user_error/2).

% --------------------------------------------------------------------------

:- doc(bug, "MH: not really clear why this should be here.").

% Built-in in CiaoPP
% if you change this declaration, you have to change ciaoPP:
:- export(entry_point_name/2).
:- doc(hide, entry_point_name/2).
:- meta_predicate entry_point_name(goal, ?).
:- prop entry_point_name/2 + no_rtcheck.
:- if(defined(optim_comp)).
:- '$props'(entry_point_name/2, [impnat=indefinable]).
:- else.
:- impl_defined(entry_point_name/2).
:- endif.
