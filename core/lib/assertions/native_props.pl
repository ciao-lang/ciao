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

% TODO: Improve documentation saying if the run-time checks of some
% TODO: properties are complete (exhaustive), incomplete, not possible
% TODO: or unimplemented --EMM.

:- doc(bug, "MH: Some of these properties should be moved to rtchecks
   or testing libs.").

:- doc(bug, "MH: Missing test cases and examples.").

%% :- reexport(engine(term_typing),[ground/1,nonvar/1,var/1]).
%% :- doc(doinclude,ground/1).
%% :- doc(doinclude,nonvar/1).
%% :- doc(doinclude,var/1).
%% 
%% :- reexport(engine(basic_props),[regtype/1, native/2, native/1, sideff/2,
%%         term/1, int/1, nnegint/1, flt/1, num/1, atm/1, struct/1, gnd/1]).

%% TODO MH - Put only in the relevant parts
:- set_prolog_flag(multi_arity_warnings, off).
:- use_module(engine(hiord_rt)). % call/?

% ===========================================================================
:- doc(section, "Meta-properties: instance and compat").
% TODO: should be at the beginning? in assertions?

:- doc(bug, "MH: Also defined (much better) in basic_props!!").

:- export(compat/1).
:- prop compat(Prop) + no_rtcheck
# "Use @var{Prop} as a compatibility property. Normally used with
   types. See the discussion in @ref{Declaring regular types}.".

:- meta_predicate compat(goal).
compat(_). % processed in rtchecks_basic

% --------------------------------------------------------------------------

:- doc(bug, "MH: The idea was to call it inst/2 (to avoid confusion
   with instance/2). Note that it is also defined --this way-- in  
   basic_props!!"). 
:- doc(bug, "MH: inst/1 not really needed since it is the default? ").

:- export(instance/1).
:- prop instance(Prop) + no_rtcheck
# "Use Prop as an instantiation property. Normally used with
   types. See the discussion in @ref{Declaring regular types}.".

:- meta_predicate instance(goal).
instance(_). % processed in rtchecks_basic

% TODO: JF: The following does not seem the right instance/2 (the one
%   in CiaoPP's native.pl has a form native_props:instance(V,T), where
%   T seems a type). terms_check:instance/2 is a completely different
%   one.
%
% :- reexport(library(terms_check), [instance/2]).
% :- doc(doinclude, [instance/2]).

% --------------------------------------------------------------------------

:- doc(bug, "succeeds/1 is only used in 2 modules, its name conflicts
   with other notions in assertions, it should be renamed (e.g. to
   test_succeeds/1 or something along that line)").

:- doc(bug, "We probably need a succeeds/1 comp property. It actually
   appears in the CiaoPP tutorial at ciaopp/doc/tutorial.tex").

:- export(succeeds/1). % TODO: very crazy. % TODO: rename!
:- prop succeeds(Goal) + no_rtcheck # "A call to @var{Goal} succeeds.".

:- meta_predicate succeeds(goal).

:- impl_defined(succeeds/1).

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
:- impl_defined(mshare/1).

% --------------------------------------------------------------------------
% Amadeo
:- export(indep/2).
:- trust prop indep(X, Y) + native(indep([[X, Y]]))
# "@var{X} and @var{Y} do not have variables in common.".

indep(A, B) :-
    mark(A, Ground), % Ground is var if A ground
    nonvar(Ground), % If 1st argument was ground, no need to proceed
    marked(B), !,
    fail.
indep(_, _).

mark('$$Mark', no) :- !. % Mark the variable, signal variable found
mark(Atom,     _) :- atomic(Atom), !.
mark(Complex,  GR) :- mark(Complex, 1, GR).

mark(Args, Mth, GR) :-
    arg(Mth, Args, ThisArg), !,
    mark(ThisArg, GR),
    Nth is Mth+1,
    mark(Args, Nth, GR).
mark(_, _, _).

marked(Term) :-
    functor(Term, F, A),
    ( A > 0, !, marked(Term, 1)
    ; F = '$$Mark' ).

marked(Args, Mth) :-
    arg(Mth, Args, ThisArg), !,
    ( marked(ThisArg)
    ; Nth is Mth+1,
        marked(Args, Nth)
    ).

% --------------------------------------------------------------------------
% Amadeo
:- export(indep/1).
:- trust prop indep(X) + native(indep(X))
# "The variables in the the pairs in @tt{@var{X}} are pairwise independent.".

indep([]).
indep([[X, Y]|L]) :- indep(X, Y), indep(L).

:- doc(linear(X), "@var{X} is bound to a term which is linear,
   i.e., if it contains any variables, such variables appear only once
   in the term. For example, @tt{[1,2,3]} and @tt{f(A,B)} are linear
   terms, while @tt{f(A,A)} is not.").

% --------------------------------------------------------------------------
:- export(covered/2).
:- doc(covered(X, Y), "All variables occuring in @var{X} occur also
   in @var{Y}. Used by the non-strict independence-based annotators.").

:- trust prop covered(X, Y) + native # "@var{X} is covered by @var{Y}.".

:- impl_defined(covered/2).

% --------------------------------------------------------------------------
:- export(linear/1).
:- trust prop linear(X) + native
# "@var{X} is instantiated to a linear term.".

:- impl_defined(linear/1).

% --------------------------------------------------------------------------
:- export(nonground/1).
:- prop nonground(X) + native(not_ground(X))
# "@tt{@var{X}} is not ground.".

nonground(X) :- \+ ground(X).

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

:- impl_defined(clique/1).

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

:- impl_defined(clique_1/1).

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

:- prop is_det(X)
# "All calls of the form @var{X} are deterministic.".

:- meta_predicate is_det(goal).

:- impl_defined(is_det/1).

% --------------------------------------------------------------------------

:- export(non_det/1).
:- doc(non_det(X), "All calls of the form @var{X} are
   non-deterministic, i.e., they always produce more than one
   solution.").

:- prop non_det(X)
# "All calls of the form @var{X} are non-deterministic.".

:- meta_predicate non_det(goal).

:- impl_defined(non_det/1).

% --------------------------------------------------------------------------

:- export(possibly_nondet/1). % TODO: maybe_nondet?
:- doc(possibly_nondet(X), "Non-determinism is not ensured for calls
   of the form @var{X}. In other words, nothing can be ensured about
   determinacy of such calls. This is the default when no information
   is given for a predicate, so this property does not need to be
   stated explicitly. ").

:- prop possibly_nondet(X) + no_rtcheck
# "Non-determinism is not ensured for calls of the form @var{X}.".

:- meta_predicate possible_nondet(goal).

possibly_nondet(Goal) :- call(Goal).

% --------------------------------------------------------------------------

:- export(mut_exclusive/1).
:- doc(mut_exclusive(X), "For any call of the form @var{X} at most one
   clause succeeds, i.e., clauses are pairwise exclusive. Note that
   determinacy is the transitive closure (to all called predicates) of
   this property. This property is inferred and checked natively by
   CiaoPP using the domains and techniques of
   @cite{determ-lopstr04,determinacy-ngc09}.").

:- prop mut_exclusive(X) + rtcheck(unimplemented)
# "For any call of the form @var{X} at most one clause succeeds.".

:- meta_predicate mut_exclusive(goal).

mut_exclusive(Goal) :- call(Goal).

% --------------------------------------------------------------------------

:- export(not_mut_exclusive/1).
:- doc(not_mut_exclusive(X), "For calls of the form @var{X} more
   than one clause may succeed. I.e., clauses are not disjoint for
   some call.").

%% For any call of the form @var{X} at most one
%% clause succeeds, i.e. clauses are pairwise exclusive.").

:- prop not_mut_exclusive(X) + rtcheck(unimplemented)
# "For some calls of the form @var{X} more than one clause
may succeed.".

:- meta_predicate not_mut_exclusive(goal).

not_mut_exclusive(Goal) :- call(Goal). 

% --------------------------------------------------------------------------

:- export(possibly_not_mut_exclusive/1).
:- doc(possibly_not_mut_exclusive(X), "Mutual exclusion of the clauses
   for calls of the form @var{X} cannot be ensured. This is the
   default when no information is given for a predicate, so this
   property does not need to be stated explicitly.").

:- prop possibly_not_mut_exclusive(X) + no_rtcheck
# "Mutual exclusion is not ensured for calls of the form @var{X}.".

:- meta_predicate possibly_not_mut_exclusive(goal).

possibly_not_mut_exclusive(Goal) :- call(Goal).

% ---------------------------------------------------------------------------

:- export(not_fails/1).
:- doc(not_fails(X), "Calls of the form @var{X} produce at least one
   solution (succeed), or do not terminate. This property is inferred
   and checked natively by CiaoPP using the domains and techniques of
   @cite{non-failure-iclp97,nfplai-flops04}.").

:- trust prop not_fails(X) + native
# "All the calls of the form @var{X} do not fail.".

:- meta_predicate not_fails(goal).

:- impl_defined(not_fails/1).

% --------------------------------------------------------------------------

:- export(fails/1).
:- doc(fails(X), "Calls of the form @var{X} fail.").

:- trust prop fails(X) + native
# "Calls of the form @var{X} fail.".

:- meta_predicate fails(goal).

:- impl_defined(fails/1).

% --------------------------------------------------------------------------

:- export(possibly_fails/1). % TODO: may_fail?
:- doc(possibly_fails(X), "Non-failure is not ensured for any call of
   the form @var{X}. In other words, nothing can be ensured about
   non-failure nor termination of such calls.").

:- prop possibly_fails(X) + no_rtcheck
# "Non-failure is not ensured for calls of the form @var{X}.".

:- meta_predicate possibly_fails(goal).
% TODO: put a note that these are incomplete/probably wrong and may
% give weird results in rtchekcs
possibly_fails(Goal) :- call(Goal).

% --------------------------------------------------------------------------

:- export(covered/1). 
:- doc(covered(X), "For any call of the form @var{X} there is at least
   one clause whose test (guard) succeeds (i.e., all the calls of the
   form @var{X} are covered).  Note that nonfailure is the transitive
   closure (to all called predicates) of this
   property. @cite{non-failure-iclp97,nfplai-flops04}.").

:- prop covered(X) + rtcheck(unimplemented)
# "All the calls of the form @var{X} are covered.".

covered(Goal) :- call(Goal).

% --------------------------------------------------------------------------

:- export(not_covered/1).
:- doc(not_covered(X), "There is some call of the form @var{X} for
   which there is no clause whose test succeeds
   @cite{non-failure-iclp97}.").

:- prop not_covered(X) + rtcheck(unimplemented)
# "Not all of the calls of the form @var{X} are covered.".

not_covered(Goal) :- call(Goal).

% --------------------------------------------------------------------------

:- export(possibly_not_covered/1).
:- doc(possibly_not_covered(X), "Covering is not ensured for any call of
   the form @var{X}. In other words, nothing can be ensured about
   covering of such calls.").

:- prop possibly_not_covered(X) + no_rtcheck
# "Covering is not ensured for calls of the form @var{X}.".

:- meta_predicate possibly_not_covered(goal).

possibly_not_covered(Goal) :- call(Goal).

% --------------------------------------------------------------------------

:- export(test_type/2).
:- prop test_type(X, T) # "Indicates the type of test that a predicate
    performs.  Required by the nonfailure analyisis.".

:- meta_predicate test_type(goal, ?).

test_type(Goal, _) :- call(Goal).

% --------------------------------------------------------------------------

:- export(no_choicepoints/1).
:- prop no_choicepoints(X)
# "A call to @var{X} does not leave new choicepoints.".

:- meta_predicate no_choicepoints(goal).

:- impl_defined(no_choicepoints/1).

% --------------------------------------------------------------------------

:- export(leaves_choicepoints/1).
:- prop leaves_choicepoints(X)
# "A call to @var{X} leaves new choicepoints.".

:- meta_predicate leaves_choicepoints(goal).

:- impl_defined(leaves_choicepoints/1).

% ===========================================================================
:- doc(section, "Cardinality and exact solutions").

:- export(cardinality/3). % TODO:[new-resources]
:- prop cardinality(Goal,Lower,Upper) + no_rtcheck
   # "@var{Goal} has a number of solutions between
      @var{Lower} and @var{Upper}.".
:- impl_defined(cardinality/3).

:- export(num_solutions/2).
:- prop num_solutions(X, N) : callable * int # "Calls of the form
   @var{X} have @var{N} solutions, i.e., @var{N} is the cardinality of
   the solution set of @var{X}.".

% TODO: change name (this is not correct)
:- prop num_solutions(Goal, Check) : callable * callable
# "For a call to @var{Goal}, @pred{Check(X)} succeeds, where @var{X} is
   the number of solutions.".

:- meta_predicate num_solutions(goal, addterm(pred(1))).

:- impl_defined(num_solutions/3).

% --------------------------------------------------------------------------

:- doc(bug, "relations/2 is the same as num_solutions/2!").

:- export(relations/2).
:- doc(relations(X, N), "Calls of the form @var{X} produce @var{N} solutions,
   i.e., @var{N} is the cardinality of the solution set of @var{X}.").

:- prop relations(X, N) : callable * int + rtcheck(unimplemented)
# "Goal @var{X} produces @var{N} solutions.".

:- meta_predicate relations(goal, ?).

:- impl_defined(relations/2).

% --------------------------------------------------------------------------

:- export(finite_solutions/1).
:- doc(finite_solutions(X), "Calls of the form @var{X} produce a
   finite number of solutions @cite{non-failure-iclp97}.").

:- prop finite_solutions(X) + no_rtcheck
# "All the calls of the form @var{X} have a finite number of
   solutions.".

:- meta_predicate finite_solutions(goal).

finite_solutions(Goal) :- call(Goal).

% --------------------------------------------------------------------------

:- export(solutions/2).
:- prop solutions(Goal, Sols) : callable * list
# "Goal @var{Goal} produces the solutions listed in @var{Sols}.".

:- meta_predicate solutions(addterm(goal), ?).

:- impl_defined(solutions/3).

% ===========================================================================
:- doc(section, "Data sizes, cost, termination").

:- export(size/2).
:- prop size(X, Y) + no_rtcheck
# "@var{Y} is the size of argument @var{X}, for any approximation.".

size(_, _).
% :- impl_defined(size/2).

% --------------------------------------------------------------------------

:- doc(bug,"size/3 vs. size_ub, size_lb redundant...").

:- export(size/3).
:- prop size(A, X, Y) : bound(A) + no_rtcheck
# "@var{Y} is the size of argument @var{X}, for the approximation @var{A}.".

size(_, _, _).
% :- impl_defined(size/3).

% --------------------------------------------------------------------------

:- export(size_lb/2).
:- doc(size_lb(X, Y), "The minimum size of the terms to which the
   argument @var{Y} is bound is given by the expression
   @var{Y}. Various measures can be used to determine the size of an
   argument, e.g., list-length, term-size, term-depth, integer-value,
   etc. @cite{caslog,granularity-jsc}. See @pred{measure_t/1}.").

:- prop size_lb(X, Y) + no_rtcheck
# "@var{Y} is a lower bound on the size of argument @var{X}.".

:- impl_defined(size_lb/2).

% --------------------------------------------------------------------------

:- export(size_ub/2).
:- doc(size_ub(X, Y), "The maximum size of the terms to which the
   argument @var{Y} is bound is given by the expression
   @var{Y}. Various measures can be used to determine the size of an
   argument, e.g., list-length, term-size, term-depth, integer-value,
   etc. @cite{caslog,granularity-jsc}. See @pred{measure_t/1}.").

:- prop size_ub(X, Y) + no_rtcheck
# "@var{Y} is a upper bound on the size of argument @var{X}.".

:- impl_defined(size_ub/2).

% --------------------------------------------------------------------------

:- export(size_o/2).
:- prop size_o(X, Y) + no_rtcheck
# "The size of argument @var{X} is in the order of the expression @var{Y}.".

:- impl_defined(size_o/2).

% --------------------------------------------------------------------------

:- export(size_metric/3).
:- prop size_metric(Head, Var, Metric)
    :: measure_t(Metric) + no_rtcheck

# "@var{Metric} is the measure used to determine the size of the terms
   that @var{Var} is bound to, for any type of approximation.".

:- meta_predicate size_metric(goal, ?, ?).
size_metric(Goal, _, _) :- call(Goal).

% --------------------------------------------------------------------------

:- export(size_metric/4).
:- prop size_metric(Head, Approx, Var, Metric)
    :: (bound(Approx), measure_t(Metric)) + no_rtcheck

# "@var{Metric} is the measure used to determine the size of the terms
   that variable @var{Var} bound to, for the approximation
   @var{Approx}.". % depth/1 See resources_basic

:- meta_predicate size_metric(goal, ?, ?, ?).
size_metric(Goal, _, _, _) :- call(Goal).

% --------------------------------------------------------------------------

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

%%    The size of the term is the number of rule
%%    applications of its type definition.

   @item @tt{void}: Used to indicate that the size of this argument
     should be ignored.

   @end{itemize}

").

:- regtype measure_t(X) # "@var{X} is a term size metric.".

measure_t(void).
measure_t(int).
measure_t(length).
measure_t(size).
measure_t(depth([_|_])).

% --------------------------------------------------------------------------

:- doc(bug, "Should probably find a better name...").

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

% --------------------------------------------------------------------------

:- export(steps_lb/2).
:- doc(steps_lb(X, Y), "The minimum computation (in resolution steps)
   spent by any call of the form @var{X} is given by the expression
   @var{Y} @cite{low-bounds-ilps97,granularity-jsc}").

:- prop steps_lb(X, Y) + no_rtcheck
# "@var{Y} is a lower bound on the cost of any call of the form
@var{X}.".

:- meta_predicate steps_lb(goal, ?).
steps_lb(Goal, _) :- call(Goal).

%% lower_time(X,Y)
%% # "The minimum computation time spent by calls of the form @var{X} is
%%    given by the expression @var{Y}.".

% --------------------------------------------------------------------------

:- export(steps_ub/2).
:- doc(steps_ub(X, Y), "The maximum computation (in resolution steps)
   spent by any call of the form @var{X} is given by the expression
   @var{Y} @cite{caslog,granularity-jsc}.").

:- prop steps_ub(X, Y) + no_rtcheck

# "@var{Y} is a upper bound on the cost of any call of the form
   @var{X}.".

:- meta_predicate steps_ub(goal, ?).
steps_ub(Goal, _) :- call(Goal).

%% upper_time(X,Y)
%% # "The maximum computation time spent by calls of the form @var{X} is
%%    given by the expression @var{Y}.".

% --------------------------------------------------------------------------

:- export(steps/2).
:- doc(steps(X, Y), "The computation (in resolution steps) spent by
   any call of the form @var{X} is given by the expression @var{Y}").

:- prop steps(X, Y) + no_rtcheck
# "@var{Y} is the cost (number of resolution steps) of any call of the
   form @var{X}.".

:- meta_predicate steps(goal, ?).
steps(Goal, _) :- call(Goal).

% --------------------------------------------------------------------------

:- export(steps_o/2).
:- prop steps_o(X, Y) + no_rtcheck
# "@var{Y} is the complexity order of the cost of any call of the form
   @var{X}.".

:- meta_predicate steps_o(goal, ?).
steps_o(Goal, _) :- call(Goal).

% --------------------------------------------------------------------------

:- export(rsize/2). % TODO:[new-resources]
:- prop rsize(Var,SizeDescr) + no_rtcheck
   # "@var{Var} has its size defined by @var{SizeDescr}.".
:- impl_defined(rsize/2).

% --------------------------------------------------------------------------

:- export(costb/4). % TODO:[new-resources]
:- prop costb(Goal,Resource,Lower,Upper) + no_rtcheck 
   # "@var{Lower} (resp. @var{Upper}) is a (safe) lower (resp. upper)
     bound on the cost of the computation of @var{Goal} expressed in
     terms of @var{Resource} units.".
:- impl_defined(costb/4).

% --------------------------------------------------------------------------

:- export(terminates/1).
:- doc(terminates(X), "Calls of the form @var{X} always terminate.").

:- prop terminates(X) + no_rtcheck
# "All calls of the form @var{X} terminate.".

:- meta_predicate terminates(goal).
terminates(Goal) :- call(Goal).

% ===========================================================================
:- doc(section, "Exceptions").

:- export(exception/1).
:- prop exception(Goal, E) # "Calls to @var{Goal} will throw an
    exception that unifies with @var{E}.".

:- meta_predicate exception(goal, ?).

:- impl_defined(exception/2).

% --------------------------------------------------------------------------

:- export(exception/2).
:- prop exception(Goal)
# "Calls of the form @var{Goal} will throw an (unspecified) exception.".

:- meta_predicate exception(goal).

:- impl_defined(exception/1).

% --------------------------------------------------------------------------

:- export(possible_exceptions/2).
:- prop possible_exceptions(Goal, Es) : list(Es) + rtcheck(unimplemented)
# "Calls of the form @var{Goal} may throw exceptions, but only
   the ones that unify with the terms listed in @var{Es}.".

:- meta_predicate possible_exceptions(goal, ?).

possible_exceptions(Goal, _E) :- call(Goal).

% --------------------------------------------------------------------------

:- export(no_exception/1).
:- prop no_exception(Goal)
# "Calls of the form @var{Goal} do not throw any exception.".

:- meta_predicate no_exception(goal).

:- impl_defined(no_exception/1).

% --------------------------------------------------------------------------

:- export(no_exception/2).
:- prop no_exception(Goal, E)
# "Calls of the form @var{Goal} do not throw any exception that
  unifies with @var{E}.".

:- meta_predicate no_exception(goal, ?).

:- impl_defined(no_exception/2).

% ===========================================================================
:- doc(section, "Signals").

:- export(signal/1).
:- prop signal(Goal)
# "Calls to @var{Goal} will send an (unspecified) signal.".

:- meta_predicate signal(goal).

:- impl_defined(signal/1).

% --------------------------------------------------------------------------

:- export(signal/2).
:- prop signal(Goal, E)
# "Calls to @var{Goal} will send a signal that unifies with @var{E}.".

:- meta_predicate signal(goal, ?).

:- impl_defined(signal/2).

% --------------------------------------------------------------------------

:- export(possible_signals/2).
:- prop possible_signals(Goal, Es) + rtcheck(unimplemented)
# "Calls of the form @var{Goal} may generate signals, but only the
   ones that unify with the terms listed in @var{Es}.".

:- meta_predicate possible_signals(goal, ?).

possible_signals(Goal, _E) :- call(Goal).

% --------------------------------------------------------------------------

:- export(no_signal/1).
:- prop no_signal(Goal)
# "Calls of the form @var{Goal} do not send any signal.".

:- meta_predicate no_signal(goal).

:- impl_defined(no_signal/1).

% --------------------------------------------------------------------------

:- export(no_signal/2).
:- prop no_signal(Goal, E)
# "Calls of the form @var{Goal} do not send any signals that unify
  with @var{E}.".

:- meta_predicate no_signal(goal, ?).

:- impl_defined(no_signal/2).

% ===========================================================================
:- doc(section, "Other side effects").

:- doc(bug,"Still missing other side effects such as dynamic
   predicates, mutables, I/O, etc.").

:- doc(bug,"These need to be unified with the sideff(pure) ones!").

:- export(sideff_pure/1).
:- prop sideff_pure(X) + no_rtcheck
# "@var{X} is pure, i.e., has no side-effects.".

:- meta_predicate sideff_pure(goal).
sideff_pure(Goal) :- call(Goal).

% --------------------------------------------------------------------------

:- export(sideff_soft/1).
:- prop sideff_soft(X) + no_rtcheck
# "@var{X} has @index{soft side-effects}, i.e., those not affecting
   program execution (e.g., input/output).".

:- meta_predicate sideff_soft(goal).
sideff_soft(Goal) :- call(Goal).

% --------------------------------------------------------------------------

:- export(sideff_hard/1).
:- prop sideff_hard(X) + no_rtcheck
# "@var{X} has @index{hard side-effects}, i.e., those that might affect
   program execution (e.g., assert/retract).".

:- meta_predicate sideff_hard(goal).
sideff_hard(Goal) :- call(Goal).

% ===========================================================================
:- doc(section, "Polyhedral constraints").

:- export(constraint/1).
:- doc(constraint(C), "@var{C} contains a list of linear
   (in)equalities that relate variables and @tt{int} values. For
   example, @tt{[A < B + 4]} is a constraint while @tt{[A < BC + 4]}
   or @tt{[A = 3.4, B >= C]} are not. Used by polyhedra-based
   analyses.").

:- trust prop constraint(C) + native
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

% ===========================================================================
:- doc(section, "Other properties").

:- doc(bug, "Needs reorganization...").

:- doc(tau(Types), "@var{Types} contains a list with the type associations
   for each variable, in the form @tt{V/[T1,..,TN]}. Note that tau is used
   in object-oriented programs only").

:- export(tau/1).
:- trust prop tau(TypeInfo) + native
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

% --------------------------------------------------------------------------

:- doc(bug, "Should be in unittest_props library?").

:- export(user_output/2).
:- prop user_output(Goal, S) #
    "Calls of the form @var{Goal} write @var{S} to standard output.".
:- meta_predicate user_output(goal, ?).
:- impl_defined(user_output/2).

%% :- export(user_error/2).
%% :- prop user_error(Goal, S) #
%%      "Calls of the form @var{Goal} write @var{S} to standard error.".
%% 
%% :- meta_predicate user_error(goal, ?).
%% :- impl_defined(user_error/2).

:- doc(bug, "MH: not really clear why this should be here.").

% Built-in in CiaoPP
:- export(entry_point_name/2).
:- doc(hide, entry_point_name/2).

:- prop entry_point_name/2 + no_rtcheck.
% if you change this declaration, you have to change ciaoPP:

:- meta_predicate entry_point_name(goal, ?).
:- impl_defined(entry_point_name/2).

% ------------------------------------------------------------------
