:- module(native_props, [
% Meta-properties:
% (should be at the beginning? in assertions?) 
		compat/1,
		instance/1,
		% instance/2 % reexported from terms_check

% Sharing/aliasing, groundness:
     		mshare/1, % Read as possibly_share
		indep/2,
		indep/1,
		covered/2,
		linear/1,
		nonground/1,
		clique/1,
		clique_1/1,

% Determinacy:
		is_det/1,
		non_det/1,
		possibly_nondet/1, % maybe_nondet?
		mut_exclusive/1,
		not_mut_exclusive/1,
		possibly_not_mut_exclusive/1,

% Non-failure: 
		not_fails/1,
		succeeds/1,
		fails/1,
		possibly_fails/1, % may_fail?
		covered/1, 
		not_covered/1,
		possibly_not_covered/1,
		test_type/2,

% More general cardinality, choicepoints, and exact solutions:
		num_solutions/2,
		relations/2,
		finite_solutions/1,
		solutions/2,
		no_choicepoints/1,
		leaves_choicepoints/1,

% Data sizes, cost, termination:
		size/2,
		size/3,
		size_lb/2,
		size_ub/2,
		size_o/2,

		size_metric/3,
		size_metric/4,
		measure_t/1,
		bound/1,

		steps/2,
		steps_lb/2,
		steps_o/2,
		steps_ub/2,

		terminates/1,

% Exceptions:
		exception/1,
		exception/2,
		possible_exceptions/2,
		no_exception/1,
		no_exception/2,

% Signals:
		signal/1,
		signal/2,
		possible_signals/2,
		no_signal/1,
		no_signal/2,

% Other side-effects:
		sideff_hard/1,
		sideff_pure/1,
		sideff_soft/1,

% Polyhedral constraints:
		constraint/1,

% Other properties:
		tau/1,
		% intervals/2 %[LD]

		user_output/2
		% , user_error/2

	    ],
	    [assertions, regtypes, hiord]).


:- doc(bug, "MH: The implementations for run-time checking, unless
   they are straightforward should be in the run-time check library
   and here they should just just be declared
   implementation_defibned. This file should be more about
   documentation, and providing definitions for the assertions when
   used by CiaoPP.").

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

:- reexport(library(terms_check), [instance/2]).
:- doc(doinclude, [instance/2]).

% TODO MH: check which are needed and make imports explicit
:- use_module(library(terms_vars)).
:- use_module(library(hiordlib)).
:- use_module(library(sort), [sort/2]).
:- use_module(library(lists)).
:- use_module(library(streams)).
:- use_module(library(file_utils)).
:- use_module(library(system)).
:- use_module(library(odd)).
:- use_module(library(rtchecks/rtchecks_send)).
:- use_module(engine(internals), ['$setarg'/4]).

% --------------------------------------------------------------------------
:- doc(title, "Properties which are native to analyzers").
% --------------------------------------------------------------------------

:- doc(author, "Francisco Bueno").
:- doc(author, "Manuel Hermenegildo").
:- doc(author, "Pedro L@'{o}pez").
:- doc(author, "Edison Mera").
:- doc(author, "Amadeo Casas").

:- doc(module, "@cindex{properties, native} This library contains
   a set of properties which are natively understood by the different
   program analyzers of @apl{ciaopp}.  They are used by @apl{ciaopp}
   on output and they can also be used as properties in assertions.").

%%    Note that the implementations provided for the properties are the ones used
%%    when run-time checks are enabled.  Run-time check for properties
%%    @var{Prop} must be implemented following certain rules:
%%    ** Comment: these rules are incomplete! See other documentation 
%%                for properties. 
%% 
%%    @begin{itemize}
%%    @item For any @var{Goal}, @pred{call(Goal)} must be equivalent to:
%% 
%%      intercept(Prop(Goal), rtcheck(_, _, _, _, _, _), true).
%% 
%%    @item Remove the choicepoints if the goal does not introduce new ones.
%% 
%%    @item Try to throw the run-time check exception as soon as the
%%    property being validated has been violated.
%% 
%%    @item All the checks must be compatible among them.
%%    @end{itemize}


:- doc(usage, "@tt{:- use_module(library(assertions/native_props))}

   or also as a package @tt{:- use_package(nativeprops)}.

   Note the slightly different names of the library and the package.").

% TODO: Improve documentation saying if the run-time checks of some
% TODO: properties are complete (exhaustive), incomplete, not possible
% TODO: or unimplemented --EMM.

% --------------------------------------------------------------------------
:- doc(section, "Meta-properties: instance and compat").
% --------------------------------------------------------------------------

:- doc(bug, "MH: Also defined (much better) in basic_props!!").

:- prop compat(Prop) + no_rtcheck

# "Use @var{Prop} as a compatibility property. Normally used with
   types. See the discussion in @ref{Declaring regular types}.".

:- meta_predicate compat(goal).
compat(_). % processed in rtchecks_basic

% --------------------------------------------------------------------------

:- doc(bug, "MH: The idea was to call it inst/2 (to avoid confusion
   with instace/2). Note that it is also defiend --this way-- in  
   basic_props!!"). 
:- doc(bug, "MH: inst/1 not really needed since it is the default? ").
:- doc(bug, "MH: Note that instance/2 is reexported above!").

:- prop instance(Prop) + no_rtcheck

# "Use Prop as an instantiation property. Normally used with
   types. See the discussion in @ref{Declaring regular types}.".

:- meta_predicate instance(goal).
instance(_). % processed in rtchecks_basic

% --------------------------------------------------------------------------
:- doc(section, "Sharing/aliasing, groundness").
% --------------------------------------------------------------------------

:- doc(mshare(X), "@var{X} contains all @index{sharing sets}
   @cite{jacobs88,abs-int-naclp89} which specify the possible variable
   occurrences in the terms to which the variables involved in the
   clause may be bound. Sharing sets are a compact way of representing
   groundness of variables and dependencies between variables. This
   representation is however generally difficult to read for
   humans. For this reason, this information is often translated to
   @prop{ground/1}, @prop{indep/1} and @prop{indep/2} properties,
   which are easier to read.").

% MH These look strange as sharing sets! Also, it is really wrong: it
% does not check the ground variables (the ones that do not appear in
% the sahring set)). To make it complete we need to pass all clause
% variables to mshare in another argument...
:- test mshare(L) : (L = [[A], [p(A)]]) + fails.
:- test mshare(L) : (L = [[A], [p(B)]]) + not_fails.

:- prop mshare(X) + (native(sharing(X)), no_rtcheck)
# "The sharing pattern for the variables in the clause is @tt{@var{X}}.".

% MH: This run-time check simply cannot be done like this! It is
% necessary to pass the variables (mshare/2) or read them from the
% clause database or whatever.
mshare(L) :-
	map(L, term_variables, V),
	\+ not_mshare(V).

% try to find a counter-example:
not_mshare([V1|L]) :-
	member(V2, L),
	member(X1, V1),
	member(X2, V2),
	X1 == X2 -> true
    ;
	not_mshare(L).

% --------------------------------------------------------------------------
% Amadeo
:- true prop indep(X, Y) + native(indep([[X, Y]]))
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
:- true prop indep(X) + native(indep(X))
# "The variables in the the pairs in @tt{@var{X}} are pairwise independent.".

indep([]).
indep([[X, Y]|L]) :- indep(X, Y), indep(L).

:- doc(linear(X), "@var{X} is bound to a term which is linear,
   i.e., if it contains any variables, such variables appear only once
   in the term. For example, @tt{[1,2,3]} and @tt{f(A,B)} are linear
   terms, while @tt{f(A,A)} is not.").

% --------------------------------------------------------------------------
:- doc(covered(X, Y), "All variables occuring in @var{X} occur also
   in @var{Y}. Used by the non-strict independence-based annotators.").

:- true prop covered(X, Y) + native # "@var{X} is covered by @var{Y}.".

covered(X, Y) :-
	varsbag(X, VarsX, []),
	varsbag(Y, VarsY, []),
	sublist(VarsX, VarsY).

% --------------------------------------------------------------------------
:- true prop linear(X) + native
# "@var{X} is instantiated to a linear term.".

linear(T) :-
	varsbag(T, VarsBag, []),
	sort(VarsBag, VarsSet),
	length(VarsBag, N),
	length(VarsSet, N).

% --------------------------------------------------------------------------
:- prop nonground(X) + native(not_ground(X))
# "@tt{@var{X}} is not ground.".

nonground(X) :- \+ ground(X).

% --------------------------------------------------------------------------
% Jorge 
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
:- doc(clique_1(X), "@var{X} is a set of variables of interest, much
   the same as a sharing group but @var{X} represents all the sharing
   groups in the powerset of those variables but disregarding the
   singletons. Similar to a sharing group, a clique_1 is often translated
   to @prop{ground/1}, @prop{indep/1}, and @prop{indep/2} properties.").

:- prop clique_1(X) + (native(clique_1(X)), no_rtcheck)
# "The 1-clique sharing pattern is @tt{@var{X}}.".

:- impl_defined(clique_1/1).

% --------------------------------------------------------------------------
:- doc(section, "Determinacy, failure, cardinality, choice-points").
% --------------------------------------------------------------------------

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

is_det(Goal) :-
	Solved = solved(no),
	Goal,
	(
	    arg(1, Solved, no)
	->
	    true
	;
	    send_comp_rtcheck(Goal, is_det, non_det)
	    % more than one solution!
	),
	'$setarg'(1, Solved, yes, true).

% --------------------------------------------------------------------------

:- doc(non_det(X), "All calls of the form @var{X} are
   non-deterministic, i.e., they always produce more than one
   solution.").

:- prop non_det(X)
# "All calls of the form @var{X} are non-deterministic.".

:- meta_predicate non_det(goal).

non_det(Goal) :-
	Solved = solved(no),
	(
	    true
	;
	    arg(1, Solved, one) ->
	    send_comp_rtcheck(Goal, non_det, is_det),
	    fail
	),
	'$metachoice'(C0),
	Goal,
	'$metachoice'(C1),
	(
	    arg(1, Solved, no) ->
	    (
		C1 == C0 ->
		!,
		send_comp_rtcheck(Goal, non_det, no_choicepoints)
	    ;
		'$setarg'(1, Solved, one, true)
	    )
	;
	    '$setarg'(1, Solved, yes, true)
	).

% --------------------------------------------------------------------------

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
:- doc(possibly_not_mut_exclusive(X), "Mutual exclusion of the clauses
   for calls of the form @var{X} cannot be ensured. This is the
   default when no information is given for a predicate, so this
   property does not need to be stated explicitly.").

:- prop possibly_not_mut_exclusive(X) + no_rtcheck
# "Mutual exclusion is not ensured for calls of the form @var{X}.".

:- meta_predicate possibly_not_mut_exclusive(goal).

possibly_not_mut_exclusive(Goal) :- call(Goal).

% --------------------------------------------------------------------------
:- doc(section, "Failure and success").
% --------------------------------------------------------------------------

% not_fails = succeeds	or not_terminates. -- EMM

:- doc(not_fails(X), "Calls of the form @var{X} produce at least one
   solution (succeed), or do not terminate. This property is inferred
   and checked natively by CiaoPP using the domains and techniques of
   @cite{non-failure-iclp97,nfplai-flops04-short}.").

:- true prop not_fails(X) + native
# "All the calls of the form @var{X} do not fail.".
% %
% :- meta_predicate not_fails( goal ).
%
% not_fails( X ) :-
% 	if( X , true , throw( rtcheck( nf , fail , X  ) ) ).

:- meta_predicate not_fails(goal).

not_fails(Goal) :-
	Solved = solved(no),
	(
	    true
	;
	    arg(1, Solved, no) ->
	    send_comp_rtcheck(Goal, not_fails, fails),
	    fail
	),
	'$metachoice'(C0),
	no_exception_2(Goal, not_fails, _),
	'$metachoice'(C1),
	( C0 == C1 -> !
	; '$setarg'(1, Solved, yes, true) ).

% --------------------------------------------------------------------------

:- prop succeeds(Prop) + no_rtcheck # "A call to @var{Prop} succeeds.".

:- meta_predicate succeeds(goal).

succeeds(Prop) :- \+ \+ call(Prop). % Defined but processed in rtchecks_basic

% --------------------------------------------------------------------------

:- doc(fails(X), "Calls of the form @var{X} fail.").

:- true prop fails(X) + native
# "Calls of the form @var{X} fail.".

:- meta_predicate fails(goal).

fails(Goal) :-
	Solved = solved(no),
	no_exception_2(Goal, fails, _),
	(
	    arg(1, Solved, no) ->
	    send_comp_rtcheck(Goal, fails, not_fails),
	    '$setarg'(1, Solved, yes, true)
	;
	    true
	).


% --------------------------------------------------------------------------

:- doc(possibly_fails(X), "Non-failure is not ensured for any call of
   the form @var{X}. In other words, nothing can be ensured about
   non-failure nor termination of such calls.").

:- prop possibly_fails(X) + no_rtcheck
# "Non-failure is not ensured for calls of the form @var{X}.".

:- meta_predicate possibly_fails(goal).

possibly_fails(Goal) :- call(Goal).

% --------------------------------------------------------------------------

:- doc(covered(X), "For any call of the form @var{X} there is at least
   one clause whose test (guard) succeeds (i.e., all the calls of the
   form @var{X} are covered).  Note that nonfailure is the transitive
   closure (to all called predicates) of this
   property. @cite{non-failure-iclp97,nfplai-flops04-short}.").

:- prop covered(X) + rtcheck(unimplemented)
# "All the calls of the form @var{X} are covered.".

covered(Goal) :- call(Goal).

% --------------------------------------------------------------------------

:- doc(not_covered(X), "There is some call of the form @var{X} for
   which there is no clause whose test succeeds
   @cite{non-failure-iclp97}.").

:- prop not_covered(X) + rtcheck(unimplemented)
# "Not all of the calls of the form @var{X} are covered.".

not_covered(Goal) :- call(Goal).

% --------------------------------------------------------------------------

:- doc(possibly_not_covered(X), "Covering is not ensured for any call of
   the form @var{X}. In other words, nothing can be ensured about
   covering of such calls.").

:- prop possibly_not_covered(X) + no_rtcheck
# "Covering is not ensured for calls of the form @var{X}.".

:- meta_predicate possibly_not_covered(goal).

possibly_not_covered(Goal) :- call(Goal).

% --------------------------------------------------------------------------

:- prop test_type(X, T) # "Indicates the type of test that a predicate
	performs.  Required by the nonfailure analyisis.".

:- meta_predicate test_type(goal, ?).

test_type(Goal, _) :- call(Goal).

% --------------------------------------------------------------------------
:- doc(section, "More general cardinality, choice points, and exact solutions").
% --------------------------------------------------------------------------

:- prop num_solutions(X, N) : callable * int # "Calls of the form
   @var{X} have @var{N} solutions, i.e., @var{N} is the cardinality of
   the solution set of @var{X}.".

:- prop num_solutions(Goal, Check) : callable * callable
# "For a call to @var{Goal}, @pred{Check(X)} succeeds, where @var{X} is
   the number of solutions.".

:- meta_predicate num_solutions(goal, addterm(pred(1))).

num_solutions(Goal, _, N) :-
	int(N),
	!,
	num_solutions_eq(Goal, N).
num_solutions(Goal, Check, Term) :-
	Sols = num_solutions(0),
	(
	    true
	;
	    arg(1, Sols, N0),
	    (
		Check(N0) -> fail
	    ;
		send_comp_rtcheck(Goal, num_solutions(Term),
		    num_solutions(N0)),
		fail
	    )
	),
	'$metachoice'(C0),
	call(Goal),
	'$metachoice'(C1),
	arg(1, Sols, N0),
	N1 is N0 + 1,
	(
	    C1 == C0 ->
	    !,
	    (
		Check(N1) -> true
	    ;
		send_comp_rtcheck(Goal, num_solutions(Term), num_solutions(N0))
	    )
	;
	    '$setarg'(1, Sols, N1, true)
	).


:- meta_predicate num_solutions_eq(goal, ?).
num_solutions_eq(Goal, N) :-
	Sols = solutions(0),
	(
	    true
	;
	    arg(1, Sols, A),
	    (
		(A == done ; A == N) -> fail
	    ;
		send_comp_rtcheck(Goal, num_solutions(N), Sols),
		fail
	    )
	),
	'$metachoice'(C0),
	call(Goal),
	'$metachoice'(C1),
	arg(1, Sols, A),
	(
	    A == done -> true
	;
	    N1 is A + 1,
	    (
		C1 == C0 ->
		!,
		(
		    N1 == N -> true
		;
		    send_comp_rtcheck(Goal, num_solutions(N),
			num_solutions(N1))
		)
	    ;
		(
		    N1 > N ->
		    send_comp_rtcheck(Goal, num_solutions(N),
			num_solutions('>'(N))),
		    '$setarg'(1, Sols, done, true)
		;
		    '$setarg'(1, Sols, N1, true)
		)
	    )
	).

% --------------------------------------------------------------------------

:- doc(bug, "This is the same as num_solutions/2!").

:- doc(relations(X, N), "Calls of the form produce @var{N} solutions,
   i.e., @var{N} is the cardinality of the solution set of @var{X}.").

:- prop relations(X, N) : callable * int + rtcheck(unimplemented)
# "Goal @var{X} produces @var{N} solutions.".

:- meta_predicate relations(goal, ?).
relations(Goal, _) :- call(Goal).

% --------------------------------------------------------------------------

:- doc(finite_solutions(X), "Calls of the form @var{X} produce a
   finite number of solutions @cite{non-failure-iclp97}.").

:- prop finite_solutions(X) + no_rtcheck
# "All the calls of the form @var{X} have a finite number of
   solutions.".

:- meta_predicate finite_solutions(goal).
finite_solutions(Goal) :- call(Goal).

% --------------------------------------------------------------------------

:- prop solutions(Goal, Sols) : callable * list
# "Goal @var{Goal} produces the solutions listed in @var{Sols}.".

:- meta_predicate solutions(addterm(goal), ?).

solutions(Goal, Sol, Sols) :-
	Remaining = solutions(Sols),
	(
	    true
	;
	    arg(1, Remaining, Sols0),
	    (
		(Sols == done ; Sols0 == []) -> fail
	    ;
		append(Sols2, Sols0, Sols),
		send_comp_rtcheck(Goal, solutions(Sols), solutions(Sols2)),
		fail
	    )
	),
	'$metachoice'(C0),
	call(Goal),
	'$metachoice'(C1),
	arg(1, Remaining, Sols0),
	(
	    Sols0 == done -> true
	;
	    [Elem|Sols1] = Sols0,
	    (
		C1 == C0 ->
		!,
		(
		    Elem \= Sol ->
		    append(Curr, Sols0, Sols),
		    append(Curr, [Sol], Sols2),
		    send_comp_rtcheck(Goal, solutions(Sols), solutions(Sols2))
		;
		    true
		)
	    ;
		(
		    Elem \= Sol ->
		    append(Curr, Sols0,   Sols),
		    append(Curr, [Sol|_], Sols2),
		    send_comp_rtcheck(Goal, solutions(Sols), solutions(Sols2)),
		    '$setarg'(1, Remaining, done, true)
		;
		    '$setarg'(1, Remaining, Sols1, true)
		)
	    )
	).

% --------------------------------------------------------------------------

:- prop no_choicepoints(X)
# "A call to @var{X} does not leave new choicepoints.".

:- meta_predicate no_choicepoints(goal).

no_choicepoints(Goal) :-
	'$metachoice'(C0),
	Goal,
	'$metachoice'(C1),
	( C1 == C0 -> true
	; send_comp_rtcheck(Goal, no_choicepoints, leaves_choicepoints)
	).

% --------------------------------------------------------------------------

:- prop leaves_choicepoints(X)
# "A call to @var{X} leaves new choicepoints.".

:- meta_predicate leaves_choicepoints(goal).
leaves_choicepoints(Goal) :-
	'$metachoice'(C0),
	Goal,
	'$metachoice'(C1),
	( C1 == C0 ->
	    send_comp_rtcheck(Goal, leaves_choicepoints, no_choicepoints)
	; true ).

% --------------------------------------------------------------------------
:- doc(section, "Data sizes, cost, termination").
% --------------------------------------------------------------------------

:- prop size(X, Y) + no_rtcheck
# "@var{Y} is the size of argument @var{X}, for any approximation.".

size(_, _).
% :- impl_defined(size/2).


% --------------------------------------------------------------------------

:- doc(bug,"size/3 vs. size_ub, size_lb redundant...").

:- prop size(A, X, Y) : bound(A) + no_rtcheck
# "@var{Y} is the size of argument @var{X}, for the approximation @var{A}.".

size(_, _, _).
% :- impl_defined(size/3).

% --------------------------------------------------------------------------

:- doc(size_lb(X, Y), "The minimum size of the terms to which the
   argument @var{Y} is bound is given by the expression
   @var{Y}. Various measures can be used to determine the size of an
   argument, e.g., list-length, term-size, term-depth, integer-value,
   etc. @cite{caslog,granularity-jsc}. See @pred{measure_t/1}.").

:- prop size_lb(X, Y) + no_rtcheck
# "@var{Y} is a lower bound on the size of argument @var{X}.".

:- impl_defined(size_lb/2).

% --------------------------------------------------------------------------

:- doc(size_ub(X, Y), "The maximum size of the terms to which the
   argument @var{Y} is bound is given by the expression
   @var{Y}. Various measures can be used to determine the size of an
   argument, e.g., list-length, term-size, term-depth, integer-value,
   etc. @cite{caslog,granularity-jsc}. See @pred{measure_t/1}.").

:- prop size_ub(X, Y) + no_rtcheck
# "@var{Y} is a upper bound on the size of argument @var{X}.".

:- impl_defined(size_ub/2).

% --------------------------------------------------------------------------

:- prop size_o(X, Y) + no_rtcheck
# "The size of argument @var{X} is in the order of the expression @var{Y}.".

:- impl_defined(size_o/2).

% --------------------------------------------------------------------------

:- prop size_metric(Head, Var, Metric)
	:: measure_t(Metric) + no_rtcheck

# "@var{Metric} is the measure used to determine the size of the terms
   that @var{Var} is bound to, for any type of approximation.".

:- meta_predicate size_metric(goal, ?, ?).
size_metric(Goal, _, _) :- call(Goal).

% --------------------------------------------------------------------------

:- prop size_metric(Head, Approx, Var, Metric)
	:: (bound(Approx), measure_t(Metric)) + no_rtcheck

# "@var{Metric} is the measure used to determine the size of the terms
   that variable @var{Var} bound to, for the approximation
   @var{Approx}.". % depth/1 See resources_basic

:- meta_predicate size_metric(goal, ?, ?, ?).
size_metric(Goal, _, _, _) :- call(Goal).

% --------------------------------------------------------------------------

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

:- doc(steps(X, Y), "The computation (in resolution steps) spent by
   any call of the form @var{X} is given by the expression @var{Y}").

:- prop steps(X, Y) + no_rtcheck

# "@var{Y} is the cost (number of resolution steps) of any call of the
   form @var{X}.".

:- meta_predicate steps(goal, ?).
steps(Goal, _) :- call(Goal).

% --------------------------------------------------------------------------

:- prop steps_o(X, Y) + no_rtcheck

# "@var{Y} is the complexity order of the cost of any call of the form
   @var{X}.".

:- meta_predicate steps_o(goal, ?).
steps_o(Goal, _) :- call(Goal).

% --------------------------------------------------------------------------

:- doc(terminates(X), "Calls of the form @var{X} always terminate.").

:- prop terminates(X) + no_rtcheck
# "All calls of the form @var{X} terminate.".

:- meta_predicate terminates(goal).
terminates(Goal) :- call(Goal).

% --------------------------------------------------------------------------
:- doc(section, "Exceptions").
% --------------------------------------------------------------------------

:- prop exception(Goal, E) # "Calls to @var{Goal} will throw an
	exception that unifies with @var{E}.".

:- meta_predicate exception(goal, ?).

exception(Goal, E) :-
	catch(Goal, F,
	    (
		( E \= F ->
		    send_comp_rtcheck(Goal, exception(E), exception(F))
		;
		    true
		),
		throw(F)
	    )),
	send_comp_rtcheck(Goal, exception(E), no_exception).

% --------------------------------------------------------------------------

:- prop exception(Goal)
# "Calls of the form @var{Goal} will throw an (unspecified) exception.".

:- meta_predicate exception(goal).

exception(Goal) :-
	Goal,
	send_comp_rtcheck(Goal, exception, no_exception).

% --------------------------------------------------------------------------

:- prop possible_exceptions(Goal, Es) : list(Es) + rtcheck(unimplemented)
# "Calls of the form @var{Goal} may throw exceptions, but only
   the ones that unify with the terms listed in @var{Es}.".

:- meta_predicate possible_exceptions(goal, ?).

possible_exceptions(Goal, _E) :- call(Goal).

% --------------------------------------------------------------------------

:- prop no_exception(Goal)
# "Calls of the form @var{Goal} do not throw any exception.".

:- meta_predicate no_exception(goal).

no_exception(Goal) :- no_exception_2(Goal, no_exception, _).

% --------------------------------------------------------------------------

:- prop no_exception(Goal, E)
# "Calls of the form @var{Goal} do not throw any exception that
  unifies with @var{E}.".

:- meta_predicate no_exception(goal, ?).

no_exception(Goal, E) :- no_exception_2(Goal, no_exception(E), E).

:- meta_predicate no_exception_2(goal, ?, ?).
no_exception_2(Goal, Prop, E) :-
	catch(Goal, E,
	    (
		send_comp_rtcheck(Goal, Prop, exception(E)),
		throw(E)
	    )).

% --------------------------------------------------------------------------
:- doc(section, "Signals").
% --------------------------------------------------------------------------

:- prop signal(Goal)
# "Calls to @var{Goal} will send an (unspecified) signal.".

:- meta_predicate signal(goal).

signal(Goal) :- signal(Goal, _).

% --------------------------------------------------------------------------

:- prop signal(Goal, E)
# "Calls to @var{Goal} will send a signal that unifies with @var{E}.".

:- meta_predicate signal(goal, ?).

signal(Goal, E) :-
	'$metachoice'(Choice),
	asserta_signal_check(Choice, Goal, E, yes),
	'$metachoice'(C0),
	intercept(Goal, E, (emit_signal(Choice, E), send_signal(E))),
	'$metachoice'(C1),
	retract_signal_check(Choice, Goal, E, yes),
	(C0 == C1 -> ! ; true).

% --------------------------------------------------------------------------

:- prop possible_signals(Goal, Es) + rtcheck(unimplemented)
# "Calls of the form @var{Goal} may generate signals, but only the
   ones that unify with the terms listed in @var{Es}.".

:- meta_predicate possible_signals(goal, ?).

possible_signals(Goal, _E) :- call(Goal).

% --------------------------------------------------------------------------

:- prop no_signal(Goal)
# "Calls of the form @var{Goal} do not send any signal.".

:- meta_predicate no_signal(goal).

no_signal(Goal) :- no_signal(Goal, _).

% --------------------------------------------------------------------------

:- prop no_signal(Goal, E)
# "Calls of the form @var{Goal} do not send any signals that unify
  with @var{E}.".

:- meta_predicate no_signal(goal, ?).

no_signal(Goal, E) :-
	'$metachoice'(Choice),
	asserta_signal_check(Choice, Goal, E, no),
	'$metachoice'(C0),
	intercept(Goal, E, (emit_signal(Choice, E), send_signal(E))),
	'$metachoice'(C1),
	retract_signal_check(Choice, Goal, E, no),
	(C0 == C1 -> ! ; true).

:- data signal_db/3.

asserta_signal_check(Choice, _, E, _) :-
	asserta_fact(signal_db(Choice, no, E)).
asserta_signal_check(Choice, Goal, _, CheckThrown) :-
	end_signal_check(Choice, Goal, CheckThrown), fail.

retract_signal_check(Choice, Goal, _, CheckThrown) :-
	end_signal_check(Choice, Goal, CheckThrown).
retract_signal_check(Choice, _, E, _) :-
	asserta_fact(signal_db(Choice, no, E)),
	fail.

signal_prop(yes, E, signal(yes, E), signal(no,  E)).
signal_prop(no,  E, signal(no,  E), signal(yes, E)).

end_signal_check(Choice, Goal, CheckThrown) :-
	retract_fact_nb(signal_db(Choice, Thrown, E)),
	signal_prop(CheckThrown, E, EP, EV),
	( Thrown = CheckThrown -> true
	; send_comp_rtcheck(Goal, EP, EV)
	).

emit_signal(Choice, E) :-
	retract_fact_nb(signal_db(Choice, _, _)),
	assertz_fact(signal_db(Choice, yes, E)).

% --------------------------------------------------------------------------
:- doc(section, "Other side effects").
% --------------------------------------------------------------------------

:- doc(bug,"Still missing other side effects such as dynamic
   predicates, mutables, I/O, etc.").

:- doc(bug,"These need to be unified with the sideff(pure) ones!").

:- prop sideff_pure(X) + no_rtcheck
# "@var{X} is pure, i.e., has no side-effects.".

:- meta_predicate sideff_pure(goal).
sideff_pure(Goal) :- call(Goal).

% --------------------------------------------------------------------------

:- prop sideff_soft(X) + no_rtcheck
# "@var{X} has @index{soft side-effects}, i.e., those not affecting
   program execution (e.g., input/output).".

:- meta_predicate sideff_soft(goal).
sideff_soft(Goal) :- call(Goal).

% --------------------------------------------------------------------------

:- prop sideff_hard(X) + no_rtcheck
# "@var{X} has @index{hard side-effects}, i.e., those that might affect
   program execution (e.g., assert/retract).".

:- meta_predicate sideff_hard(goal).
sideff_hard(Goal) :- call(Goal).

% --------------------------------------------------------------------------
:- doc(section, "Polyhedral constraints").
% --------------------------------------------------------------------------

:- doc(constraint(C), "@var{C} contains a list of linear
   (in)equalities that relate variables and @tt{int} values. For
   example, @tt{[A < B + 4]} is a constraint while @tt{[A < BC + 4]}
   or @tt{[A = 3.4, B >= C]} are not. Used by polyhedra-based
   analyses.").

:- true prop constraint(C) + native
# "@var{C} is a list of linear equations.".

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
% 	lin_expr(Lin_Expr, Vars, New_Lin_Expr).
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
	int(Coeff).

% --------------------------------------------------------------------------
:- doc(section, "Other properties").
% --------------------------------------------------------------------------

:- doc(bug, "Needs reorganization...").

:- doc(tau(Types), "@var{Types} contains a list with the type associations
   for each variable, in the form @tt{V/[T1,..,TN]}. Note that tau is used
   in object-oriented programs only").

:- true prop tau(TypeInfo) + native
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

:- prop user_output(Goal, S) #
	"Calls of the form @var{Goal} write @var{S} to standard output.".

:- meta_predicate user_output(goal, ?).
user_output(Goal, S) :-
	'$metachoice'(Choice),
	mktemp_in_tmp('tmpciaoXXXXXX', FileName),
	asserta_user_output_check(Choice, FileName, Goal, S),
	'$metachoice'(C0),
	catch(Goal, E,
	    (end_output_check(Choice, FileName, Goal, S), throw(E))),
	'$metachoice'(C1),
	retract_user_output_check(Choice, FileName, Goal, S),
	(C0 == C1 -> ! ; true).

:- data output_db/2.

asserta_user_output_check(Choice, FileName, _, _) :-
	ini_output_check(Choice, FileName).
asserta_user_output_check(Choice, FileName, Goal, S) :-
	end_output_check(Choice, FileName, Goal, S), fail.

retract_user_output_check(Choice, FileName, Goal, S) :-
	end_output_check(Choice, FileName, Goal, S).
retract_user_output_check(Choice, FileName, _, _) :-
	ini_output_check(Choice, FileName), fail.

ini_output_check(Choice, FileName) :-
	open_output(FileName, SO),
	assertz_fact(output_db(Choice, SO)),
	!.

end_output_check(Choice, FileName, Goal, S) :-
	retract_fact(output_db(Choice, SO)),
	close_output(SO),
	file_to_string(FileName, S1),
	delete_file(FileName),
	display_string(S1),
	(
	    S \== S1 ->
	    send_comp_rtcheck(Goal, user_output(S), user_output(S1))
	;
	    true
	),
	!.

%%%%%%%%%%%%%%
%%%% This one is in the testing library (unittest)
%% :- prop user_error(Goal, S) #
%% 	"Calls of the form @var{Goal} write @var{S} to standard error.".
%% 
%% :- meta_predicate user_error(goal, ?).
%% user_error(Goal, S) :-
%% 	mktemp_in_tmp('tmpciaoXXXXXX', FileName),
%% 	open_error(FileName, SO),
%% 	call(Goal),
%% 	close_error(SO),
%% 	file_to_string(FileName, S1),
%% 	display_string(S1),
%% 	(
%% 	    S \== S1 ->
%% 	    send_comp_rtcheck(Goal, user_error(S), user_error(S1))
%% 	;
%% 	    true
%% 	).
%% 
%%%%%%%%%%%%%%


:- doc(bug, "MH: not really clear why this should be here.").

% Built-in in CiaoPP
:- export(entry_point_name/2).
:- doc(hide, entry_point_name/2).

:- prop entry_point_name/2 + no_rtcheck.
% if you change this declaration, you have to change ciaoPP:

:- meta_predicate entry_point_name(goal, ?).
:- impl_defined(entry_point_name/2).


% ------------------------------------------------------------------
% ------------------------------------------------------------------
% ------------------------------------------------------------------
% ------------------------------------------------------------------
% ------------------------------------------------------------------
% ------------------------------------------------------------------
% ------------------------------------------------------------------


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% % Collapsed properties, to improve performance of run-time checks. --EMM
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% % These properties are not well implemented:
%% :- prop not_fails_is_det/1 
%% # "Collapsed property of @var{not_fails/1} and @var{is_det/1}.".
%% 
%% :- meta_predicate not_fails_is_det(goal).
%% not_fails_is_det(Goal) :-
%% 	Solved = solved(no),
%% 	(
%% 	    true
%% 	;
%% 	    arg(1, Solved, no) ->
%% 	    send_comp_rtcheck(Goal, not_fails, fails),
%% 	    fail
%% 	),
%% 	Goal,
%% 	(
%% 	    arg(1, Solved, no)
%% 	->
%% 	    true
%% 	;
%% 	    send_comp_rtcheck(Goal, is_det, non_det))
%% % more than one solution!
%% 	),
%% 	'$setarg'(1, Solved, yes, true).
%% 
%% :- prop not_fails_non_det/1 
%% # "Collapsed property of @var{not_fails/1} and @var{non_det/1}.".
%% 
%% :- meta_predicate not_fails_non_det(goal).
%% not_fails_non_det(Goal) :-
%% 	Solved = solved(no),
%% 	(
%% 	    true
%% 	;
%% 	    arg(1, Solved, no) ->
%% 	    send_comp_rtcheck(Goal, not_fails, fails),
%% 	    fail
%% 	;
%% 	    arg(1, Solved, one) ->
%% 	    send_comp_rtcheck(Goal, non_det, is_det),
%% 	    fail
%% 	),
%% 	'$metachoice'(C0),
%% 	Goal,
%% 	'$metachoice'(C1),
%% 	(
%% 	    arg(1, Solved, no) ->
%% 	    (
%% 		C1 == C0 ->
%% 		!,
%% 		send_comp_rtcheck(Goal, non_det, no_choicepoints))
%% 	    ;
%% 		'$setarg'(1, Solved, one, true)
%% 	    )
%% 	;
%% 	    '$setarg'(1, Solved, yes, true)
%% 	).

