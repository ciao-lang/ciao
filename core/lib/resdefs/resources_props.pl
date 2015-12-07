:- module(_, [
		cost/7,
		cost/8,
		head_cost/4,
		literal_cost/4,
		intervals/3,
		calls/1,
		cost_args/2
	    ],
	    [assertions, regtypes, hiord]).

:- use_package(library(resdefs/resources_decl)).
:- reexport(library(resdefs/resources_types)).
:- use_module(library(resdefs/resdefs_rt)).
:- use_module(engine(internals)).
:- use_module(library(rtchecks/rtchecks_send)).


%----------------------------------------------------------------------------
% Added by JNL 
%----------------------------------------------------------------------------

:- regtype cost_mode_t/1.

cost_mode_t('+').
cost_mode_t('-').
cost_mode_t('?').
cost_mode_t(void).

:- meta_predicate cost_args(goal, ?).

:- prop cost_args(Head, Args) : callable * list(cost_mode_t) + no_rtcheck
# "Specifies the modes for the given predicate, as required by
   the resources analysis".

cost_args(Goal, _) :- call(Goal).

:- meta_predicate head_cost(goal, ?, ?, pred(2)).

:- prop head_cost(Head, Approx, Res, Cost_Pred) + no_rtcheck
# "The partial resource spent @var{Res} by any call of the form
   @var{Head} is given by the execution of the predicate
   @var{Cost_Pred}.  @var{Approx} gives the type approximation:
   @tt{ub} (upper bound), @tt{lb} (lower bound), or @var{o}
   (complexity order).".

head_cost(Goal, _, _, _) :- call(Goal).


:- meta_predicate literal_cost(goal, ?, ?, pred(2)).

:- prop literal_cost(Head, Approx, Res, Cost_Pred) + no_rtcheck
# "The partial resource spent @var{Res} by any instantiation of the
   head when @var{Head} is invoked is given by the execution of the
   predicate @var{Cost_Pred}. @var{Approx} gives the type
   approximation: @tt{ub} (upper bound), @tt{lb} (lower bound), or
   @var{o} (complexity order).".

literal_cost(Goal, _, _, _) :- call(Goal).

%----------------------------------------------------------------------------
% Added by EMM and MTT
%----------------------------------------------------------------------------

:- pred can_validate_res(Res, Rel) # "Resource @var{Res}, relative
	or absolute, (specified by @var{Rel}), can be validated
	(even if it is defined).".

can_validate_res(Res, Rel) :-
	'$def$ru'(Res), (Rel == rel -> '$def$gru'(Res) ; true).

:- prop cost(Head, Rel, Approx, Type, Res, IF, CostExpr)
	: callable * rel_t * approx * cost_type
	* resource * list(callable) * cost_expression + rtcheck(incomplete)

# "The total resource spent of resource @var{Res} when @var{Head} is
   called is given by cost function @var{CostExpr}. @var{Approx} gives
   the type approximation.  @var{Type} specifies the kind of cost
   information we are interested in.

   In additon to cost/7, we have other properties, cost/4, cost/5,
   rel_cost/4 and rel_cost/5, that are expanded by the @tt{resdefs}
   package.".

:- meta_predicate cost(goal, ?, ?, ?, ?, addterm(list(goal)), ?).
cost(Goal, Rel, Ap, Type, Res, IF, _, CF) :-
	can_validate_res(Res, Rel),
	!,
	(
	    resource_usage(Res, T0) ->
	    cost_t(Type, Goal, Rel, Ap, Res, IF, CF, T0)
	;
	    call(Goal)
	).
cost(Goal, _, _, _, _, _, _, _) :- call(Goal).

cost_prop(abs, Ap, Type, Res, T, Pr) :- abs_cost_prop(Ap, Type, Res, T, Pr).
cost_prop(rel, Ap, Type, Res, T, Pr) :- rel_cost_prop(Ap, Type, Res, T, Pr).

abs_cost_prop(Ap, call, Res, T, cost(Ap, Res, T)) :- !.
abs_cost_prop(Ap, Type, Res, T, cost(Ap, Type, Res, T)).

rel_cost_prop(Ap, call, Res, T, rel_cost(Ap, Res, T)) :- !.
rel_cost_prop(Ap, Type, Res, T, rel_cost(Ap, Type, Res, T)).

relativize_res(abs, _,   R,  R).
relativize_res(rel, Res, R0, R) :-
	global_resource_usage(Res, GR),
	% R = R0 / GR.
	R is R0 / GR.

:- meta_predicate calls(list(goal)).
calls([]).
calls([H|T]) :- call(H), calls(T).

:- meta_predicate check_compare(goal, ?, ?, ?, ?, ?, list(goal), ?).

check_compare(Goal, Rel, Ap, Type, Res, ER0, IF, CostFunction) :-
	( calls(IF), eval_cf(CostFunction, R) ->
	    relativize_res(Rel, Res, ER0, ER),
	    (
		compare_ap(Ap, ER, R) ->
		cost_prop(Rel, Ap, Type, Res, R,  Prop),
		cost_prop(Rel, eq, Type, Res, ER, VProp),
		send_comp_rtcheck(Goal, Prop, VProp)
	    ;
		true
	    )
	;
	    cost_prop(Rel, Ap, Type, Res, R, Prop),
	    send_comp_rtcheck(Goal, Prop, fails(CostFunction))
	).

% Note that T can be 0.Nan
compare_ap(ub, ET, T) :- ET > T.
compare_ap(lb, ET, T) :- ET < T.

:- meta_predicate cost_t(?, goal, ?, ?, ?, list(goal), ?, ?).
cost_t(call_exit, Goal, Rel, Ap, Res, IF, CostFunction, T0) :-
	!,
	Solved = solved(no),
	'$metachoice'(C0),
	call(Goal),
	'$metachoice'(C1),
	(
	    arg(1, Solved, no)
	->
	    resource_usage(Res, T1),
	    T is T1 - T0,
	    check_compare(Goal, Rel, Ap, call_exit, Res, T, IF, CostFunction)
	;
	    true
	),
	(C1 == C0 -> ! ; true),
	'$setarg'(1, Solved, yes, true).
cost_t(call_fail, Goal, Rel, Ap, Res, IF, CostFunction, T0) :-
	!,
	Solved = solved(no),
	(
	    true
	;
	    arg(1, Solved, no) ->
	    resource_usage(Res, T1),
	    T is T1 - T0,
	    check_compare(Goal, Rel, Ap, call_fail, Res, T, IF, CostFunction),
	    fail
	),
	'$metachoice'(C0),
	Goal,
	'$metachoice'(C1),
	(C1 == C0 -> ! ; true),
	'$setarg'(1, Solved, yes, true).
cost_t(call, Goal, Rel, Ap, Res, IF, CostFunction, T0) :-
	!,
	Solved = solved(no),
	(
	    true
	;
	    arg(1, Solved, no) ->
	    resource_usage(Res, T1),
	    T is T1 - T0,
	    check_compare(Goal, Rel, Ap, call, Res, T, IF, CostFunction),
	    fail
	),
	'$metachoice'(C0),
	Goal,
	'$metachoice'(C1),
	(
	    arg(1, Solved, no)
	->
	    resource_usage(Res, T1),
	    T is T1 - T0,
	    check_compare(Goal, Rel, Ap, call, Res, T, IF, CostFunction)
	;
	    true
	),
	(C1 == C0 -> ! ; true),
	'$setarg'(1, Solved, yes, true).
cost_t(redo, Goal, Rel, Ap, Res, IF, CostFunction, T0) :-
	!,
	Solved = solved(no),
	(
	    true
	;
	    arg(1, Solved, true) ->
	    resource_usage(Res, T1),
	    T is T1 - T0,
	    check_compare(Goal, Rel, Ap, redo, Res, T, IF, CostFunction),
	    fail
	),
	'$metachoice'(C0),
	Goal,
	'$metachoice'(C1),
	(
	    arg(1, Solved, true)
	->
	    resource_usage(Res, T1),
	    T is T1 - T0,
	    check_compare(Goal, Rel, Ap, redo, Res, T, IF, CostFunction)
	;
	    true
	),
	(C1 == C0 -> ! ; true),
	'$setarg'(1, Solved, yes, true).

%%% Expanded by resdefs package, but not implemented here: -- EMM
:- push_prolog_flag(unused_pred_warnings, yes).

:- export(cost/4).
:- export(cost/5).
:- export(rel_cost/5).
:- export(rel_cost/4).
:- export(intervals/2).

:- true prop cost(Head, Approx, Type, Res, CostExpr)
	: callable * approx * cost_type * resource * cost_expression
# "Same as @pred{cost/7}, but when the cost is absolute (@var{Rel} is
   abs)".
:- impl_defined(cost/5). % Transformed by resdefs_tr to cost/7.

:- true prop cost(Head, Approx, Res, CF)
	: callable * approx * term * term
# "Like @pred{cost/5}, but with @var{Type}=@tt{call}.".
:- impl_defined(cost/4). % Transformed by resdefs_tr to cost/7.

:- true prop rel_cost(Head, Approx, Type, Res, CostExpr)
	: callable * approx * cost_type * term * callable

# "The amount spent of resource @var{Res} relative to the global
   consumption of this resource when @var{Head} is called is given by
   cost expression @var{CostFunction} (in this case,
   @var{CostFunction}=<1). @var{Approx} gives the type of
   approximation.  @var{Type} specifies the kind of cost information
   we are interested in.".

:- impl_defined(rel_cost/5). % Transformed by resdefs_tr to cost/7.

:- prop rel_cost(Head, Approx, Res, CostExpr)
	: callable * approx * term * callable
# "Same as @var{rel_cost/5}, but with Type=call.".
:- impl_defined(rel_cost/4). % Transformed by resdefs_tr to cost/7.

%[LD]
:- prop intervals(X, Y)
# "Data size @var{X} belongs to some interval in the list of intervals
@var{Y}. The list of intervals @var{Y} represents union of its elements".

:- impl_defined(intervals/2). % Transformed by resdefs_tr to intervals/3.
%[\LD]

:- pop_prolog_flag(unused_pred_warnings).

:- prop intervals(Goal, Value, Intervals) # "@var{Goal} is
	executed to obtain the data size @var{Size} in @var{Value},
	and the predicate succeeds iif the Value is in the interval
	@var{Intervals}.".

:- meta_predicate intervals(addterm(goal), ?, ?).

intervals(G, _, X, L) :- call(G), in_intervals(X, L), !.

in_interval(X, i(A, B)) :-
	(A == neginf -> true ; A == inf -> fail ; A =< X),
	(B == neginf -> fail ; B == inf -> true ; X =< B).

in_intervals(X, [I|_]) :- in_interval(X, I).
in_intervals(X, [_|L]) :- in_intervals(X, L).
