:- module(_, _, []).

:- use_module(library(assertions/assrt_lib),
	    [assertion_read/9, assertion_body/7]).
:- use_module(library(aggregates)).
:- use_module(library(rtchecks/rtchecks_basic)).
:- use_module(library(rtchecks/rtchecks_tr), [body_expansion/3]).

resprof_sentence_tr(0,           0,           _) :- !.
resprof_sentence_tr(end_of_file, end_of_file, _) :- !.
resprof_sentence_tr((:- resource(Resource)), [
		(:- resource(Resource)),
		( init_resource_usage :-
		    init_resource_usage(Resource, ub) )], _) :- !.
resprof_sentence_tr((:- load_resource_module(Module)),
	    [(:- use_module(Module))],            _) :- !.
resprof_sentence_tr((:- head_cost(Approx, Resource, Func)),
	    head_cost(Approx, Resource, Func),    _) :- !.
resprof_sentence_tr((:- literal_cost(Approx, Resource, Func)),
	    literal_cost(Approx, Resource, Func), _) :- !.
resprof_sentence_tr((:- Decl), (:- Decl), _) :- !.
resprof_sentence_tr((Head :- Body0),
	    (Head :- inc_cost(head_cost, Head), Body), Module) :- !,
	body_expansion(Body0, add_prof_literal(Module), Body).
resprof_sentence_tr(Head, (Head :- inc_cost(head_cost, Head)), _) :- !.

add_prof_literal(Literal0, Module, ( inc_cost(literal_cost, Literal0),
		Literal )) :-
	functor(Literal0, F, N),
	functor(Literal1, F, N),
	findall(check_cost(CheckCall, Comp, Cost), valid_call_comp(Module, Literal1,
		Literal2, CheckCall, Comp), List),
	list(List, ucost(Literal2, Cost)),
	lists_to_disj(List, Disj),
	( Disj == fail -> Literal = Literal0
	;
	    Literal0=Literal2,
	    Literal = (
		apply_trust_literal(Disj, Cost, Updates),
		try_finally(Literal0, restore_resources(Updates))
	    )
	).

ucost(check_cost(_, [Comp|_], Cost), Literal, Cost) :- arg(1, Comp, Literal).

resprof_type(pred).
resprof_type(comp).

valid_call_comp(Module, Pred0, Pred, CheckCall, Comp) :-
	current_fact(assertion_read(Pred0, Module,
		trust, Type, Body, _, _, _, _)),
	resprof_type(Type),
	assertion_body(Pred, _, Call, _, Comp, _, Body),
	compound_check_props(non_inst, Call, Call, CheckCall),
	Comp \== [].

/*
resprof_goal_tr(_:_,         _,                     _) :- !, fail.
resprof_goal_tr(end_of_file, end_of_file,           _) :- !, fail.
resprof_goal_tr(Goal,        prof_literal(RM:Goal), M) :-
functor(Goal, F, N),
module_qualifier(F, N, M, RM),
display(user_error, Goal), nl(user_error),
RM \== resprof_rt.
*/

:- use_module(library(compiler/c_itf_internal),
	    [multifile/3, defines/3, imports/5]).

module_qualifier(F, N, M, RM) :-
	( multifile(M, F, N) -> RM = M % multifile
	; defines(M, F, N) -> RM = M % local defined have priority
	; imports(M, _IM, F, N, EM) -> RM = EM
	; RM = M
	).
