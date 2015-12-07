:- module(profiler_tr, [profiler_def/3, profiler_goal_trans/3, cost_center/4],
	    [assertions, define_flag]).

:- use_module(library(aggregates)).
:- use_module(library(llists)).
:- use_module(library(system)).
:- use_module(library(file_utils)).
:- use_module(library(assertions/assrt_lib),
	    [assertion_read/9, assertion_body/7]).

define_flag(optimized_profiler, [on, off], off).

:- doc(bug, "Profiling over a program implemented with several
   modules is not equivalent to profile the same program implemented
   in one module, the total time is fewer in the first case.").

% Note: You must not use the use_module directive here with
% a module suitable to be profiled.

:- data renamed_db/3.
:- data cost_center_db/4.
:- data no_cost_center_db/3.
:- data all_cost_center_db/2.
:- data all_no_cost_center_db/1.

cost_center(F, N, H, M) :-
	all_cost_center_db(H0, M),
	(cost_center_db(F, N, H, M) -> true ; H = H0),
	!.
cost_center(_, _, _, M) :-
	all_no_cost_center_db(M),
	!,
	fail.
cost_center(F, N, _, M) :-
	no_cost_center_db(F, N, M),
	!,
	fail.
cost_center(F, N, H, M) :-
	cost_center_db(F, N, H, M).

profiler_def(0,           0,       _) :- !.
profiler_def(end_of_file, Clauses, M) :-
	!,
	( renamed_db(_, _, M) ->
	    Clauses = [(:- initialization(profile_module_init(M))),
		end_of_file]
	; Clauses = end_of_file
	).
profiler_def((:- include(this_module_cc_auto)), (:- include(.(MCCAuto))), M) :-
	!,
	atom_concat(M, '_cc_auto', MCCAuto),
	absolute_file_name(.(M), FN),
	atom_concat(BN, '.pl',         FN),
	atom_concat(BN, '_cc_auto.pl', FNCC),
	(file_exists(FNCC) -> true ; string_to_file("", FNCC)).
profiler_def((:- Declaration), (:- Declaration), M) :-
	!,
	profiler_proc_decl(Declaration, M).
profiler_def((Head :- Body), Clauses, M) :-
	!,
	profiler_proc_clause(Head, Body, Clauses, M).
profiler_def(Head, Clauses, M) :-
	profiler_proc_clause(Head, true, Clauses, M).

profiler_proc_decl(all_cost_center, M) :-
	assertz_fact(all_cost_center_db(1, M)).
profiler_proc_decl(all_cost_center(hooks), M) :-
	assertz_fact(all_cost_center_db(1, M)).
profiler_proc_decl(all_cost_center(nohooks), M) :-
	assertz_fact(all_cost_center_db(0, M)).
profiler_proc_decl(all_no_cost_center, M) :-
	assertz_fact(all_no_cost_center_db(M)).
profiler_proc_decl(cost_center(F/N), M) :-
	assertz_fact(cost_center_db(F, N, 1, M)).
profiler_proc_decl(cost_center(F/N, hooks), M) :-
	assertz_fact(cost_center_db(F, N, 1, M)).
profiler_proc_decl(cost_center(F/N, nohooks), M) :-
	assertz_fact(cost_center_db(F, N, 0, M)).
profiler_proc_decl(cost_center((A, B)), M) :-
	profiler_proc_decl(cost_center(A), M),
	profiler_proc_decl(cost_center(B), M).
profiler_proc_decl(cost_center([]),    _).
profiler_proc_decl(cost_center([A|B]), M) :-
	profiler_proc_decl(cost_center(A), M),
	profiler_proc_decl(cost_center(B), M).
profiler_proc_decl(no_cost_center([]),    _).
profiler_proc_decl(no_cost_center([A|B]), M) :-
	profiler_proc_decl(no_cost_center(A), M),
	profiler_proc_decl(no_cost_center(B), M).
profiler_proc_decl(no_cost_center(F/N), M) :-
	assertz_fact(no_cost_center_db(F, N, M)).
profiler_proc_decl(no_cost_center((A, B)), M) :-
	profiler_proc_decl(no_cost_center(A), M),
	profiler_proc_decl(no_cost_center(B), M).

profiler_goal_trans(cost_center(Name, Goal0), Goal, _) :-
	cost_center_trans(unknown, Name, 0, 1, Goal0, Goal).
profiler_goal_trans(cost_center(Name, Arity, Hooks, Goal0), Goal, _) :-
	cost_center_trans(unknown, Name, Arity, Hooks, Goal0, Goal).
% Specialized versions:
profiler_goal_trans(cost_center_ncnf(Name, Arity, Hooks, Goal0), Goal, _) :-
	cost_center_trans(no_choicepoints_not_fails, Name, Arity, Hooks, Goal0,
	    Goal).
profiler_goal_trans(cost_center_nc(Name, Arity, Hooks, Goal0), Goal, _) :-
	cost_center_trans(no_choicepoints, Name, Arity, Hooks, Goal0, Goal).
profiler_goal_trans(cost_center_nf(Name, Arity, Hooks, Goal0), Goal, _) :-
	cost_center_trans(not_fails, Name, Arity, Hooks, Goal0, Goal).
profiler_goal_trans(end_of_file, _, M) :-
	retractall_fact(renamed_db(_, _, M)),
	retractall_fact(cost_center_db(_, _, _, M)),
	retractall_fact(no_cost_center_db(_, _, M)),
	retractall_fact(all_cost_center_db(_, M)),
	retractall_fact(all_no_cost_center_db(M)).

ipred_recursivity(OrigFunctor, NewFunctor, Arity, Pred, NewPred) :-
	(
	    functor(Pred, OrigFunctor, Arity),
	    Pred =.. [_|Args] ->
	    NewPred =.. [NewFunctor|Args]
	;
	    NewPred = Pred
	).

%:- pred avoid_recursivity/5 + not_fails.
avoid_recursivity(OrigFunctor, NewFunctor, Arity, (Pred, Body),
	    (NewPred, NewBody)) :-
	ipred_recursivity(OrigFunctor, NewFunctor, Arity, Pred, NewPred),
	avoid_recursivity(OrigFunctor, NewFunctor, Arity, Body, NewBody).
avoid_recursivity(OrigFunctor, NewFunctor, Arity, Pred, NewPred) :-
	ipred_recursivity(OrigFunctor, NewFunctor, Arity, Pred, NewPred).

:- doc(bug, "Check implementation of repeat/0, that creates
	choicepoints in C and undo/1, which execute goals on
	backtracking in order to see if this implementation can be
	optimized --EMM.").

cost_center_trans(unknown, Name, Arity, Hooks, Goal0, Goal) :-
% 	Goal = cost_center(Name, Arity, Hooks, Goal0).
	Goal = (
	    cc_call(Name, Arity, Hooks, PrevECC, CutTo),
	    cc_fail(PrevECC, ChPt0),
	    Goal0,
	    cc_exit(PrevECC, ActiveECC, ChPt1),
	    cc_redo(ActiveECC, ChPt0, ChPt1, CutTo)
	).
cost_center_trans(no_choicepoints_not_fails, Name, Arity, Hooks,
	    Goal0, Goal) :-
	Goal = (
	    cc_call_ncnf(Name, Arity, Hooks, PrevCC),
	    Goal0,
	    cc_exit_ncnf(PrevCC)
	).
cost_center_trans(no_choicepoints, Name, Arity, Hooks, Goal0, Goal) :-
	Goal = (
	    cc_call(Name, Arity, Hooks, PrevCC, CutTo),
	    cc_fail_nc(PrevCC),
	    Goal0,
	    cc_exit_nc(PrevCC, CutTo)
	).
cost_center_trans(not_fails, Name, Arity, Hooks, Goal0, Goal) :-
	Goal = (
	    cc_call_nf(Name, Arity, Hooks, PrevECC, CutTo),
	    Goal0,
	    cc_exit(PrevECC, ActiveECC, ChPt1),
	    cc_redo_nf(ActiveECC, ChPt1, CutTo)
	).

valid_assertions(true,  pred).
valid_assertions(true,  comp).
valid_assertions(trust, pred).
valid_assertions(trust, comp).

get_cost_center_properties(Pred, M, Properties) :-
	findall(Comp, get_cost_center_comp(Pred, M, Comp), Comps0),
	append(Comps0, Comps),
%	collapse_dups(Comps1, Comps),
	comps_to_properties(Comps, Properties).

comps_to_properties(Comps, Properties) :-
	member(no_choicepoints(_), Comps) ->
	(
	    member(not_fails(_), Comps) ->
	    Properties = no_choicepoints_not_fails
	;
	    Properties = no_choicepoints
	)
    ;
	member(not_fails(_), Comps) ->
	Properties = not_fails
    ;
	Properties = unknown.

get_cost_center_comp(Pred, M, Comp) :-
	current_prolog_flag(optimized_profiler, on),
	!,
	assertion_read(Pred, M, Status, Type, ABody, _Dict, _S, _LB, _LE),
	valid_assertions(Status, Type),
	assertion_body(Pred, _Compat, _Call, _Succ, Comp, _Comm, ABody).

profiler_proc_clause(Head, Body0, Clauses, M) :-
	functor(Head, F, N),
	( cost_center(F, N, Hooks, M) ->
	    Head =.. [_|Args],
	    atom_concat('$cc$', F, ProfFunctor),
	    ProfHead =.. [ProfFunctor|Args],
	    avoid_recursivity(F, ProfFunctor, N, Body0, Body),
	    ( renamed_db(F, N, M) ->
		Clauses = [(ProfHead :- Body)]
	    ;
		functor(Head0, F, N),
		Head0 =.. [F|Args0],
		Body1 =.. [ProfFunctor|Args0],
		atom_concat(M,  ':', P0),
		atom_concat(P0, F,   P),
		functor(Pred, F, N),
		get_cost_center_properties(Pred, M, Properties),
		cost_center_trans(Properties, P, N, Hooks, Body1, Body2),
		Clauses = [('$cc$'(M, F, N)),
		    (Head0 :-    Body2),
		    (ProfHead :- Body)],
		assertz_fact(renamed_db(F, N, M))
	    )
	;
	    Clauses = [(Head :- Body0)]
	).
