:- module(embedded_tr, [
		srcdbg_expand/4,
		srcdbg_no_expand/4,
		srcdbg_expand_decl/3,
		srcdbg_no_expand_decl/3
	    ], [assertions]).

:- use_module(library(compiler/c_itf_internal), [location/3]).
:- use_module(library(lists),                    [reverse/2]).
:- use_module(library(varnames/complete_dict)).
:- use_module(library(debugger/debugger_tr)).

:- pred srcdbg_expand/4 # "This is the expansion needed to perform
   source-level debugging.".

% for debug and trace
srcdbg_expand_decl((:- IsDecl), (:- initialization(spy(Arg))), _Mod) :-
	functor(IsDecl, Decl, 1),
	Decl=spy, !,
	arg(1, IsDecl, Arg).

% for nodebug
srcdbg_no_expand_decl((:- IsDecl), [], _Mod) :- !,
	functor(IsDecl, Decl, 1),
	Decl=spy, !.

% for debug and trace
srcdbg_expand(clause(Head, Old_B), clause(Head, New_B), _Mod, Dict) :-
	location(Src, L0, L1),
	srcdbg_expand(Head, Old_B, New_B, Dict,
	    expand_goal(Src, L0, L1, Dict, Dict1, expand)),
	complete_dict(Head-New_B, Dict, [Dict1], Dict2),
	reverse(Dict2, Dict1).

% for nodebug
srcdbg_no_expand(clause(Head, Old_B), clause(Head, New_B), _Mod, Dict) :-
	location(Src, L0, L1),
	srcdbg_expand(Head, Old_B, New_B, Dict,
	    expand_goal(Src, L0, L1, Dict, Dict1, noexpand)),
	Dict1 = Dict.

expand_goal(Goal0, _, _, _, _, _, Expand, Xs, Ys, Goal) :-
	expand_spec(Goal0, Goal, Xs, Ys, Expand),
	!.
expand_goal(Goal0, Src, L0, L1, Dict, Dict1, Expand, Xs, Ys, Goal) :-
	expand_byrd(Goal0, Src, L0, L1, Dict, Dict1, Expand, Xs, Ys, Goal).

expand_spec(true, true, Xs, Ys, _) :-
	add_pred_to_list(true, Xs, Ys), !.
expand_spec(debug, debug, Xs, Ys, _) :-
	add_pred_to_list(debug, Xs, Ys), !.
expand_spec(trace, trace, Xs, Ys, _) :-
	add_pred_to_list(trace, Xs, Ys), !.
expand_spec(nodebug, nodebug, Xs, Ys, _) :-
	add_pred_to_list(nodebug, Xs, Ys), !.
expand_spec(notrace, notrace, Xs, Ys, _) :-
	add_pred_to_list(notrace, Xs, Ys), !.
expand_spec(spy(X), spy(X), Xs, Ys, expand) :-
	add_pred_to_list(spy, Xs, Ks),
	add_spy_pred_to_list(X, Ks, Ys), !.
expand_spec(spy(_X),  true,     Xs, Xs, noexpand).
expand_spec(nospy(X), nospy(X), Xs, Ys, expand) :-
	add_pred_to_list(nospy, Xs, Zs),
	add_spy_pred_to_list(X, Zs, Ys), !.
expand_spec(nospy(_X), true,     Xs, Xs, noexpand).
expand_spec(nospyall,  nospyall, Xs, Ys, _) :-
	add_pred_to_list(nospyall, Xs, Ys), !.
expand_spec(this_module(X),  this_module(X),  Xs, Xs, _).
expand_spec(debug_module(X), debug_module(X), Xs, Xs, _).

expand_byrd(Goal, Src, L0, L1, Dict, Dict1, expand, Xs, Zs,
	    srcdbg_byrd(Goal, Pred, Src, L0, L1, d(Dict, Dict1), Number)) :-
	functor(Goal, Pred, Arity),
	add_pred_to_list(Pred, Xs, Ks),
	get_pred_number(Pred, Ks, Number),
	search_args(1, Ks, Goal, Arity, Zs, Dict),
	!.
expand_byrd(Goal, _, _, _, _, _, noexpand, _, _, Goal).
