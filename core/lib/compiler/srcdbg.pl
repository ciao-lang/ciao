:- module(srcdbg, [srcdbg_expand/6], [assertions]).

:- use_module(library(compiler/c_itf_internal), [location/3]).
:- use_module(library(debugger/debugger_tr)).

:- pred srcdbg_expand/6 # "This is the expansion needed to perform
   source-level debugging.".

srcdbg_expand(Old_H, Old_B, Old_H, New_B, Dict, Dict1) :-
	location(Src, L0, L1),
	debugger_tr:srcdbg_expand(Old_H, Old_B, New_B, Dict,
	    expand_goal(Src, L0, L1, Dict, Dict1)).

expand_goal(true, _, _, _, _, _, Xs, Xs, true) :- !.
expand_goal(Goal, Src, L0, L1, Dict, Dict1, Xs, Zs,
	    srcdbg_spy(Goal, Pred, Src, L0, L1, d(Dict, Dict1), Number)) :-
	functor(Goal, Pred, Arity),
	add_pred_to_list(Pred, Xs, Ks),
	get_pred_number(Pred, Ks, Number),
	search_args(1, Ks, Goal, Arity, Zs, Dict),
	!.
