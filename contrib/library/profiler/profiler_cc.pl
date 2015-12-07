:- module(profiler_cc, [
		cost_center/2,
		cost_center/4,
		cost_center_nc/4,
		cost_center_nf/4,
		cost_center_ncnf/4],
	    [assertions, nativeprops, profiler]).

:- use_module(library(profiler/profiler_rt)).

:- pred cost_center(Name, Goal) :: atm * callable # "Defines a
	cost center named @var{Name} at program point-level.  Due to
	implementation limitations, you can not define any predicate
	named @var{Name/0}, because we handle such cost centers like
	predicates of arity 0.".

% Note that the folowing predicates are expanded by the profiler
% package, to avoid duplicated logic. --EMM:

:- meta_predicate cost_center(?, goal).
cost_center(Name, Goal) :- cost_center(Name, Goal).

:- meta_predicate cost_center(?, ?, ?, goal).

cost_center(Name, Arity, Hooks, Goal) :-
	cost_center(Name, Arity, Hooks, Goal).

/*
cost_center(_, _, _, '$:'(Goal)) :-
	get_profile_active(0),
	!,
	'$meta_call'(Goal).
cost_center(Name, Arity, Hooks, '$:'(Goal)) :-
	cc_call(Name, Arity, Hooks, PrevECC, CutTo),
	cc_fail(PrevECC, ChPt0),
	'$meta_call'(Goal),
	cc_exit(PrevECC, ActiveCC, ChPt1),
	cc_redo(ActiveCC, ChPt0, ChPt1, CutTo).

cost_center(_, _, _, Goal) :-
	get_profile_active(0),
	!,
	call(Goal).
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Specialized versions of hooks used when some static properties has      %%
%%  been inferred (like non failure or determinism).                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- meta_predicate cost_center_ncnf(?, ?, ?, goal).
cost_center_ncnf(Name, Arity, Hooks, Goal) :-
	cost_center_ncnf(Name, Arity, Hooks, Goal).

:- meta_predicate cost_center_nc(?, ?, ?, goal).
cost_center_nc(Name, Arity, Hooks, Goal) :-
	cost_center_nc(Name, Arity, Hooks, Goal).

:- meta_predicate cost_center_nf(?, ?, ?, goal).
cost_center_nf(Name, Arity, Hooks, Goal) :-
	cost_center_nf(Name, Arity, Hooks, Goal).
