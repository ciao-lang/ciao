:- module(det_hook_rt,
	[det_try/3, '$pending_cut_goals'/0],
	[assertions, isomodes]).

:- doc(nodoc,assertions).

:- doc(title, "Runtime predicates for call on determinate").

:- doc(author, "Jose F. Morales").
:- doc(author, "Manuel Carro").

:- doc(module, "Implementation of variant of call and cut which
executes pending goals when the computation has no more
alternatives.").

:- data cut_goal/2.

:- pred det_try(Goal, OnCut, OnFail) : callable * callable *
callable # "@var{Action} is called, and @var{OnCut} and @var{OnFail}
are goals to be executed when @var{Goal} is cut or when it finitely
fails, respectively.  In order for this to work, cutting must be
performed in a special way, by using the @pred{!!/0} predicate, also
provided by this module.".

:- meta_predicate det_try(goal, goal, goal).

det_try(Action, OnCutGoal, OnFailGoal) :-
	cut_goal(OnCutGoal),
	(Action ; remove_cut_goal, OnFailGoal, fail).
	
cut_goal(Goal) :-
	'$metachoice'(C),
	asserta_fact(cut_goal(C, Goal)).

remove_cut_goal :-
	'$metachoice'(C),
	retract_fact(cut_goal(C, _)).

:- doc(hide, '$pending_cut_goals'/0).

'$pending_cut_goals' :-
	'$metachoice'(X),
	pending_cut_goals_2(X).

pending_cut_goals_2(X) :-
	current_fact(cut_goal(C, Goal)), 
	C >= X, !,
	retract_fact(cut_goal(C, Goal)),
	Goal,
	pending_cut_goals_2(X).
pending_cut_goals_2(_).

/*
:- doc(doinclude, '!!'/0).
:- pred '!!' # "Performs a special cut which prunes alternatives away,
as the usual cut, but which also executes the goals specified as
@var{OnCut} for any call in the scope of the cut.".
*/
