%%----------------------------------------------------------------------
%%
%% MYCIN PROGRAMMING LIBRARY
%%
%% run-time support
%%
%% AUTHOR: Angel Fernandez Pineda
%% DATE:   2002
%%
%% Distributed under Ciao Prolog license terms
%%
%% NOTE:   CF stands for "certainty factor"
%%----------------------------------------------------------------------

:- module(mycin_rulebase_rt,[],[hiord]).

%%----------------------------------------------------------------------
%% Combine evidence from premises
%%----------------------------------------------------------------------

min([X],X) :- !.
min([X|N],Min) :-
	min_aux(N,X,Min).

min_aux([],X,X).
min_aux([Y|N],X,Min) :-
	X < Y ,
	!,
	min_aux(N,X,Min).

min_aux([Y|N],_,Min) :-
	min_aux(N,Y,Min).

%%----------------------------------------------------------------------
%% Propagation of certainty (AND-CF-accumulation)
%%----------------------------------------------------------------------

certainty_propagation(RuleCF,GoalCFList,CF) :-
	min(GoalCFList,X),
	X > 0,
	!,
	CF is X * RuleCF.

certainty_propagation(_,_,0).

:- export(certainty_propagation/3).

%%----------------------------------------------------------------------
%% Translate deterministic goals to mycin goals
%%----------------------------------------------------------------------

:- meta_predicate(mycinly(goal,?)).

mycinly(\+(Goal),-1) :-
	call(Goal),
	!.

mycinly(\+(_),1) :-
	!.

mycinly(!,0) :- !.

mycinly((A;B),CF) :- 
	!,
	mycinly(A,CF1),
	mycinly(B,CF2),
	or2(CF1,CF2,CF).

mycinly(if(A,B,C),0) :- 
	!,
	throw(invalid_goal_at_mycin_rule(if(A,B,C))).

mycinly('->'(A,B),CF) :- 
	!,
	mycinly((\+(A);B),CF).

mycinly(Goal,1) :-
	call(Goal),
	!.

mycinly(_,-1).

:- export(mycinly/2).

%% Auxiliary: OR-CF inference for deterministic goals

or2(1,1,1).
or2(-1,-1,-1).
or2(-1,1,0).
or2(1,-1,0).

or2(0,1,1).
or2(1,0,1).
or2(0,-1,-1).
or2(-1,0,-1).

%%----------------------------------------------------------------------
%% Validate certainty factor for mycin metarule
%%----------------------------------------------------------------------

validate_metarule_cf(CF) :-
	number(CF),
	CF >= -1,
	CF =< 1.

:- export(validate_metarule_cf/1).
