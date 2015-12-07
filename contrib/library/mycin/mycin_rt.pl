%%----------------------------------------------------------------------
%%
%% MYCIN PROGRAMMING LIBRARY
%%
%% run-time support
%%
%% AUTHOR: Angel Fernandez Pineda
%% DATE:   FEBRAURY 2000
%%
%% NOTE:   CF stands for "certainty factor"
%%----------------------------------------------------------------------

:- module(mycin_rt,[],[]).

:- use_module(library(aggregates), [bagof/3]).

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
%% Certainty inference from several rules (OR-CF-accumulation)
%%----------------------------------------------------------------------

%% Auxiliary: OR-CF inference from two rules.

or2(CF1,CF2,CF) :-
	CF1 >= 0,
	CF2 >= 0,
	!,
	CF is ((CF1+CF2)-(CF1*CF2)).

or2(CF1,CF2,CF) :-
	CF1 =< 0,
	CF2 =< 0,
	!,
	CF is ((CF1+CF2)+(CF1*CF2)).

or2(CF1,CF2,CF) :-
	ACF1 is abs(CF1),
	ACF2 is abs(CF2),
	min([ACF1,ACF2],M),
	CF is ((CF1+CF2)/(1-M)).

%% OR-CF inference from several rules.

:- export(or_CF_inference/2).

or_CF_inference([],0).

or_CF_inference([SingleRuleCF],SingleRuleCF) :- !.

or_CF_inference([RuleCF|OtherRulesCF],CF) :-
	or_CF_inference(OtherRulesCF,Aux),
	or2(RuleCF,Aux,CF).

%%----------------------------------------------------------------------
%% Translate deterministic goals to mycin goals
%%----------------------------------------------------------------------

:- meta_predicate(mycinly(goal,?)).

%% mycinly(Goal,CertaintyFactor).

%mycinly(\+(Goal),CF) :-
%	mycinly(Goal,CF1),
%	CF is -1*CF1,
%	!.

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

mycinly('->'(A,B),0) :- 
	!,
	throw(invalid_goal_at_mycin_rule('->'(A,B))).

mycinly(Goal,1) :-
	call(Goal),
	!.

mycinly(_,-1).

:- export(mycinly/2).

%%----------------------------------------------------------------------
%% Execute mycin goal
%%----------------------------------------------------------------------

:- meta_predicate(mycin(goal,?,?)).

mycin(Goal,CFArg,CF) :-
	if(bagof(CFArg,Goal,OR_CF_List),
	  or_CF_inference(OR_CF_List,CF),
	  CF = 0
	).

:- export(mycin/3).

%%----------------------------------------------------------------------
%% Execute external mycin goal
%%----------------------------------------------------------------------

:- export(extern/3).

:- meta_predicate extern(goal,?,?).

extern(Goal,CF,CF) :-
	if(Goal,true,CF = 0).
