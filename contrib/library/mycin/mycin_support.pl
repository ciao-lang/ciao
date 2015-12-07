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
%%----------------------------------------------------------------------

:- module(mycin_support,[],[hiord,assertions,basicmodes]).

%%----------------------------------------------------------------------

:- use_module(library(aggregates), [bagof/3]).

%%----------------------------------------------------------------------
%% Properties
%%----------------------------------------------------------------------

:- export(certainty_factor/1).

:- prop certainty_factor(CF) #
	"@var{CF} is a number between -1 and 1 representing the
         certainty qualification for a given fact or rule.".

certainty_factor(CF) :-
	number(CF),
	CF >= -1,
	CF =< +1.

:- export(mycin_rule/1).

:- prop mycin_rule(Rule) #
	"@var{Rule} is a simple fact or a modus-ponens styled rule 
         which has been assigned a certainty qualifier.".

mycin_rule(cf(Fact,CF)) :-
	certainty_factor(CF),
	term(Fact).

mycin_rule((cf(Fact,CF) :- Body)) :-
	certainty_factor(CF),
	term(Fact),
	sequence(Body,term).

:- export(mycin_goal/1).
:- prop mycin_goal(Goal) #
	"@var{Goal} is a @index{mycin goal} for mycin rules, which
         may be module-qualified like Prolog goals.".

mycin_goal(Goal) :-
	term(Goal).

%%------------------------------------------------------------------------
%% DOC HEADERS
%%------------------------------------------------------------------------

:- doc(title,
	"Executing MYCIN rules from Prolog").

:- doc(author,"Angel Fernandez Pineda").

:- doc(usage,
	"This library is automatically loaded when using the
	  @em{mycin} package:
@begin{verbatim}
    :- use_package(mycin).
@end{verbatim}
	For information about how to declare mycin rulebases refer
        to @library{mycin_rulebase}.
        ").

:- doc(module,
	"This library is designed to provide a way to call
         mycin rules and retrieve the corresponding certainty factor.
         Those rules are called in the same fashion as Prolog goals
         via @pred{cf/2}.

         This package is automatically imported when defining 
         @concept{mycin rules} using the @library{mycin_rulebase} package.
         ").

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

:- doc(hide,or_CF_inference/2).

or_CF_inference([],0).

or_CF_inference([SingleRuleCF],SingleRuleCF) :- 
	!.

or_CF_inference([RuleCF|OtherRulesCF],CF) :-
	or_CF_inference(OtherRulesCF,Aux),
	or2(RuleCF,Aux,CF).

%%----------------------------------------------------------------------
%% Execute mycin goal
%%----------------------------------------------------------------------

:- export(cf/2).

:- meta_predicate cf(pred(1),?).

:- pred cf(+MycinGoal,-CF) : (certainty_factor(CF),mycin_goal(MycinGoal)) #
	"Retrieves the @concept{certainty factor} for @var{MycinGoal}
         into @var{CF} variable. For Example:
@begin{verbatim}
 winner(Hourse) cf Certainty
@end{verbatim}
	Note that @var{CF} must be a free variable at call time. Otherwise,
        @pred{cf/2} may fail. @var{MycinGoal} must be previously imported
        from a mycin rulebase via a regular @decl{use_module/1} declaration.
        @var{MycinGoal} may be module qualified, too. 

        Note that this predicate is capable of backtracking over all solutions
        for @var{MycinGoal}.".

cf(MycinGoal,ResultingCF) :-
	if(
           bagof(CFArg, MycinGoal(CFArg), OR_CF_List),
	   or_CF_inference(OR_CF_List,ResultingCF),
	   ResultingCF = 0
	).
