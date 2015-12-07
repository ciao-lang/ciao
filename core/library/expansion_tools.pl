%%------------------------------------------------------------------------
%% 
%% TOOLS FOR SECOND-PASS CODE EXPANDERS
%%
%% AUTHOR:  Angel Fernandez Pineda
%%
%%------------------------------------------------------------------------

:- module(expansion_tools,[],[assertions,hiord]).


%%------------------------------------------------------------------------

:- use_module(library(compiler/c_itf)).

%%------------------------------------------------------------------------

:- doc(title,     "Code translation utilities").
:- doc(subtitle,  "A tool for code-expander modules").
:- doc(author,    "Angel Fernandez Pineda").

:- doc(module,
	"This library offers a general way to perform clause body 
         expansions. Goal, fact and spec translation predicates are
         authomatically called when needed, while this utility navigates
         through the meta-argument specification of the body itself.
         All predicates within this library must be called at 
         @em{second-pass expansions}, since it uses information stored
         at @lib{c_itf} library.").

:- prop fact_expander(Expander) : expander_pred #
	"@var{Expander} is a user-defined predicate which performs
         @em{fact} meta-type translation".

:- prop goal_expander(Expander) : expander_pred #
	"@var{Expander} is a user-defined predicate which performs
         @em{goal} meta-type translation".

:- prop spec_expander(Expander) : expander_pred #
	"@var{Expander} is a user-defined predicate which performs
         @em{spec} meta-type translation".

:- prop expander_pred(Pred) # 
	"@var{Pred} is a user-defined predicate used to perform code 
         translations. First argument will be binded to the corresponding
         term to be translated. Second argument must be binded to the
         corresponding translation. Third argument will be binded
         to the current module were first argument appears.
         Additional arguments will be user-defined.
        ".

:- doc(doinclude,expander_pred/1).

fact_expander(_).
goal_expander(_).
spec_expander(_).
expander_pred(_).

:- doc(bug,"@em{pred(N)} meta-arguments are not supported 
                at this moment.").

%%------------------------------------------------------------------------
%% 
%% META-PREDICATE SPECIFICATION (MACRO)
%%
%%------------------------------------------------------------------------

:- doc(imports_meta_pred/3,
	"Macro provided in order to know meta-predicate specifications
         accessible from a module.").

:- pred imports_meta_pred(Module,MetaSpec,AccessibleAt) :
	( atm(Module), term(MetaSpec) ) #
        "Tells whether @var{MetaSpec} meta-predicate specification is
         accessible from @var{Module}. @var{AccessibleAt} will be 
         binded to '-' whether meta-predicate is a builtin one. If not,
         it will be unified with the module which defines the meta-predicate.
        ".

% rtcheck -- EMM

:- export(imports_meta_pred/3).

imports_meta_pred(_,\+(goal),-).

imports_meta_pred(_,^(?,goal),-).

imports_meta_pred(_,if(goal,goal,goal),-).

imports_meta_pred(_,','(goal,goal),-).

imports_meta_pred(_,';'(goal,goal),-).

imports_meta_pred(_,'->'(goal,goal),-).

imports_meta_pred(Module,Goal,Module) :-
	c_itf:meta_args(Module,Goal).

imports_meta_pred(ThisModule,Goal,Module) :-
	c_itf:defines_module(Base,ThisModule),
	c_itf:imports_pred(Base,Lib,_,_,_,Goal,_),
	lib2module(Lib,Module),
	Goal \== 0.

lib2module(Lib,Mod) :-
	base_name(Lib,Path),
	!,
	defines_module(Path,Mod).

%%------------------------------------------------------------------------
%% 
%% BODY EXPANDER
%%
%%------------------------------------------------------------------------

:- doc(body_expander/6,
	"This predicate is the main translation tool. It navigates through
	 a clause body, when a single 
	 @em{goal} appears, user-code is called
	 in order to perform a translation. Whether user-code fails to
	 translate the involved goal, it remains the same.
         Regardless that goal is translated or not, an argument 
	 expansion will be performed over all goals if applicable
         (see @pred{arg_expander/6} predicate).

         Variable (unknown at compile time) goals will also be 
         attempt to translate.
	").

:- pred body_expander(GoalTrans, FactTrans, SpecTrans,
	Module, Body, ExpandedBody) :
        ( goal_expander(GoalTrans),fact_expander(FactTrans),
	  spec_expander(SpecTrans) , atm(Module), 
%	  nonvar(Body), % rtcheck -- EMM
	  var(ExpandedBody) ) #
        "Translates @var{Body} to @var{ExpandedBody} by the usage of
         user-defined translators @var{GoalTrans}, 
         @var{FactTrans} and @var{SpecTrans}. The 
         module where the original body appears must be unified with
         @var{Module} argument.
        ".

:- export(body_expander/6).

:- meta_predicate body_expander(pred(3),pred(3),pred(3),?,?,?).

%% single user goal.

body_expander(GoalExp,FactExp,SpecExp,Module,Body,NewBody) :-
	user_goal(Body),
	!,
	( GoalExp(Body,ExpBody,Module) -> 
	  (
%	  inform_user(['goal exp 1: ',Body,' : ',ExpBody]),
	  arg_expander(GoalExp,FactExp,SpecExp,Module,ExpBody,NewBody)
%	  inform_user(['arg exp 1:  ',ExpBody,' : ',NewBody])
	  )
	;
	  (
	  arg_expander(GoalExp,FactExp,SpecExp,Module,Body,NewBody)
%	  inform_user(['arg exp 2:  ',Body,' : ',NewBody])
	  )
	).

%% not a single user goal.

body_expander(GoalExp,FactExp,SpecExp,Module,Body,NewBody) :-
	arg_expander(GoalExp,FactExp,SpecExp,Module,Body,NewBody).
%	inform_user(['arg exp 3:  ',Body,' : ',NewBody]),
%	true.

%%------------------------------------------------------------------------
%% 
%% ARGUMENT EXPANDER
%% ( must not fail )
%%------------------------------------------------------------------------


:- doc(arg_expander/6,
	"This predicate is an auxiliary translation tool, which is 
         used by @pred{body_expander/6} predicate. 
         It remains exported as a macro. 
         The predicate navigates through the @em{meta-argument specification}
         of a goal. Whether a @em{goal,fact or spec} argument appears,
	 user-code is called in order to perform a translation. 
         Whether user-code fails to translate the involved argument, 
	 it remains the same.
         Builtins as ','/2 or ';'/2 are treated 
         as meta-predicates defining @em{goal} meta-arguments.
         When a @em{goal} meta-argument is located, 
	 @pred{body_expander/6} will be called in order to navigate
         through it. Notice that a @em{goal} meta-argument may be unified
         with another goal defining another meta-argument, so 
         navigation is required.
         If arguments are not known to arg_expander/6, translation 
         will not occur. This is posible whether goal
         or qualifing module are variables.
	").

:- pred arg_expander(GoalTrans, FactTrans, SpecTrans,
	Module, Goal, ExpandedGoal) :
        ( goal_expander(GoalTrans),fact_expander(FactTrans),
	  spec_expander(SpecTrans) , atm(Module), 
%	  nonvar(Goal), % -- rtcheck -- EMM
	  var(ExpandedBody) ) #
        "Translates @var{Goal} to @var{ExpandedGoal} by applying
         user-defined translators (@var{GoalTrans}, 
         @var{FactTrans} and @var{SpecTrans}) to each 
         meta-argument present at such goal.
         The module where the original goal appears must be unified with
         @var{Module} argument.
        ".

:- export(arg_expander/6).

:- meta_predicate arg_expander(pred(3),pred(3),pred(3),?,?,?).

% unknown args

arg_expander(_GoalExp,_FactExp,_SpecExp,_Module,Goal,Goal) :-
	var(Goal),
	!.

arg_expander(_GoalExp,_FactExp,_SpecExp,_Module,M:G,M:G) :-
	( var(G) ; var(M) ),
	!.

% will be run-time expanded

arg_expander(_GoalExp,_FactExp,_SpecExp,_Module,M:G,M:G) :-
	\+ atom(M),
	!.

arg_expander(GoalExp,FactExp,SpecExp,Module,Goal,Exp) :-
	arg_expander_aux(GoalExp,FactExp,SpecExp,Module,Goal,Exp),
	!.

% goal has no meta-arguments

arg_expander(_,_,_,_,Goal,Goal).

%%------------------------------------------------------------------------

% goal is module-qualified

arg_expander_aux(GoalExp,FactExp,SpecExp,Module,AtModule:Goal,Exp) :-
	!,
	functor(Goal,F,A),
	A > 0,
	functor(Meta,F,A),
	imports_meta_pred(Module,Meta,AtModule),
	Goal =.. [_|Args],
	Meta =.. [_|MetaSpec],
	arg_exp_aux(MetaSpec,Args,GoalExp,FactExp,SpecExp,Module,ExpArgs),
	Exp  =.. [F|ExpArgs].

% goal is not module-qualified

arg_expander_aux(GoalExp,FactExp,SpecExp,Module,Goal,Exp) :-
	functor(Goal,F,A),
	A > 0,
	functor(Meta,F,A),
	imports_meta_pred(Module,Meta,AtModule),
	!,
	Goal =.. [_|Args],
	Meta =.. [_|MetaSpec],
	arg_exp_aux(MetaSpec,Args,GoalExp,FactExp,SpecExp,Module,ExpArgs),
	NewGoal =.. [F|ExpArgs],
	( AtModule = '-' -> Exp = NewGoal ; Exp = AtModule:NewGoal ).


arg_exp_aux([],[],_,_,_,_,[]).

arg_exp_aux([goal|Nms],[Arg|Na],GExp,FExp,SExp,Mod,[ExpArg|Nea]) :-
	body_expander(GExp,FExp,SExp,Mod,Arg,ExpArg),
	!,
	arg_exp_aux(Nms,Na,GExp,FExp,SExp,Mod,Nea).

arg_exp_aux([fact|Nms],[Arg|Na],GExp,FExp,SExp,Mod,[ExpArg|Nea]) :-
	FExp(Arg,ExpArg,Mod),
	!,
	arg_exp_aux(Nms,Na,GExp,FExp,SExp,Mod,Nea).

arg_exp_aux([spec|Nms],[Arg|Na],GExp,FExp,SExp,Mod,[ExpArg|Nea]) :-
	SExp(Arg,ExpArg,Mod),
	!,
	arg_exp_aux(Nms,Na,GExp,FExp,SExp,Mod,Nea).

arg_exp_aux([pred(N)|Nms],[Arg|Na],GExp,FExp,SExp,Mod,[Arg|Nea]) :-
	!,
	inform_user(['PRED(',N,') : ',Arg]),
	arg_exp_aux(Nms,Na,GExp,FExp,SExp,Mod,Nea).

arg_exp_aux([_|Nms],[Arg|Na],GExp,FExp,SExp,Mod,[Arg|Nea]) :-
	!,
	arg_exp_aux(Nms,Na,GExp,FExp,SExp,Mod,Nea).

%%------------------------------------------------------------------------
%% MACROS
%%------------------------------------------------------------------------

user_goal(X) :- var(X),!.
user_goal(_:_) :- !.
user_goal(true) :- !,fail.
user_goal(\+(_)) :- !,fail.
user_goal(^(_,_)) :- !,fail.
user_goal(if(_,_,_)) :- !,fail.
user_goal(';'(_,_)) :- !,fail.
user_goal(','(_,_)) :- !,fail.
user_goal('->'(_,_)) :- !,fail.
user_goal(_).
