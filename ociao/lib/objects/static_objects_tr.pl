%%------------------------------------------------------------------------
%%
%% O'Ciao: Object Oriented Programming in Ciao/Prolog
%%
%% SOURCE-TO-SOURCE EXPANSION FOR STATIC OBJECT USAGE
%%
%% AUTHOR : Angel Fernandez Pineda
%%
%% CLIP Laboratory - Technical University Of Madrid.
%%
%%         - Distributed under the Ciao Prolog license terms -
%%
%%------------------------------------------------------------------------

:- use_module(library(expansion_tools)).

%%------------------------------------------------------------------------
%%
%% BODY EXPANSION FOR STATIC OBJECT USAGE (at second-pass expansion)
%% (OPTIMIZES STATIC USAGE OF STATICALLY DECLARED OBJECTS).
%%
%%------------------------------------------------------------------------

static_objects_expansion(Module,Body,NewBody) :-
	body_expander(goal_expansion,
	              fact_expansion,
		      spec_expansion,
		      Module,
		      Body,
		      NewBody).
%%------------------------------------------------------------------------
%%
%% EXPAND ONE GOAL
%%
%%------------------------------------------------------------------------

%% Unknown goal.

goal_expansion(Goal,_,_) :-
	var(Goal),
	!,
	fail.

%% Normalize static usage of objects

goal_expansion(ID:Goal,Exp,Module) :-
	atom(ID),
	instance_of(Module,ID,Cons),
	functor(Cons,F,_),
	functor(Object,F,1),
	arg(1,Object,ID),
	!,
	( goal_expansion(Object:Goal,Exp,Module) -> 
	  true
	; 
	  Exp = Object:Goal 
	).

%% Goal is related to statically declared object

goal_expansion(Object:Goal,Exp,Module) :-
	nonvar(Object),
	nonvar(Goal),
	functor(Object,Class,1),
	arg(1,Object,ID),
	instance_of(Module,ID,Cons),
	functor(Cons,Class,_),
	!,
	(optimize_goal(ID,Class,Goal,Exp,Module) -> true ; Exp = Object:Goal),
	true. 

%% Goal is related to dynamically declared object

goal_expansion(Object:Goal,'class$call'(Object,Goal,Module),Module) :-
	var(Object),
	nonvar(Goal),
	!,
	dynamic_optimization(Module).

%%------------------------------------------------------------------------

optimize_goal(ID,DerivedFrom,Goal,Exp,Module) :-
	functor(Goal,F,A),
	instance_of(Module,ID,Cons),
	functor(Cons,DerivedFrom,_),
	accessible(DerivedFrom,F,A),
	!,
	do_optimize_goal(ID,DerivedFrom,Goal,F,A,Exp).

optimize_goal(ID,DerivedFrom,Goal,_,Module) :-
	instance_of(Module,ID,Cons),
	functor(Cons,DerivedFrom,_),
	!,
	message(Module,error,
	 ['call to inaccessible predicate at instance ',ID,' : ',Goal]
	),
	fail.

optimize_goal(ID,DerivedFrom,Goal,_,Module) :-
	!,
	message(Module,error,
	 ['unknown instance ',ID,' of class ',DerivedFrom,' at ',Goal]),
	fail.

%%------------------------------------------------------------------------

do_optimize_goal(ID,DerivedFrom,Goal,F,A,At:Exp) :-
	implementation(DerivedFrom,method,At,F,A),
	!,
	atom_concat(['obj$',F],NewF),
	Goal    =.. [_|Args],
	append(Args,[ID],NewArgs),
	Exp     =.. [NewF|NewArgs],
	true. 

%%------------------------------------------------------------------------
%%
%% EXPAND ONE FACT
%%
%%------------------------------------------------------------------------

%% Unknown fact.

fact_expansion(Var,_,_) :-
	var(Var),
	!,
	fail.

%% Normalize static usage of objects

fact_expansion(ID:Fact,Exp,Module) :-
	atom(ID),
	instance_of(Module,ID,Cons),
	functor(Cons,F,_),
	functor(Object,F,1),
	arg(1,Object,ID),
	!,
	( fact_expansion(Object:Fact,Module,Exp) -> 
	  true
	; 
	  Exp = Object:Fact 
	).

%% Fact is related to statically declared object

fact_expansion(Object:Fact,_,Module) :-
	nonvar(Object),
	nonvar(Fact),
	functor(Object,Class,1),
	arg(1,Object,ID),
	atom(ID),
	instance_of(Module,ID,Cons),
	functor(Cons,Class,_),
	!,
	validate_fact(Class,ID,Fact,Module),
	fail.

%%------------------------------------------------------------------------

validate_fact(Class,_,Fact,_) :-
	functor(Fact,F,A),
	implementation(Class,attribute,_,F,A),
	accessible(Class,F,A),
	!. 

validate_fact(Class,ID,Fact,Module) :-
	functor(Fact,F,A),
	implementation(Class,attribute,_,F,A),
	!,
	message(error,Module,
	 ['inaccessible attribute ',Fact,' at instance ',ID]),
	fail. 

validate_fact(_,ID,Fact,Module) :-
	nonvar(Fact),
	!,
	message(error,Module,
	 ['unknown attribute ',Fact,' at instance ',ID]),
	fail. 

%%------------------------------------------------------------------------
%%
%% EXPAND ONE SPEC
%%
%%------------------------------------------------------------------------

spec_expansion(Spec,_,_) :-
	var(Spec),
	!,
	fail.

spec_expansion(F/A,_,_) :-
	( var(F) ; var(A) ),
	!,
	fail.

spec_expansion(Module:F/A,At:NewF/NewA,Module) :-
	functor(Check,F,A),
	goal_expansion(Module:Check,At:ExpCheck,Module),
	!,
	functor(ExpCheck,NewF,NewA).
