%%------------------------------------------------------------------------
%%
%% O'Ciao: Object Oriented Programming in Ciao/Prolog
%%
%% AUXILIARY FILE FOR
%% SOURCE-TO-SOURCE EXPANSION FOR CLASS DECLARATION
%%
%% USAGE:
%% :- include(library(class/class_tr_aux)).
%%
%% AUTHOR : Angel Fernandez Pineda
%% 
%% CLIP Laboratory - Technical University Of Madrid.
%%
%%         - Distributed under the Ciao Prolog license terms -
%%
%%------------------------------------------------------------------------
%% This file is used by class_tr.pl in order to expand method clauses.
%%
%% THE FOLLOWING LIBRARIES ARE ASSUMED TO BE IMPORTED
%%
%% c_itf_internal
%% class_error_reporting
%% class_itf
%%
%%------------------------------------------------------------------------
%%
%% method_expansion/4 is assumed to be called at second pass,
%% when all module dependences has been solved.
%% 
%%------------------------------------------------------------------------

:- use_module(library(expansion_tools)).

method_expansion(Body,Module,InstVar,Exp) :-
	body_expander(goal_expansion(InstVar),
	              fact_expansion(InstVar),
		      spec_expansion(InstVar),
		      Module,
		      Body,
		      Exp).

%%------------------------------------------------------------------------
%%
%% EXPAND ONE GOAL
%%
%% goal_expansion(goal,expandedgoal,module,instanceID)
%%------------------------------------------------------------------------

goal_expansion(Goal,InstanceID,Exp,Module):-
	goal_expansion_(Goal,Exp,Module,InstanceID).

%% unknown goal

goal_expansion_(Var,class_rt(ID):Var,_,ID) :-
	var(Var),
	!.

%% Avoid invalid usage of method/attribute as module-qualified

goal_expansion_(A_Module:Goal,Exp,Module,ID) :-
	atom(A_Module),
	A_Module = Module,
	!,
%	goal_expansion(Goal,Exp,Module,ID).
	goal_expansion(Goal,ID,Exp,Module).

%% Goal is not related to this class

goal_expansion_(_:_,_,_,_) :-
	!,
	fail.

%% retrieving self instance ID 

goal_expansion_(self(Var),_,Module,_) :-
	nonvar(Var),
	!,
	message(Module,error,
	 ['argument to self/1 must be a free variable']),
	fail.

goal_expansion_(self(Var),class_rt:self(InstVar,Var),_,InstVar) :-
	var(Var),
	!.

%% Goal belongs to assert/retract family and involves 
%% virtual attribute.

goal_expansion_(Goal,NewGoal,Module,InstVar) :-
	fact2attr(Goal,Fact,_),
	functor(Goal,AssrtPred,1),
	\+ functor(Fact,inherited,_),
	functor(Fact,F,A),
	is_virtual(Module,F,A),
	is_state(Module,F,A),
	!,
	functor(NewGoal,AssrtPred,1),
	arg(1,NewGoal,virtual(InstVar):Fact).

%% Goal belongs to assert/retract family and involves 
%% explicitly inherited attr,e.g.: asserta_fact(inherited attr(88)).

goal_expansion_(Goal,class_rt:NewGoal,Module,InstVar) :-
	fact2attr(Goal,FactArg,NewGoal),
	nonvar(FactArg),
	FactArg = inherited(Fact),
	functor(Fact,F,A),
	inherited_attribute_from(Module,AtClass,F/A),
	!,
	Fact =.. [_|Args],
	atom_concat([':',AtClass,'::',F],NewF),
	NewFact =.. [NewF|Args],
	arg(1,NewGoal,NewFact),
	arg(2,NewGoal,InstVar).

goal_expansion_(Goal,fail,Module,_) :-
	fact2attr(Goal,FactArg,_),
	nonvar(FactArg),
	FactArg = inherited(Fact),
	functor(Fact,_,_),
	!,
	message(Module,error,['unknown inherited attribute in ',Goal]).

%% Goal belongs to assert/retract family and involves some attribute,
%% e.g.: retract(attr(_)).

goal_expansion_(Goal,class_rt:NewGoal,Module,InstVar) :-
	fact2attr(Goal,Fact,NewGoal),
	nonvar(Fact),
	functor(Fact,F,A),
	attribute_from(Module,AtClass,F/A),
	!,
	Fact =.. [_|Args],
	atom_concat([':',AtClass,'::',F],NewF),
	NewFact =.. [NewF|Args],
	arg(1,NewGoal,NewFact),
	arg(2,NewGoal,InstVar). 

%% Goal is a virtual method declared at this class,e.g.:
%% :- virtual v/1.
%% p(X) :- v(X).

goal_expansion_(Goal,(virtual(InstVar):Goal),Module,InstVar) :-
	functor(Goal,F,A),
	is_virtual(Module,F,A),
	!.

%% Goal is an explicitly inherited attribute,i.e: inherited attr(I)

goal_expansion_(inherited(Goal),
               class_rt:current_attr(InstFact,Inst),Module,Inst) :-
	functor(Goal,F,A),
	inherited_attribute_from(Module,AtClass,F/A),
	!,
	Goal =.. [_|Args],
	atom_concat([':',AtClass,'::',F],InstF),
	InstFact =.. [InstF|Args].
	
%% Goal is an explicitly inherited method, i.e: inherited method(K)

goal_expansion_(inherited(Goal),AtClass:NewGoal,Module,InstVar) :-
	functor(Goal,F,A),
	inherited_method_from(Module,AtClass,F/A),
	!,
	method_head(Goal,NewGoal,InstVar).

%% Invalid inherited goal

goal_expansion_(inherited(Goal),_,Module,_) :-
	!,
	message(Module,error,['unknown inherited goal: ',Goal]),
	fail.

%% Goal is an attribute, e.g.: attr(I)

goal_expansion_(Goal,class_rt:current_attr(InstFact,Inst),Module,Inst) :-
	functor(Goal,F,A),
	attribute_from(Module,AtClass,F/A),
	!,
	Goal =.. [_|Args],
	atom_concat([':',AtClass,'::',F],InstF),
	InstFact =.. [InstF|Args].

%% Goal is a method, e.g.: mymethod(8,7)

goal_expansion_(Goal,AtClass:NewGoal,Module,InstVar) :-
	functor(Goal,F,A),
	method_from(Module,AtClass,F/A),
	!,
	method_head(Goal,NewGoal,InstVar).

%%------------------------------------------------------------------------
%%
%% EXPAND ONE FACT
%%
%%------------------------------------------------------------------------

fact_expansion(Fact,Inst,Exp,Module) :-
	fact_expansion_(Fact,Exp,Module,Inst).

%% Fact is unknown

fact_expansion_(Fact,class_rt(Inst):Fact,_,Inst) :-
	var(Fact),
	!.

%% Avoid invalid calls to class as a module.

fact_expansion_(A_Module:Fact,Exp,Module,Inst) :-
	atom(A_Module),
	A_Module = Module,
	!,
%	fact_expansion(Fact,Exp,Module,Inst),
	fact_expansion(Fact,Inst,Exp,Module).

%% Fact is not related to current class.

fact_expansion_(_:_,_,_,_) :-
	!,
	fail.

%% Correct usage of explicitly inherited attribute.

fact_expansion_(inherited(Fact),class_rt(Inst):Fact,Module,Inst) :-
	nonvar(Fact),
	functor(Fact,F,A),
	inherited_attribute_from(Module,_,F/A),
	!.

%% Incorrect usage of attribute

fact_expansion_(Fact,Fact,Module,_) :-
	functor(Fact,F,A),
	method_from(Module,_,F/A),
	!,
	message(Module,error,
	 ['invalid argument: ', F,'/',A,' is not an attribute']).

fact_expansion_(inherited(Fact),inherited(Fact),Module,_) :-
	functor(Fact,F,A),
	inherited_method_from(Module,_,F/A),
	!,
	message(Module,error,
	 ['invalid argument: inherited ', F,'/',A,' is not an attribute']).

fact_expansion_(inherited(Fact),inherited(Fact),Module,_) :-
	!,
	message(Module,error,['unknown inherited fact: ',Fact]).

%%------------------------------------------------------------------------
%%
%% EXPAND PREDICATE SPECIFICATION
%%
%%------------------------------------------------------------------------

spec_expansion(Spec,Inst,Exp,Module) :-
	spec_expansion_(Spec,Exp,Module,Inst).

%% spec is unknown

spec_expansion_(Spec,class_rt(Inst):Spec,_,Inst) :-
	var(Spec),
	!.

spec_expansion_(inherited(Spec),class_rt(Inst):inherited(Spec),_,Inst) :-
	var(Spec),
	!.


spec_expansion_(F/A,class_rt(Inst):F/A,_,Inst) :-
	(var(F) ; var(A)),
	!.

spec_expansion_(inherited(F/A),class_rt(Inst):inherited(F/A),_,Inst) :-
	(var(F) ; var(A)),
	!.

%% explicitly inherited attribute spec.

spec_expansion_(inherited(F/A),class_rt(Inst):F/A,Module,Inst) :-
	atom(F),
	integer(A),
	A >= 0,
	inherited_attribute_from(Module,_,F/A),
	!.

spec_expansion_(inherited(F/A),NewF/NewA,Module,_) :-
	atom(F),
	integer(A),
	A >= 0,
	inherited_method_from(Module,AtClass,F/A),
	atom_concat(AtClass,':obj$',Aux),
	atom_concat(Aux,F,NewF),
	NewA is A+1,
	!.

spec_expansion_(inherited(Fact),inherited(Fact),Module,_) :-
	!,
	message(Module,error,['unknown inherited spec: ',Fact]).

%% Spec is known to involve an attribute

spec_expansion_(F/A,class_rt(Inst):inherited(F/A),Module,Inst) :-
	atom(F),
	integer(A),
	A >= 0,
	attribute_from(Module,_,F/A),
	!.

%% Spec is known to involve a method

spec_expansion_(F/A,NewF/NewA,Module,_) :-
	atom(F),
	integer(A),
	A >= 0,
	method_from(Module,AtClass,F/A),
	atom_concat(AtClass,':obj$',Aux),
	atom_concat(Aux,F,NewF),
	NewA is A+1,
	!.

%%------------------------------------------------------------------------
%%
%% MAP ASSERT/RETRACT PREDICATES TO ASSERT_ATTR/RETRACT_ATTR PREDICATES
%%
%%------------------------------------------------------------------------

fact2attr(asserta(Fact),        Fact, asserta_attr(_,_)    ).
fact2attr(assertz(Fact),        Fact, assertz_attr(_,_)    ).
fact2attr(assert(Fact),         Fact, assert_attr(_,_)     ).
fact2attr(retract(Fact),        Fact, retract_attr(_,_)    ).
fact2attr(retractall(Fact),     Fact, retractall_attr(_,_) ).
fact2attr(abolish(Fact),        Fact, retractall_attr(_,_) ).
fact2attr(assertz_fact(Fact),   Fact, assertz_attr(_,_)    ).
fact2attr(asserta_fact(Fact),   Fact, asserta_attr(_,_)    ).
fact2attr(retract_fact(Fact),   Fact, retract_attr(_,_)    ).
fact2attr(retract_fact_nb(Fact),Fact, retract_attr_nb(_,_) ).
fact2attr(retractall_fact(Fact),Fact, retractall_attr(_,_) ).
fact2attr(erase(Fact),          Fact, retractall_attr(_,_) ).
fact2attr(current_fact(Fact),   Fact, current_attr(_,_)    ).
fact2attr(current_fact_nb(Fact),Fact, current_attr_nb(_,_) ).
fact2attr(set_fact(Fact),       Fact, set_attr(_,_)        ).
