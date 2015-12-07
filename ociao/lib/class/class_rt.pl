%%------------------------------------------------------------------------
%%
%% O'Ciao: Object Oriented Programming in Ciao/Prolog
%%
%% RUN-TIME SUPPORT FOR CLASSES
%%
%% AUTHOR : Angel Fernandez Pineda
%%
%% CLIP Laboratory - Technical University Of Madrid.
%%
%%         - Distributed under the Ciao Prolog license terms -
%%
%%------------------------------------------------------------------------
%%
%% NOTICE: data_facts module not used because of module system 
%% expansions inserted at compile time.
%%
%%------------------------------------------------------------------------
%% WARNING:
%%   Those predicates are used by O'Ciao class expansion.
%%   Dont call directly !!!!
%%------------------------------------------------------------------------

:- module(class_rt,
	[
	    mod_exp/5,
	    self/2,
	    assert_attr/2,
	    asserta_attr/2,
	    assertz_attr/2,
	    set_attr/2,
	    retract_attr/2,
	    retract_attr_nb/2,
	    retractall_attr/2,
	    current_attr/2,
	    current_attr_nb/2,	    
	    functor_concat/3
	]).

%%------------------------------------------------------------------------

:- use_module(engine(internals), [
	last_module_exp/5,
	'$compile_term'/2,
	'$current_clauses'/2,
	'$inserta'/2,
	'$insertz'/2,
	'$current_instance'/5,
	'$erase'/1,
	'$unlock_predicate'/1]).
:- use_module(engine(hiord_rt), ['$meta_call'/1]).

%%------------------------------------------------------------------------
%%
%% ASSERT FAMILIY
%%
%%------------------------------------------------------------------------


assert_attr(Attr,Instance) :-
	asserta_attr(Attr,Instance). 

asserta_attr(Attr,Instance) :-
	functor_concat(Instance,Attr,InstFact),
	'$compile_term'([InstFact|true],Ptr),
	'$current_clauses'(InstFact,Root),
	'$inserta'(Root,Ptr). 

assertz_attr(Attr,Instance) :-
	functor_concat(Instance,Attr,InstFact),
	'$compile_term'([InstFact|true],Ptr),
	'$current_clauses'(InstFact,Root),
	'$insertz'(Root,Ptr).

%%------------------------------------------------------------------------
%%
%% CURRENT_FACT FAMILIY
%%
%%------------------------------------------------------------------------

current_attr(Attr,Instance) :-
	functor_concat(Instance,Attr,InstFact),
	current_attr_aux(InstFact,block).

current_attr_nb(Attr,Instance) :-
	functor_concat(Instance,Attr,InstFact),
	current_attr_aux(InstFact,no_block).

%%------------------------------------------------------------------------

current_attr_aux(Fact,Kind) :-
	'$current_clauses'(Fact,Root),
	'$current_instance'(Fact,true,Root,_,Kind),
	'$unlock_predicate'(Root).

%%------------------------------------------------------------------------
%%
%% RETRACT FAMILIY
%%
%%------------------------------------------------------------------------

retract_attr(Attr,Instance) :-
	functor_concat(Instance,Attr,InstFact),
	retract_attr_aux(InstFact,block).

retract_attr_nb(Attr,Instance) :-
	functor_concat(Instance,Attr,InstFact),
	retract_attr_aux(InstFact,no_block).

retractall_attr(Attr,Instance) :-
	functor_concat(Instance,Attr,InstFact),
	retract_attr_aux(InstFact,no_block),
	fail.

retractall_attr(_,_). 

%%------------------------------------------------------------------------

retract_attr_aux(Fact,Kind) :-
	'$current_clauses'(Fact,Root),
	'$current_instance'(Fact,true,Root,Ptr,Kind),
	'$erase'(Ptr),
	'$unlock_predicate'(Root).

%%------------------------------------------------------------------------
%%
%% SET_ATTR MACRO
%%
%%------------------------------------------------------------------------

set_attr(Attr,Instance) :-
	functor_concat(Instance,Attr,InstFact),
	functor(InstFact,InstF,A),
	functor(Aux,InstF,A),
	set_attr_aux(Aux),
	'$compile_term'([InstFact|true],Ptr),
	'$current_clauses'(InstFact,Root),
	'$inserta'(Root,Ptr).

set_attr_aux(Aux) :-
	retract_attr_aux(Aux,no_block),
	fail.

set_attr_aux(_).


%%------------------------------------------------------------------------
%%
%% SELF INSTANCE ID RETRIEVING
%%
%%------------------------------------------------------------------------

self(ID,Object) :-
	'id$fromclass'(ID,Class),
	functor(Object,Class,1),
	arg(1,Object,ID).

%%------------------------------------------------------------------------
%%
%% RUN-TIME EXPANSION INSIDE CLASS CODE
%%
%%------------------------------------------------------------------------

:- multifile 'id$fromclass'/2.
:- data      'id$fromclass'/2.

mod_exp(goal,self(Var),_,ID,Var = Exp) :-
	'id$fromclass'(ID,Class),
	functor(Exp,Class,1),
	arg(1,Class,ID),
	!.

mod_exp(K,Module:Goal,Module,ID,Exp) :-
	!,
	mod_exp(K,Goal,Module,ID,Exp).

mod_exp(K,\+ Goal,Module,ID,\+ Exp) :-
	!,
	mod_exp(K,Goal,Module,ID,Exp).

mod_exp(K,^(X,Goal),Module,ID,^(X,Exp)) :-
	!,
	mod_exp(K,Goal,Module,ID,Exp).

mod_exp(K,if(C,T,E),Module,ID,if(NC,NT,NE)) :-
	!,
	mod_exp(K,C,Module,ID,NC),
	mod_exp(K,T,Module,ID,NT),
	mod_exp(K,E,Module,ID,NE).

mod_exp(K,Body,Module,ID,Exp) :-
	functor(Body,F,2),
	( F = ',' ; F= '->' ; F = ';' ),
	!,
	arg(1,Body,Left),
	arg(2,Body,Right),
	mod_exp(K,Left,Module,ID,LeftBody),
	mod_exp(K,Right,Module,ID,RightBody),
	functor(Exp,F,2),
	arg(1,Exp,LeftBody),
	arg(2,Exp,RightBody).

mod_exp(goal,Goal,Module,ID,Exp) :-
	atom_concat(Module,':goal$exp',CheckF),
	functor(Check,CheckF,3),
	arg(1,Check,Goal),
 	arg(2,Check,ID),
 	arg(3,Check,Exp),
	'$meta_call'(Check),
	!.

mod_exp(fact,Goal,Module,ID,Exp) :-
	atom_concat(Module,':fact$exp',CheckF),
	functor(Check,CheckF,3),
	arg(1,Check,Goal),
 	arg(2,Check,ID),
 	arg(3,Check,Exp),
	'$meta_call'(Check),
	!.

mod_exp(spec,F/A,Module,ID,ExpF/ExpA) :-
	functor(Check,F,A),
	mod_exp(goal,Check,Module,ID,ExpCheck),
	!,
	functor(ExpCheck,ExpF,ExpA).

mod_exp(K,Goal,From,_,Exp) :- 
	last_module_exp(Goal,K,From,-,Exp),
	!.

mod_exp(_,Goal,_,_,Goal).

%%------------------------------------------------------------------------

:- use_module(library(terms), [copy_args/3]).

functor_concat(Atom,Term,NewTerm) :-
	functor(Term,F,A),
	atom_concat(Atom,F,NewF),
	functor(NewTerm,NewF,A),
	copy_args(A,Term,NewTerm).
