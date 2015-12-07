%%------------------------------------------------------------------------
%%
%% O'Ciao: Object Oriented Programming in Ciao/Prolog
%%
%% SOURCE-TO-SOURCE EXPANSION FOR OBJECT USAGE
%%
%% AUTHOR : Angel Fernandez Pineda
%%
%% CLIP Laboratory - Technical University Of Madrid.
%%
%%         - Distributed under the Ciao Prolog license terms -
%%
%%------------------------------------------------------------------------

:- module(objects_tr,[obj_sentence_trans/3,obj_clause_trans/3]).

%%------------------------------------------------------------------------

:- use_module(library(compiler/c_itf_internal)).
:- use_module(library(lists), [append/3]).
:- use_module(library(class/class_itf)).
:- use_module(library(objects/objects_error_reporting)).

% Auxiliary
:- include(library(objects/objects_tr_aux)).
:- include(library(objects/static_objects_tr)).
:- use_module(library(terms), [atom_concat/2]).

%%------------------------------------------------------------------------

:- data debug/0.               % Show expansion.

:- data module/1.              % Module being expanded.
:- data used_class/2.          % MODULE uses CLASS.
:- data instance_of/3.         % MODULE declares ID as an static 
                               % instance of CONS.
:- data dynamic_optimization/1.% Module must optimize dynamic calls to 
                               % methods.
:- data analyze/1.             % Module must analyze semantics of Var:goal.

%%------------------------------------------------------------------------

% debug.

%%------------------------------------------------------------------------
%%
%% AVOID ANY EXPANSION FROM Ciao TOPLEVEL
%%
%%------------------------------------------------------------------------

:- data toplevel_warning/0.

obj_sentence_trans((:- add_clause_trans(obj_clause_trans/3)),[],Module) :-
	\+ toplevel_warning,
	functor(Module,user,_),
	!,
	asserta_fact(toplevel_warning),
	true.

obj_sentence_trans((:- add_clause_trans(obj_clause_trans/3)),[],Module) :-
	functor(Module,user,_),
	!.

obj_sentence_trans(_,[],Module) :-
 	functor(Module,user,_),
 	!,
 	fail.

%%------------------------------------------------------------------------
%%
%% INITIALIZATION / END OF FIRST-PASS EXPANSION
%%
%%------------------------------------------------------------------------

obj_sentence_trans(0,_,Module) :-
	retractall_fact(used_class(Module,_)),
	retractall_fact(instance_of(Module,_,_)),
	retractall_fact(module(_)),
	set_fact(dynamic_optimization(Module)),
	set_fact(analyze(Module)),
	start_of_messages(Module,['Object usage analysis at ',Module]),
	!,
	fail.

obj_sentence_trans(end_of_file,Exp,Module) :-
	( instance_of(Module,_,_) -> 
	  Init = [
		     (:- initialization('$static_instance_creation$')),
		     ('$static_instance_creation$'),
		     (:- push_prolog_flag(unused_pred_warnings, no)),
	             ('$force$rt$info$'(X) :- call(X)),
		     (:- pop_prolog_flag(unused_pred_warnings)),
		     (:- multifile 'class$call'/3)
			     % Forces Ciao compiler to generate
                             % run-time info for this 
                             % module.
	         ]
	;
	  Init = [
		     (:- push_prolog_flag(unused_pred_warnings, no)),
		     ('$static_instance_creation$'),
		     ('$force$rt$info$'(X) :- call(X)),
		     (:- pop_prolog_flag(unused_pred_warnings)),
		     (:- multifile 'class$call'/3)
		 ]
	),
	findall((:- instance_of(ID,Cons) ),
	        instance_of(Module,ID,Cons),
		StaticIDS
	),
	append(StaticIDS,Init,Aux),
	append(Aux,[end_of_file],Exp).

%%------------------------------------------------------------------------
%%
%% DECLARATIONS
%%
%%------------------------------------------------------------------------

%%------------------------------------------------------------------------
%% DISABLE OPTIMIZATIONS
%%------------------------------------------------------------------------

obj_sentence_trans((:- disable_optimization),[],Module) :-
	retractall_fact(dynamic_optimization(Module)),
	!.

obj_sentence_trans((:- disable_analysis ),[],Module) :-
	retractall_fact(analyze(Module)),
	!.

%%------------------------------------------------------------------------
%% CLASS USAGE DECLARATION
%%------------------------------------------------------------------------

obj_sentence_trans((:- use_class(Base)),
	[
	    (:- use_module(Base,[])),
	    (:- redefining(_)),
	    (:- use_class(Base))
	],Module ) :-
	class_from_base(Base,Class),
	( used_class(Module,Class) -> true ;
          asserta_fact(used_class(Module,Class)) ),
	!.

%%------------------------------------------------------------------------
%% INSTANCE DECLARATION
%%------------------------------------------------------------------------

obj_sentence_trans((:- instance_of(ID,Cons)),Exp,Module) :-
	!,
	obj_sentence_trans((:- new(ID,Cons)),Exp,Module).

obj_sentence_trans((:- new(ID,_)),[],Module) :-
	\+ atom(ID),
	!,
	message(Module,error,['invalid instance identifier ',ID,
	               ': must be an atom']).

obj_sentence_trans((:- new(ID,_)),[],Module) :-
	instance_of(Module,ID,_),
	!,
	message(Module,error,['instance identifier ',ID,' already in use']).

obj_sentence_trans((:- new(ID,Cons)),[],Module) :-
	!,
	( ground(Cons) -> true ;
	  message(Module,note,['Given constructor for instance ',ID,
	  ' is not ground'])
	),
	assertz_fact(instance_of(Module,ID,Cons)).

%%------------------------------------------------------------------------
%%
%% SECOND-PASS EXPANSION
%% 
%%------------------------------------------------------------------------

obj_clause_trans(clause(0,0),_,Module) :-
	defines_module(Base,Module),
	start_of_messages(Module,
	  ['Object usage compilation at ',Base]),
	generate_oop_info(Module),
	additional_itf_checking(Module),
%	generate_use_class_info(Module),
	!,
	fail.

obj_clause_trans(clause('$static_instance_creation$',_),
                 clause('$static_instance_creation$',Body),Module) :-
	!,
	end_of_messages(Module),
	static_instance_definition(Module,InstCreation),
	Body = catch(InstCreation,Error,
	   inform_user(['Static instances declared on ',Module,
                        ' could not be created due to exception: ',Error])),
	( debug ->
	  (
	      inform_user(['* instance creation:']),nl,
	      inform_user([InstCreation]),nl,nl
	  ) ; true
	),
	true.

obj_clause_trans(clause('$class$'(_),_),_,_) :- !,fail.
obj_clause_trans(clause('class$used'(_,_),_),_,_) :- !,fail.
obj_clause_trans(clause('class$default_cons'(_),_),_,_) :- !,fail.
obj_clause_trans(clause('class$constructor'(_,_,_,_),_),_,_) :- !,fail.
obj_clause_trans(clause('class$destructor'(_,_,_),_),_,_) :- !,fail.
obj_clause_trans(clause('class$implements'(_,_),_),_,_) :- !,fail.
obj_clause_trans(clause('goal$exp'(_,_,_),_),_,_) :- !,fail.
obj_clause_trans(clause('fact$exp'(_,_,_),_),_,_) :- !,fail.
obj_clause_trans(clause('$end$$of$$expansion$',_),_,_) :- !,fail.
obj_clause_trans(clause(mod_exp(_,_,_,_,_),_),_,_) :- !,fail.

obj_clause_trans(clause(Head,Body),clause(Head,NewBody),Module) :-
	set_fact(module(Module)),
	obj_clause_semantics(Head,Body,Module),
	static_objects_expansion(Module,Body,NewBody),
	!,
	( debug ->
	  (
	      inform_user(['-- expanded body of ',Head,':']),nl,
	      inform_user([NewBody]),nl,nl
	  ) ; true
	),
	true.

%%------------------------------------------------------------------------

obj_clause_semantics(Head,Body,Module) :-
	analyze(Module),
	c_itf_internal:defines_module(Base,Module),
	c_itf_internal:includes(Base,library(class)),
	functor(Head,F,A),
	atom_concat('obj$',_,F),
	arg(A,Head,LastArg),
	var(LastArg),
	!,
	compatible_with(LastArg,Module,[],StartingProp),
 	semantic_checking(Body,StartingProp,P),
	( debug ->
	  (
	      inform_user(['-- Properties from ',Head,' : ',P])
	  ) ; true
	),
	true.

obj_clause_semantics(Head,Body,Module) :-
	analyze(Module),
 	!,
 	semantic_checking(Body,[],P),
	( debug ->
	  (
	      inform_user(['-- Properties from ',Head,' : ',P])
	  ) ; true
	),
	true.

obj_clause_semantics(_,_,_).

%%------------------------------------------------------------------------
%%
%% GENERATE RUNTIME INFO ABOUT USED CLASSES
%%
%%------------------------------------------------------------------------

%generate_use_class_info(Module) :-
%        used_class(Module,Class),
%	add_clause(Module,'class$used'(Module,Class)),
%	fail.

%generate_use_class_info(_).

%%------------------------------------------------------------------------
%%
%% COLLECT OOP INFO ABOUT CLASSES AND ASCENDANCY (at second-pass)
%%
%%------------------------------------------------------------------------

:- redefining(generate_oop_info/1).

generate_oop_info(Module) :-
	used_class(Module,Class),
	class_itf:generate_oop_info(Class),
	fail.

generate_oop_info(Module) :-
	defines_module(Base,Module),
	includes(Base,library(class)),
	class_itf:generate_oop_info(Module),
	fail.

generate_oop_info(Module) :-
	used_class(Module,Class),
	public_pred(Class,method,F,A),
	implementation(Class,method,AtClass,F,A),
	\+ imports(Module,AtClass,F,A,AtClass),
	NewA is A+1,
	atom_concat('obj$',F,NewF),
	assertz_fact(imports(Module,AtClass,NewF,NewA,AtClass)),
	fail.

generate_oop_info(_).

%%------------------------------------------------------------------------
%%
%% ADDITIONAL DECLARATION CHECKING (at second-pass expansion)
%%
%%------------------------------------------------------------------------

% check whether used classes were class-expanded or interface-expanded

additional_itf_checking(Module) :- 
	used_class(Module,Class),
	defines_module(ClassBase,Class),
	( c_itf_internal:includes(ClassBase,library(class))     -> fail ; true ),
	( c_itf_internal:includes(ClassBase,library(interface)) -> fail ; true ),
	message(Module,error,
	['invalid use_class/1 declaration: ',Class,
         ' is not a class nor an interface']),
	retract_fact(used_class(Module,Class)),
	instance_of(Module,_,Cons),
	functor(Cons,Class,_),
	retract_fact(instance_of(Module,_,Cons)),
	fail.

% validate classes on new/2 declarations

additional_itf_checking(Module) :- 
	instance_of(Module,ID,Cons),
	functor(Cons,Class,_),
	( \+ defines_module(_,Class) ; \+ used_class(Module,Class) ),
	retract_fact(instance_of(Module,ID,Cons)),
	message(Module,error,['unknown class on ',ID,' instance declaration']),
	fail.


% check whether static instances ID's are Prolog modules or not

%additional_itf_checking(Module) :- 
%	instance_of(Module,ID,Cons),
%	defines_module(_,ID),
%	retract_fact(instance_of(Module,ID,Cons)),
%	message(Module, error,['instance identifier ',ID,
%	 ' is an existing Prolog module']),
%	fail.

% validate constructors on new/2 declarations

additional_itf_checking(Module) :- 
	instance_of(Module,ID,Cons),
	functor(Cons,Class,Arity),
	Arity > 0,
	defines_module(ClassBase,Class),
	\+ c_itf_internal:decl(ClassBase,method(Class/Arity)),
	retract_fact(instance_of(Module,ID,Cons)),
	message(Module,error,
	  ['unknown constructor on ',ID,' instance declaration']),
	fail.

additional_itf_checking(Module) :- 
	instance_of(Module,ID,Cons),
	functor(Cons,Class,0),
	defines_module(ClassBase,Class),
	c_itf_internal:decl(ClassBase,method(Class/A)),
	\+ c_itf_internal:decl(ClassBase,method(Class/0)),
	A > 0,
	retract_fact(instance_of(Module,ID,Cons)),
	message(Module, error,['constructor is needed on ',
	  ID,' instance declaration']),
	fail.

% Check uniform usage of static ID's at new/2 declarations.

additional_itf_checking(Module) :- 
	instance_of(Module,ID,Cons),
	decl(SomeBase,instance_of(ID,OtherCons)),
	defines_module(SomeBase,SomeModule),
	SomeModule \== Module,
	( OtherCons = Cons -> fail ; true ),
	retract_fact(instance_of(Module,ID,Cons)),
	message(Module,error,
	  ['static instance ',ID,' was derived from a different constructor',
	   ' at module ',SomeModule]),
	fail.

additional_itf_checking(_).

%%------------------------------------------------------------------------
%%
%% GENERATE STATIC INSTANCES (at second-pass expansion)
%%
%%------------------------------------------------------------------------

static_instance_definition(Module,Body) :-
	findall(
	   objects_rt:static_new(ID,Cons),
	   instance_of(Module,ID,Cons),
	   L),
	list_to_sequence(L,Body).

list_to_sequence([],true).

list_to_sequence([X|Nl],(X,Ns)) :-
	list_to_sequence(Nl,Ns).

%%------------------------------------------------------------------------
%%
%% MACROS
%%
%%------------------------------------------------------------------------

% derived_from(Module,ID,Class) :-
% 	instance_of(Module,ID,Cons),
% 	functor(Cons,Class,_).

%%------------------------------------------------------------------------

:- redefining(message/2).

message(Kind,Msg) :-
	module(M),
	message(M,Kind,Msg).

%%------------------------------------------------------------------------
%% Add new clauses on second-pass expansion
%%------------------------------------------------------------------------
/*
add_clause(Module,Head) :-
	defines_module(Base,Module),
	add_clause_of(
		Base,
		Head,
		true,
		_,
		'*object_expansion*',
		0,0),
	( debug -> (inform_user(['Added clause: ',Head])) ; true ),
	true.

add_clause_of(Base, Head, Body, VarNames, Source, Line0, Line1) :-
	assertz_fact(c_itf_internal:clause_of(Base, Head, Body, VarNames, 
                     Source, Line0, Line1)).
*/
