%%------------------------------------------------------------------------
%%
%% O'Ciao: Object Oriented Programming in Ciao/Prolog
%%
%% AUXILIARY FILE FOR
%% SOURCE-TO-SOURCE EXPANSION FOR OBJECT USAGE
%%
%% USAGE:
%% :- include(library(objects/objects_tr_aux)).
%%
%% AUTHOR : Angel Fernandez Pineda
%%
%% CLIP Laboratory - Technical University Of Madrid.
%%
%%         - Distributed under the Ciao Prolog license terms -
%%
%%------------------------------------------------------------------------
%% This file is used by objects_tr.pl in order to perform semantic analisys
%%------------------------------------------------------------------------

%%------------------------------------------------------------------------
%%
%% PERFORM CLAUSE BODY SEMANTIC CHECKING
%%
%%------------------------------------------------------------------------

semantic_checking(Var,InProp,InProp) :-
	var(Var),
	!.

semantic_checking(Body,InProp,OutProp) :-
	functor(Body,F,2),
	atom(F),
	( F=',' ; F='->' ),
	!,
	arg(1,Body,Left),
	arg(2,Body,Right),
	semantic_checking(Left,InProp,MidProp),
	semantic_checking(Right,MidProp,OutProp).

semantic_checking(if(C,T,E),InProp,InProp) :-
	!,
	semantic_checking(C,InProp,MidProp),
	semantic_checking(T,MidProp,_),
	semantic_checking(E,MidProp,_).

semantic_checking(Body,InProp,InProp) :-
	functor(Body,';',2),
	!,
	arg(1,Body,Left),
	arg(2,Body,Right),
	semantic_checking(Left,InProp,_),
	semantic_checking(Right,InProp,_).

semantic_checking(call(Goal),InProp,OutProp) :-
	!,
	semantic_checking(Goal,InProp,OutProp).

semantic_checking(Goal,InProp,OutProp) :-
	goal_checking(Goal,InProp,OutProp),
	!,
	arg_checking(Goal,InProp).

semantic_checking(Goal,InProp,InProp) :-
	!,
	arg_checking(Goal,InProp).

semantic_checking(_,Prop,Prop).

%%------------------------------------------------------------------------
%%
%% SIMPLE GOAL CHECKING
%%
%%------------------------------------------------------------------------
	
goal_checking(Var,_,_) :-
	var(Var),
	!,
	fail.

goal_checking(Objects_rt:X,InProp,OutProp) :-
	nonvar(Objects_rt),
	Objects_rt = objects_rt,
	!,
	goal_checking(X,InProp,OutProp).

%%------------------------------------------------------------------------
%% new/2
%%------------------------------------------------------------------------

%% invalid call to new/2

goal_checking(new(Var,Cons),_,_) :-
	nonvar(Var),
	!,
	message(error,
	 ['invalid first argument in call to ',new(Var,Cons)]),
	fail.

goal_checking(new(_,Cons),_,_) :-
	var(Cons),
	!,
	fail.

%% Instance is known to be derived from any other class

goal_checking(new(Var,Cons),InProp,_) :-
	check_property(prop(Var,_),InProp),
	!,
	message(warning,
	 ['invalid call to new(?,',Cons,
	  ') : first argument will not be a free variable']),
	fail.

%% Given constructor does not correspond to any used class

goal_checking(new(_,Cons),_,_) :-
	functor(Cons,Class,_),
	module(Module),
	\+ used_class(Module,Class),
	!,
	message(error,
	 ['unkown class in call to new(?,',Cons,')']),
	fail.

goal_checking(new(_,Cons),_,_) :-
	functor(Cons,Class,_),
	module(Module),
	used_class(Module,Class),
	defines_module(Base,Class),
	c_itf_internal:includes(Base,library(interface)),
	!,
	message(error,
	 ['can not create an instance from an interface: new(?,',Cons,')']),
	fail.

%% Given constructor is not defined at given class

goal_checking(new(_,Cons),_,_) :-
	functor(Cons,Class,0),
	defines_module(Base,Class),
	\+ c_itf_internal:decl(Base,method(Class/0)),
	c_itf_internal:decl(Base,method(Class/_)),
	!,
	message(error,
	 ['unkown constructor in call to new(?,',Cons,')']),
	fail.

goal_checking(new(_,Cons),_,_) :-
	functor(Cons,Class,Arity),
	Arity > 0,
	defines_module(Base,Class),
	\+ c_itf_internal:decl(Base,method(Class/Arity)),
	!,
	message(error,
	 ['unkown constructor in call to new(?,',Cons,')']),
	fail.

%% Everything allright about this stuff

goal_checking(new(Var,Cons),InProp,OutProp) :-
	functor(Cons,Class,_),
	!,
	compatible_with(Var,Class,InProp,OutProp).

%%------------------------------------------------------------------------
%% interface/2 , instance_of/2 , derived_from/2.
%%------------------------------------------------------------------------

goal_checking(TypeCheck,InProp,OutProp) :-
	nonvar(TypeCheck),
	functor(TypeCheck,F,2),
	( F = instance_of ; F = derived_from ; F = interface ),
	arg(1,TypeCheck,Var),
	arg(2,TypeCheck,Itf),
	atom(Itf),
	var(Var),
	!,
	compatible_with(Var,Itf,InProp,OutProp).

%%------------------------------------------------------------------------
%% method callling
%%------------------------------------------------------------------------

% goal unknown at compile time

goal_checking(Var:_,_,_) :-
	nonvar(Var),
	!,
	fail.

goal_checking(_:Method,_,_) :-
	var(Method),
	!,
	fail.

% Instance is known to be compatible with one or more interfaces, but
% the called method is not public at any of those interfaces.

goal_checking(Var:Method,Prop,_) :-
	var(Var),
	functor(Method,F,A),
	check_property(prop(Var,interface(Itf)),Prop),
	accessible(Itf,F,A),
	!,
	fail.

goal_checking(Var:Method,Prop,_) :-
	var(Var),
	check_property(prop(Var,interface(_)),Prop),
	!,
	message(error,
	['call to non-public ',Method]),
	fail.

% Instance is not known to be compatible with any interface,
% but, the given method is not public at any used class.

goal_checking(Var:Method,_,_) :-
	var(Var),
	functor(Method,F,A),
	module(Module),
	( Class=Module % might be a self-call
	; used_class(Module,Class) ),
	accessible(Class,F,A),
	!,
	fail.

goal_checking(Var:Method,_,_) :-
	var(Var),
	!,
	message(warning,
	 ['called ',Method,' is not public at any used class']),
	fail.

%%------------------------------------------------------------------------
%%
%% PERFORM GOAL ARGUMENT CHECKING
%%
%%------------------------------------------------------------------------

arg_checking(Var,_) :-
	var(Var),
	!.

arg_checking(Module:Goal,_) :-
	( var(Module) ; var(Goal) ),
	!.

arg_checking(\+ Goal,InProp) :-
	!,
	arg_checking(Goal,InProp).

arg_checking(Body,InProp) :-
	( functor(Body,',',2) ; functor(Body,'->',2) ; 
	  functor(Body,';',2)
	),
	!,
	arg(1,Body,Left),
	arg(2,Body,Right),
	arg_checking(Left,InProp),
	arg_checking(Right,InProp).

arg_checking(if(C,T,E),InProp) :-
	!,
	arg_checking(C,InProp),
	arg_checking(T,MidProp),
	arg_checking(E,MidProp).


% Meta-predicate

arg_checking(Goal,InProp) :-
	functor(Goal,Functor,Arity),
	Arity > 0,
	functor(Meta,Functor,Arity),
	module(Module),
	imports_meta_pred(Module,Meta,_AtModule),
	!,
	Goal =.. [_|Args],
	Meta =.. [_|MetaSpec],
	arg_chk_aux(MetaSpec,Args,InProp).

arg_checking(_,_).

%%------------------------------------------------------------------------

arg_chk_aux([],[],_).

arg_chk_aux([goal|_],[Arg|_],Prop) :-
	semantic_checking(Arg,Prop,_),
	fail.

arg_chk_aux([fact|_],[(Module:Fact)|_],Prop) :-
	var(Module),
	nonvar(Fact),
	goal_checking(Module:Fact,Prop,_),
	fail.

arg_chk_aux([_|Ns],[_|Na],Prop) :-
	!,
	arg_chk_aux(Ns,Na,Prop).

%%------------------------------------------------------------------------
%% ADD PROPERTIES FOR A VARIABLE
%%------------------------------------------------------------------------

compatible_with(Var,Class,InProp,OutProp) :-
	var(Var),
	atom(Class),
	findall(interface(Itf),impl_interface(Class,Itf),NewPropAux),
	compatible_with_aux(Var,NewPropAux,NewProp),
	append(InProp,NewProp,OutProp).

compatible_with_aux(_,[],[]).
compatible_with_aux(Var,[Prop|Np],[prop(Var,Prop)|Npp]) :-
	compatible_with_aux(Var,Np,Npp).

%%------------------------------------------------------------------------
%% PROPERTY CHECKING FOR A VARIABLE 
%%------------------------------------------------------------------------

check_property(prop(Var,P),[prop(A_Var,_)|Np]) :-
	var(Var),
	var(A_Var),
	Var \== A_Var,
	!,
	check_property(prop(Var,P),Np).

check_property(prop(X,P),[prop(X,P)|_]) :- 
	var(X).

check_property(prop(Var,P),[_|Np]) :-
	!,
	check_property(prop(Var,P),Np).

%%------------------------------------------------------------------------
%% PREDICATE IS ACCESSIBLE
%%------------------------------------------------------------------------

accessible(AtClass,_,_) :-
	module(AtClass),
	defines_module(Base,AtClass),
	includes(Base,library(class)),
	!.

accessible(AtClass,F,A) :-
	public_pred(AtClass,_,F,A),
	!.

accessible(AtClass,F,A) :-
	module(CurrentClass),
	implementation(AtClass,_,CurrentClass,F,A).
