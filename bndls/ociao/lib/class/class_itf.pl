%%------------------------------------------------------------------------
%%
%% O'Ciao: Object Oriented Programming in Ciao/Prolog
%%
%% OBJECT ORIENTED DECLARATION INTERFACING
%%
%% AUTHOR : Angel Fernandez Pineda
%% DATE   : July 1999
%%
%%------------------------------------------------------------------------

:- module(class_itf,
	[
	    class_from_base/2,
	    attribute_set/4,
	    implementation/5,
	    inherited_pred/5,
	    public_pred/4,
	    inheritable_pred/4,
	    virtual_pred/4,
	    impl_interface/2,
	    cleanup_class_info/1,
	    generate_oop_info/1
	]).

:- use_module(library(compiler/c_itf_internal)).
:- use_module(library(pathnames), [path_basename/2]).

%%------------------------------------------------------------------------

:- data current_class/1.       % CLASS/INTERFACE info has been generated.

:- data attribute_set/4.       % CLASS knows that CLASS_OR_ASCENDANT
                               % declares F/A as attribute.

:- data implementation/5.      % CLASS defines/inherits an implementation of
                               % WHAT (method/attribute) at CLASS_OR_ASCENDANT.

:- data inherited_pred/5.      % CLASS declares KIND (method/attribute),
                               % as an inherited predicate from ascendants.

:- data public_pred/4.         % CLASS declares KIND (method/attribute),
                               % as public.

:- data inheritable_pred/4.    % CLASS declares KIND (method/attribute),
                               % as inheritable.

:- data virtual_pred/4.        % CLASS declares KIND (method/attribute),
                               % as virtual.

:- data impl_interface/2.      % CLASS has implemented INTERFACE.

:- data debug/0.

% debug.

%%------------------------------------------------------------------------

% INITIALIZATION: Must be called before expanding the involved class.

cleanup_class_info(Module) :-
	retractall_fact(attribute_set(Module,_,_,_)),
	retractall_fact(implementation(Module,_,_,_,_)),
	retractall_fact(inherited_pred(Module,_,_,_,_)),
	retractall_fact(public_pred(Module,_,_,_)),
	retractall_fact(virtual_pred(Module,_,_,_)),
	retractall_fact(impl_interface(Module,_)),
	retractall_fact(current_class(Module)).

% INITIALIZATION

generate_oop_info(Module) :-
	current_class(Module),   % already generated.
	!.

generate_oop_info(Module) :-
	assertz_fact(current_class(Module)),
	fail.

% GENERATE ASCENDANCY AND ABSTRACT INTERFACE INFO

generate_oop_info(Module) :-
	assertz_fact(impl_interface(Module,Module)),
	( debug -> 
	  inform_user([Module,' ABSTRACT ITF ',Module])
	;
	  true
	),
	fail.

generate_oop_info(Module) :-
	super(Module,Super),
	generate_oop_info(Super),
	impl_interface(Super,Itf),
	\+ impl_interface(Module,Itf),
	assertz_fact(impl_interface(Module,Itf)),
	( debug -> 
	  inform_user([Module,' ABSTRACT ITF ',Itf])
	;
	  true
	),
	fail.

generate_oop_info(Module) :-
	defines_module(Base,Module),
	c_itf_internal:decl(Base,implements(Itf)),
	generate_oop_info(Itf),
	\+ impl_interface(Module,Itf),
	assertz_fact(impl_interface(Module,Itf)),
	( debug -> 
	  inform_user([Module,' ABSTRACT ITF ',Itf])
	;
	  true
	),
	impl_interface(Itf,OtherItf),
	generate_oop_info(Itf),
	\+ impl_interface(Module,OtherItf),
	assertz_fact(impl_interface(Module,OtherItf)),
	( debug -> 
	  inform_user([Module,' ABSTRACT ITF ',OtherItf])
	;
	  true
	),
	fail.

% GENERATE ATTRIBUTE SET INFO

generate_oop_info(Module) :-
	defines_module(Base,Module),
	c_itf_internal:decl(Base,attribute(F/A)),
	assertz_fact(attribute_set(Module,Module,F,A)),
	( debug -> 
	  inform_user([Module,' ATTR SET ',Module,':',F,'/',A]) 
	;
	  true
	),
	fail.

generate_oop_info(Module) :-
	super(Module,Super),
	attribute_set(Super,At,F,A),
	assertz_fact(attribute_set(Module,At,F,A)),
	( debug -> 
	  inform_user([Module,' ATTR SET ',At,':',F,'/',A]) 
	;
	  true
	),
	fail.

% GENERATE IMPLEMENTATION INFO

generate_oop_info(Module) :-
	defines_module(Base,Module),
	c_itf_internal:decl(Base,method(F/A)),
	assertz_fact(implementation(Module,method,Module,F,A)),
	( debug -> 
	  inform_user([Module,' IMPL ',Module,':',F,'/',A,' (method)']) 
	;
	  true
	),
	fail.

generate_oop_info(Module) :-
	defines_module(Base,Module),
	c_itf_internal:decl(Base,attribute(F/A)),
	assertz_fact(implementation(Module,attribute,Module,F,A)),
	( debug -> 
	  inform_user([Module,' IMPL ',Module,':',F,'/',A,' (attribute)']) 
	;
	  true
	),
	fail.

generate_oop_info(Module) :-
	super(Module,Super),
	implementation(Super,Kind,At,F,A),
	\+ implementation(Module,_,_,F,A),
	assertz_fact(implementation(Module,Kind,At,F,A)),
	( debug -> 
	  inform_user([Module,' IMPL ',At,':',F,'/',A,' (',Kind,')']) 
	;
	  true
	),
	fail.

% GENERATE INHERITED PREDICATE INFO

generate_oop_info(Module) :-
	super(Module,Super),
	defines_module(SuperBase,Super),
	c_itf_internal:decl(SuperBase,inheritable(F/A)),
	( c_itf_internal:decl(SuperBase,method(F/A)) -> 
	  Kind = method
	; 
	  ( c_itf_internal:decl(SuperBase,attribute(F/A)) -> Kind = attribute )
	),
	assertz_fact(inherited_pred(Module,Kind,Super,F,A)),
	( debug -> 
	  inform_user([Module,' INH ',Super,':',F,'/',A,' (',Kind,')']) 
	;
	  true
	),
	fail.

generate_oop_info(Module) :-
	super(Module,Super),
	inherited_pred(Super,Kind,At,F,A),
	\+ inherited_pred(Module,_,_,F,A),
	assertz_fact(inherited_pred(Module,Kind,At,F,A)),
	( debug -> 
	  inform_user([Module,' INH ',At,':',F,'/',A,' (attribute)']) 
	;
	  true
	),
	fail.

% GENERATE PUBLIC PREDICATE INFO

generate_oop_info(Module) :-
	impl_interface(Module,Itf),
	defines_module(ItfBase,Itf),
	c_itf_internal:decl(ItfBase,public(F/A)),
	( c_itf_internal:decl(ItfBase,method(F/A)) ->
	  Kind = method
	; 
	  ( c_itf_internal:decl(ItfBase,attribute(F/A)) -> 
	    Kind = attribute 
	  ;
	    inherited_pred(Module,Kind,_,F,A)
	  )
	),
	\+ public_pred(Module,_,F,A),
	assertz_fact(public_pred(Module,Kind,F,A)),
	( debug -> 
	  inform_user([Module,' PUBLIC ',F,'/',A,' (',Kind,')']) 
	;
	  true
	),
	fail.

% GENERATE INHERITABLE PREDICATE INFO

generate_oop_info(Module) :-
	impl_interface(Module,Itf),
	defines_module(ItfBase,Itf),
	c_itf_internal:decl(ItfBase,inheritable(F/A)),
	( c_itf_internal:decl(ItfBase,method(F/A)) ->
	  Kind = method
	; 
	  ( c_itf_internal:decl(ItfBase,attribute(F/A)) -> 
	    Kind = attribute 
	  ;
	    inherited_pred(Module,Kind,_,F,A)
	  )
	),
	\+ public_pred(Module,_,F,A),
	assertz_fact(inheritable_pred(Module,Kind,F,A)),
	( debug -> 
	  inform_user([Module,' INHERITABLE ',F,'/',A,' (',Kind,')']) 
	;
	  true
	),
	fail.

% GENERATE VIRTUAL PREDICATE INFO

generate_oop_info(Module) :-
	defines_module(Base,Module),
	c_itf_internal:decl(Base,virtual(F/A)),
	( c_itf_internal:decl(Base,method(F/A)) ->
	  Kind = method
	; 
	  ( c_itf_internal:decl(Base,attribute(F/A)) -> 
	    Kind = attribute 
	  ;
	    inherited_pred(Module,Kind,_,F,A)
	  )
	),
	assertz_fact(virtual_pred(Module,Kind,F,A)),
	fail.

generate_oop_info(_).

%%------------------------------------------------------------------------

super(Module,Super) :-
	defines_module(Base,Module),
	c_itf_internal:decl(Base,super(Super)).

%%------------------------------------------------------------------------
%% Convert Class source file to Class name and vice-versa
%%------------------------------------------------------------------------

% TODO: merge with flat_nested_spec/2, etc.
class_from_base(Base,Class) :-
	functor(Base,_,1),
	arg(1,Base,Path),
	!,
	class_from_base(Path,Class).
class_from_base(Base,Class) :-
	nonvar(Base), Base = (_/Class0),
	!,
	Class = Class0.
class_from_base(Base,Class) :-
	atom(Base),
	module_from_base(Base,Class).
