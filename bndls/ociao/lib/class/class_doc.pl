%%------------------------------------------------------------------------
%%
%% O'Ciao: Object Oriented Programming in Ciao/Prolog
%%
%% DOCUMENTATION FILE ON CLASS SYNTAX
%%
%% AUTHOR : Angel Fernandez Pineda
%%
%% CLIP Laboratory - Technical University Of Madrid.
%%
%%         - Distributed under the Ciao Prolog license terms -
%%
%%------------------------------------------------------------------------

:- use_package(assertions).

:- doc(nodoc,assertions).

:- doc(filetype,package).

:- use_module(library(objects/objects_rt), 
	[
	    constructor/1,
	    class_name/1,
	    interface_name/1,
	    instance_id/1,
	    class_source/1,
	    interface_source/1,
	    method_spec/1,
	    virtual_method_spec/1
	]).

%% Error reporting appendix:

:- include(library(class/class_error_doc)).

%%------------------------------------------------------------------------
%%
%% MANUAL GENERAL HEADERS
%%
%%------------------------------------------------------------------------

:- doc(title, "Declaring classes and interfaces").

:- doc(author,"Angel Fernandez Pineda").

:- doc(copyright,"@include{DocCopyright.lpdoc}").

:- doc(summary,
	"This section will explain how to declare a class/interface 
         using O'Ciao.").

:- doc(module,
	"O'Ciao classes are declared in the same way as traditional
         prolog modules. The general mechanism of @em{source expansion}
         will translate object-oriented declarations to normal prolog
         code. This is done transparently to the user.

         Abstract @index{interfaces} are restricted classes which
         declare exported predicates with no implementation. The implementation
         itselt will be provided by some class using an @decl{implements/1}
         declaration. Only @decl{export/1} and @decl{data/1} declarations are
         allowed when declaring an interface. Normal classes may treated 
         as interfaces just ignoring all exported predicate implementations.
        ").

:- doc(usage,
	"To declare a class the compiler must be told to use the @tt{class}
         @em{source expansion}. To do so, source code must start with a module
         declaration which loads the class package:
@begin{verbatim} 
           :- class(ClassName).
@end{verbatim} 
         @noindent or a @decl{module/3} declaration, as follows:      
@begin{verbatim} 
           :- module(ClassName,[],[class]).
@end{verbatim} 

         @noindent @concept{interfaces} are declared in a similar way:
@begin{verbatim} 
           :- interface(InterfaceName).
@end{verbatim} 
         
         Please, do not use SICStus-like module declaration, with a non-empty 
         export list. In other case, some non-sense errors will be 
         reported by normal Ciao module system.

         Most of the regular Ciao declarations may be used when defining
         a class, such as @decl{concurrent/1},@decl{dynamic/1},
         @decl{discontiguous/1},@decl{multifile/1}, and so on.

         However, there are some restrictions wich apply 
         to those declarations:
          @begin{itemize} 
           @item @decl{meta_predicate/1} declaration is not allowed to hold
                 @index{addmodule and pred(N) meta-arguments}, 
                 except for previously declared multifiles.
           @item Attribute and multifile predicates must be declared 
                 before any clause of the related predicate.
           @item There is no sense in declaring an attribute as meta_predicate.
          @end{itemize} 

         It is a good practique to put all your declarations at the
         very begining of the file, just before the code itself.
        ").

%%------------------------------------------------------------------------
%%
%% DOC ON SOME TYPES
%%
%%------------------------------------------------------------------------


%%------------------------------------------------------------------------
%%
%% CLASS DECLARATION DOC
%%
%%------------------------------------------------------------------------

%:- doc(class/1,
%	"If you are so old fashioned, you may use a normal module declaration
%         as follows:
%    
%         @tt{:- module(ClassName,[],[class]).}
%
%         But class/1 declaration is a better fashioned way in order to
%         declare a class :
%
%         :- class(ClassName).
%        ").

%:- decl class(ClassName) :
%	(class_name(ClassName)) #
%	"Declares current source to be a class.".

%class(_).

%%------------------------------------------------------------------------
%%
%% PUBLIC METHOD DECLARATION
%%
%%------------------------------------------------------------------------

:- doc(export/1,
	"Declares a method or attribute to be part of the 
          @index{public interface}. 

         The public interface is the set of predicates wich will be accesible
         from any code establishing an usage relationship with this class
         (see @decl{use_class/1} for further information).

         Publishing an attribute or method is very similar to 
         @em{exporting} a predicate in a Prolog module.

         Whether an inherited and exported predicate is @concept{overriden}, 
	 it must be explicitly exported again.

         An inherited (but not exported) predicate may become exported,
         without overriding it by the usage of this declaration.
        ").

:- decl export(Spec) : (method_spec(Spec)) #
	"@var{Spec} will be part of the public (exported) interface.".

%:- doc(doinclude,export/1).

%export(_).

%%------------------------------------------------------------------------

:- doc(public/1,
	"Just an alias for @decl{export/1}.").

:- decl public(Spec) : (method_spec(Spec)) #
	"This declaration may be used instead of @decl{export/1}.".

%:- doc(doinclude,public/1).

%public(_).

%%------------------------------------------------------------------------
%%
%% INHERITABLE METHOD DECLARATION
%%
%%------------------------------------------------------------------------

:- doc(inheritable/1,
	"Declares a method or attribute to be inherited by descendant classes.
         Notice that all @bf{public predicates are inheritable by default}.
         There is no need to mark them as inheritable.

         Traditionaly, object oriented languages makes use of the
         @index{protected} concept. Inheritable/1 may be used
         as the same concept.

         The set of inheritable predicates is called the 
         @index{inheritable interface}.
        ").

:- decl inheritable(MethodSpec) : (method_spec(MethodSpec)) #
	"@var{MethodSpec} is accessible to descendant classes.".

%:- doc(doinclude,inheritable/1).

%inheritable(_).

%%------------------------------------------------------------------------
%%
%% ATTRIBUTE DECLARATION
%%
%%------------------------------------------------------------------------

:- doc(data/1,
	"Declares an @index{attribute} at current class. Attributes 
         are used to build the internal state of instances. So, each
         instance will own a particular copy of those attribute definitions.
         In this way, one instance may have different state from another.

         O'Ciao attributes are restricted to hold simple facts. It is not
         possible to hold a Head :- Body clause at an instance attribute.

         Notice that attributes are @index{multi-evaluated} by nature,
         and may be manipulated by the habitual @bf{assert/retract} family 
         of predicates.

         Attributes may also be initialized. In order to do so, simply
         put some clauses after the attribute definition. Each time an
         instance is created, its initial state will be built from those
         @index{initialization clauses}.

         Note: whether a data/1 declaration appears inside an interface, 
         it will be automatically exported.
        ").

:- decl data(Spec) : (method_spec(Spec)) #
	"@var{Spec} is an attribute.".

%:- doc(doinclude,data/1).

%data(_).

%%------------------------------------------------------------------------

:- doc(dynamic/1,
	"Just an alias for @decl{data/1}.
        ").

:- decl dynamic(Spec) : (method_spec(Spec)) #
	"You may use this declaration instead of @decl{data/1}.".

%:- doc(doinclude,dynamic/1).

%dynamic(_).

%%------------------------------------------------------------------------

:- doc(concurrent/1,
	"Declares a @index{concurrent attribute} at current class. 
         Concurrent attributes are just the same as normal attributes,
         those declared using @decl{data/1}, except for they may freeze
         the calling thread instead of failing when no more choice points
         are remaining on the concurrent attribute.

         In order to get more information about concurrent behavior take
         a look to the concurrent/1 built-in declaration on 
         Ciao Prolog module system.
        ").

:- decl concurrent(Spec) : (method_spec(Spec)) #
	"Declares @var{Spec} to be a concurrent attribute.".

%:- doc(doinclude,concurrent/1).

%concurrent(_).

%%------------------------------------------------------------------------
%%
%% INHERITANCE DECLARATION
%%
%%------------------------------------------------------------------------

:- doc(inherit_class/1,
	"Makes any public and/or inheritable predicate at inherited class 
         to become accesible by any instance derived from current class.

         Inherited class is also called the @index{super class}.

         Only one inherit_class/1 declaration is allowed to be present
         at current source.

         Notice that inheritance is @concept{public} by default. Any
         public and/or inheritable declaration will remain the same to
         descendant classes. However, any inherited predicate may be
         @em{overriden} (redefined).

         A predicate is said to be @index{overriden} when 
         it has been inherited from super class, 
         but there are clauses (or a @decl{data/1} declaration)
         present at current class for such a predicate.

         Whether a @bf{public} predicate is overriden,
         the local definition must also be exported, otherwise an error
         is reported.

         Whether an @bf{inheritable} predicate (not public) is overriden,
         the local definition must also be marked as inheritable or exported, 
         otherwise an error is also reported.

        Note: whether inherit_class/1 appears inside an interface, it will
        be used as an @decl{implements/1} declaration.
        ").

:- decl inherit_class(Source) : (class_source(Source)) #
	"Establish an @index{inheritance relationship} between current
         class and the class defined at @var{Source} file.".

%:- doc(doinclude,inherit_class/1).

%inherit_class(_).

%%------------------------------------------------------------------------

:- doc(implements/1,
	"Forces current source to provide an implementation for the given 
         interface file. Such interface file may declare another class 
         or a specific interface.

         Every public predicate present at given interface file will
         be automatically declared as public at current source, 
         so you @bf{must} 
         provide an implementation for such predicates.

         The effect of this declaration is called @index{interface
         inheritance},and there is no restriction on the number of
         implements/1 declarations present at current code.
        ").

:- decl implements(Interface) : 
	interface_source(Interface) #
       "Current source is supposed to provide an implementation for 
        @var{Interface}.".

%:- doc(doinclude,implements/1).

%implements(_).

%%------------------------------------------------------------------------
%%
%% VIRTUAL METHOD DECL
%%
%%------------------------------------------------------------------------

:- doc(virtual/1,
	"This declaration may be used whenever descendant classes 
         are to implement different versions of a given predicate.

         @index{virtual} predicates give a chance to handle, in an 
         uniform way, different implementations of the same functionality.

         Whether a virtual predicate is declared as a method,
	 there must be at least one clause of it present at current source. 
         Whenever no special implementation is needed at current class,
         a never-fail/allways-fail clause may be defined 
         (depending on your needs). For example:

@begin{verbatim}
   :- virtual([ test1/1 , test2/2 ]).
   test1(_).
   test2(_,_) :- fail.
@end{verbatim}

         This kind of virtual methods are also known as 
         @index{abstract methods}, since implementation is fully delegated
         to descendant classes.
         
         An attribute may be also declared as a virtual one, but there is 
         no need to write clauses for it.
         ").

:- decl virtual(VirtualMethodSpec) : 
	(virtual_method_spec(VirtualMethodSpec)) #
	"All calls to @var{VirtualMethodSpec} predicate in current source
         will use the most descendant implementation of it.".

%:- doc(doinclude,virtual/1).

%virtual(_).

%%------------------------------------------------------------------------
%%
%% INHERITED PREDICATE CALLING
%%
%%------------------------------------------------------------------------

:- doc(inherited/1,
	"This predicate qualificator may be used whenever you need 
         to reference an attribute or method on the super class. 
         
         Since methods and attributes may be @concept{redefined}, this
         qualificator is need to distinguish between a locally declared
         predicate and the inherited one, which has the same name.

         There is no need to use inherited/1 if a particular inherited 
         predicate has not been redefined at current class.
        ").

:- pred inherited(Goal) : callable #
	"References a given @var{Goal} at the super class".

inherited(_).

%%------------------------------------------------------------------------
%%
%% RETRIEVING SELF INSTANCE ID
%%
%%------------------------------------------------------------------------

:- doc(self/1,
	"Determines which instance is currently executing self/1 goal.

         Predicate will fail if argument is not a free variable.
         Otherwise, it will allways succeed, retrieving the 
         instance identifier which is executing current code.

         This functionality is very usefull since an object must 
         have knowledge of other object's identifier in order to
         send messages to it.For example:

         :- concurrent ack/0.

         send_data_to_object(Data,Obj) :-
                self(X),
                Obj:take_this(Data,X),
		current_fact(ack).

         acknowledge :-
                asserta_fact(ack).  

         take_this(Data,Sender) :-
                validate_data(Data),
		Sender:acknowledge.
        ").

:- pred self(Variable) : (var(Variable)) => instance_id(Variable) #
	"Retrieves current instance identifier in @var{Variable}".

self(_).

%%------------------------------------------------------------------------
%%
%% CONSTRUCTOR DECLARATIONS
%%
%%------------------------------------------------------------------------

:- doc(constructor/0,
	"A @index{constructor} is a special case of method which 
         complains the following conditions:

          @begin{itemize}
           @item The constructor functor matches the current class name.
           @item A constructor may hold any number of arguments.
           @item If an inheritance relationship was defined, an inherited
                 constructor must be manually called (see below).
           @item When instance creation takes place, any of the declared 
                 constructors are implicitly called. The actual constructor
                 called depends on the @pred{new/2} goal specified by the
                 user.
          @end{itemize}
 
        This is a simple example of constructor declaration for the foo class:
         
        @begin{verbatim}
           foo :- 
               display('an instance was born').
@end{verbatim}

        Constructor declaration is not mandatory, and there may be more
        than one constructor declarations (with different arity) at the 
        source code.

        This functionality is usefull when some computation is needed at
        instance creation. For example: opening a socket, clearing the screen,
        etc.

        Whenever an inheritance relationship is established, and there is
        any constructor defined at the super class, you must call manually an
        inherited constructor. Here is an example:

        @begin{verbatim}
           :- class(foo).
           :- inherit_class(myclass).

           foo :-
               myclass(0),
               display('an instance was born').

           foo(N) :- myclass(N).
@end{verbatim}

        Consequences may be unpredictable, if you forget to call an inherited
        constructor. You should also take care not to call an inherited 
        constructor twice.

        All defined constructors are inheritable by default.
        A constructor may also be declared as public (by the user), but it
        is not mandatory.
       ").

:- pred constructor #
	"Constructors are implicitly declared".

constructor.

%%------------------------------------------------------------------------
%%
%% DESTRUCTOR DECLARATIONS
%%
%%------------------------------------------------------------------------

:- doc(destructor/0,
	"A @index{destructor} is a special case of method which 
         will be automatically called when instance destruction takes place.

         A destructor will never be wanted to be part of the public
         interface, and there is no need to mark them as inheritable,
         since all inherited destructors are called by O'Ciao just before 
         yours.

         This is a simple example of destructor declaration:
         
        @begin{verbatim}
           destructor :- 
               display('goodbye, cruel world!!!').
@end{verbatim}

        Destructor declaration is not mandatory. Failure or sucess of 
        destructors will be ignored by O'Ciao, and they will be called
        only once.

        This functionality is useful when some computation is need at
        instance destruction. For example: closing an open file.
       ").

:- pred destructor #
	"Destructors are implicitly declared".

destructor.

%%------------------------------------------------------------------------
%% BUGS / TO IMPLEMENT IN THE FUTURE
%%------------------------------------------------------------------------

:- doc(bug,
	"addmodule and pred(N) meta-arguments are not allowed on 
         meta-predicates.
	").
