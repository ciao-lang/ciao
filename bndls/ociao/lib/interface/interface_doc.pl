%%------------------------------------------------------------------------
%%
%% O'CIAO : OBJECT ORIENTED PROGRAMMING IN CIAO/Prolog
%%
%% DOCUMENTATION FILE ON CLASS ABSTRACT INTERFACES
%%
%% AUTHOR : Angel Fernandez Pineda
%% DATE   : July 1999
%%
%%------------------------------------------------------------------------

:- module(interface_doc,[],[assertions]).

%%------------------------------------------------------------------------
%%
%% MANUAL GENERAL HEADERS
%%
%%------------------------------------------------------------------------

:- doc(title,"Declaring abstract interfaces for classes").

:- doc(author,"Angel Fernandez Pineda").

:- doc(copyright,"@include{DocCopyright.lpdoc}").

:- doc(module,
	 "O'CIAO abstract interfaces are simple modules which declares exported
          predicates with no implementation. The implementation itself
          will be provided by some class using an @decl{implements/1}
          declaration.

          O'CIAO classes may be also treated as interfaces just ignoring
          all exported predicate implementation.
 
          In order to get information about error reporting, consult
          the @lib{class_doc} chapter on this documentation.
         ").

:- doc(usage,
	"To declare an interface, the @em{interface source expansion} package
         must be loaded:
@begin{verbatim} 
           :- interface(ItfName).
@end{verbatim} 
         @noindent or using a @decl{module/3} declaration, as follows:      
@begin{verbatim} 
           :- module(ItfName,[],[interface]).
@end{verbatim} 
         Note: interfaces does not declare any code, so there is no need to
         load them from the CIAO toplevel shell.
        ").

%%------------------------------------------------------------------------

:- prop interface_source(Source) #
	"@var{Source} is a valid path to a prolog file containing 
         a class declaration or an interface declaration
         (without .pl extension).".

interface_source(_).

:- prop method_spec(Spec) #
	"@var{Spec} is a method or attribute specification.".

method_spec(F/A) :-
	atom(F),
	integer(A),
	A >= 0.

%%------------------------------------------------------------------------
%%
%% PUBLIC METHOD DECLARATION
%%
%%------------------------------------------------------------------------

:- doc(export/1,
	"Declares a method or attribute to be part of the 
          @concept{public interface}. 

         There is no need to declare clauses for the exported predicate,
         so it will be assumed to be a method unless a @decl{data/1}
         declaration is also present at current source.
        ").

:- decl export(Spec) : (method_spec(Spec)) #
	"@var{Spec} will be part of the public (exported) interface.".

export(_).

%%------------------------------------------------------------------------

:- doc(public/1,
	"Just an alias for @decl{export/1}.").

:- decl public(Spec) : (method_spec(Spec)) #
	"This declaration may be used instead of @decl{export/1}.".

public(_).

%%------------------------------------------------------------------------
%%
%% ATTRIBUTE DECLARATION
%%
%%------------------------------------------------------------------------

:- doc(data/1,
	"Declares an @concept{attribute} to be implemented by any code
         using this interface. Attribute is @bf{automatically exported}
         by default, but an @decl{export/1} declaration may be used.
        ").

:- decl data(Spec) : (method_spec(Spec)) #
	"@var{Spec} is an attribute.".

data(_).

%%------------------------------------------------------------------------

:- doc(dynamic/1,
	"Just an alias for @decl{data/1}.
        ").

:- decl dynamic(Spec) : (method_spec(Spec)) #
	"You may use this declaration instead of @decl{data/1}.".

dynamic(_).

%%------------------------------------------------------------------------
%%
%% INHERITANCE DECLARATION
%%
%%------------------------------------------------------------------------

:- doc(implements/1,
	"Declares another interface to be also implemented by any code
         using this interface. This declaration has the same behavior as
         @em{implements/1} declaration described at @lib{class_doc}
         library.
        ").

:- decl implements(Interface) : 
	interface_source(Interface) #
       "@var{Interface} must alse be implemented.".

implements(_).


%%------------------------------------------------------------------------

:- doc(inherit_class/1,
	"In the interface context, inherit_class/1 has the same effect
         as implements/1").

:- decl inherit_class(Source) : (interface_source(Source)) #
	"Just an alias for @decl{implements/1} declaration.".

inherit_class(_).
