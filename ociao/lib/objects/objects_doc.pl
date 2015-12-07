%%------------------------------------------------------------------------
%%
%% O'Ciao: Object Oriented Programming in Ciao/Prolog
%%
%% DOCUMENTATION FILE ON OBJECT MANIPULATION
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

:- use_module(library(objects/objects_rt), 
	[
	    instance_id/1,
	    constructor/1,
	    class_source/1,
	    class_name/1
	]).

%%------------------------------------------------------------------------

:- doc(title, "Compile-time usage of objects").

:- doc(author,"Angel Fernandez Pineda").

:- doc(copyright,"@include{DocCopyright.lpdoc}").

:- doc(usage,
	"Any code which needs to use objects must include the
         objects package:
@begin{verbatim}
    :- module(@em{ModuleName},@em{Exports},[objects]).
@end{verbatim}
         You can use objects even if your code is a class. Note that 
         declaring a class does not automatically enables the code 
         to create instances. 
@begin{verbatim}
    :- class(@em{ModuleName},[],[objects]).
@end{verbatim}

         This package enables both static and dynamic usage of objects.
        ").

:- doc(module,
	"This package is required to enable user code to create
         objects and manipulate them, as well as loading any needed class.
         ").

%% Error reporting:

:- include(library(objects/objects_error_doc)).

%%------------------------------------------------------------------------

:- doc(' use_class'/1,
	"It establishes an @concept{usage relationship} between 
         the given file (which is supposed to
	 declare a class) and current source. 
         Usage relationships are needed in order to enable code to
         create instances of the given class, and to make calls to
         instances derived from such class.

         Since an interface is some kind of class, 
         they may be used within this declaration
         but only for semantic checking porpouses. Instances
         will not be derived from interfaces.

         use_class/1 is used in the same way as @decl{use_module/1}.
        ").

:- decl ' use_class'(ClassSource) : class_source(ClassSource) #
	"Establish usage relationship with @var{ClassSource}.".
         
%%------------------------------------------------------------------------

:- doc(' instance_of'/2,
	"Statically declares an identifier to be an instance of
         a given class.

         It may be used as @pred{new/2} predicate except for:
         @begin{itemize}

         @item The instance identifier will not be a variable, it must 
               be provided by the user, and must be unique.

         @item Instance creation will never fail, even if the constructor
               fails.
         @end{itemize}

         For every statically declared object the given constructor will
         be called at program startup. Those instances may be destroyed
         manually, but it is not recommended. 

         When reloading the involved class from the Ciao toplevel shell. 
         It may destroy statically declared instances, and create them again.

         Statically declared instances must be called using a specifically 
         designed module-qualification: @tt{ClassName(Object):Goal}. For example:
@begin{verbatim}
    :- module(example,[main/0],[objects]).
    :- use_class(library(counter)).
    :- cnt instance_of counter(10).

    main :-
         counter(cnt):decrease(1),
         counter(cnt):current_value(X),
         display(X).
@end{verbatim}
         But @bf{statically written code} (only) is allowed to use module-style  
         qualifications as a macro:
@begin{verbatim}
    main :-
         cnt:decrease(1),
         cnt:current_value(X),
         display(X).
@end{verbatim}
         Notice that dynamically expanded goals such as @tt{X=cnt,X:decrease(1)}
         will not work, use @tt{X=counter(cnt),X:decrease(1)} instead.
        ").

:- decl ' instance_of'(Object,Constructor) :
	(instance_id(Object),constructor(Constructor)) #
        "Declares @var{Object} to be an instance of the class 
         denoted by @var{Constructor}.".

%%------------------------------------------------------------------------

:- doc(' new'/2,
	"This declaration has the same effect as @decl{ instance_of/2}.").

:- decl ' new'(Object,Constructor) :
	(instance_id(Object),constructor(Constructor)) #
	"Just an alias for @decl{ instance_of/2}.".
