%%------------------------------------------------------------------------
%%
%% O'Ciao: Object Oriented Programming in Ciao/Prolog
%%
%% DOCUMENTATION FILE ON CLASS ERROR REPORTING
%%
%% AUTHOR : Angel Fernandez Pineda
%%
%% CLIP Laboratory - Technical University Of Madrid.
%%
%%         - Distributed under the Ciao Prolog license terms -
%%
%%------------------------------------------------------------------------

%:- module(class_error_doc,[],[assertions]).


:- doc(appendix,
	"

 This describes the errors reported when declaring a class or an
 interface. The first section will explain compile-time errors, this is,
 any semantic error which may be determined at compile time. The second
 section will explain run-time errors, this is, any exception that may
 be raisen by the incorrect usage of O'Ciao. Some of those errors may be
 not reported at compile time, due to the use of meta-programational
 structures. For example:

   @tt{functor(X,my_method,0),call(X).}

 O'Ciao is not able to check whether my_method/0 is a valid method
 or not. So, this kind of checking is left to run time.

 @subsection{Class and Interface error reporting at compile time}


 @begin{itemize}

 @item @bf{ERROR : multiple inheritance not allowed.}

       There are two or more inherit_class/1 declarations found at your code.
       Only one declaration is allowed, since there is no multiple code 
       inheritance support.

 @item @bf{ERROR : invalid inheritance declaration.}

       The given parameter to inherit_class/1 declaration is not a valid
       path to a Prolog source.

 @item @bf{ERROR : sorry, addmodule meta-arg is not allowed at @em{F/A}. }

       You are trying to declare @em{F/A} as meta-predicate, and one of
       the meta-arguments is @em{addmodule}. This is not allowed in O'Ciao
       due to implementation restrictions. For example:

       @tt{:- meta_predicate example(addmodule).}

       @tt{example(X,FromModule) :- call(FromModule:X).}

 @item @bf{ERROR : invalid attribute declaration for @em{Arg}. }

       Argument to data/1 or dynamic/1 declaration is not a valid predicate
       specification of the form @em{Functor/Arity}. For example:

       @tt{:- data attr.}

       @tt{:- dynamic attr(_).}

       @tt{:- data attr/m.}

      etc,etc...

 @item @bf{ERROR : pretended attribute @em{F/A} was assumed to be a method.}

       You put some clauses of @em{F/A} before the corresponding data/1 
       or dynamic/1 declaration. For example:

       @tt{attr(initial_value).}

       @tt{:- data attr/1.}

       It is a must to declare attributes before any clause of the given
       predicate.
 
 @item @bf{ERROR : destructor/0 is not allowed to be an attribute.}

       There is a :- data(destructor/0) or :- dynamic(destructor/0). 
       declaration in your code. This is not allowed since destructor/0 is 
       a reserved predicate, and must be allways a method.

 @item @bf{ERROR : @em{Constructor} is not allowed to be an attribute.}

       As the previos error, you are trying to declare a constructor as an
       attribute. A constructor must be allways a method.

 @item @bf{ERROR : invalid multifile: destructor/0 is a reserved predicate.}

       There is a :- multifile(destructor/0). 
       declaration in your code. This is not allowed since destructor/0 is 
       a reserved predicate, and must be allways a method.

 @item @bf{ERROR : invalid multifile: @em{Constructor} is a reserved predicate.}

       As the previos error, you are trying to declare a constructor as a
       multifile. Any constructor must allways be a method.

 @item @bf{ERROR : multifile declaration of @em{F/A} ignored: it
           was assumed to be a method.}

       You put some clauses of @em{F/A} before the corresponding  
       multifile/1 declaration. For example:

       example(a,b).

       @tt{:- multifile example/2.}

       Multifile predicates must be declared before any clause of the given
       predicate.

 @item @bf{ERROR : invalid multifile declaration: multifile(@em{Arg}). }

       Given argument to multifile/1 declaration is not a valid predicate
       specification, of the form @em{Functor/Arity}.

 @item @bf{ERROR : invalid public declaration: @em{Arg}. }

       Given argument @em{Arg} to public/1 or export/1 declaration 
       is not a valid predicate specification, 
       of the form @em{Functor/Arity}.

 @item @bf{ERROR : invalid inheritable declaration: inheritable(@em{Arg}). }

       Given argument @em{Arg} to inheritable/1 declaration 
       is not a valid predicate specification, of the form @em{Functor/Arity}.

 @item @bf{ERROR : destructor/0 is not allowed to be virtual.}

       There is a :- virtual(destructor/0) declaration present at your code.
       Destructors and/or constructors are not allowed to be virtual.

 @item @bf{ERROR : @em{Constructor} is not allowed to be virtual.}

       As the previous error, you are trying to declare a constructor as
       virtual. This is not allowed.

 @item @bf{ERROR : invalid virtual declaration: virtual(@em{Arg}). }

       Given argument to virtual/1 declaration is not a valid predicate
       specification, of the form @em{Functor/Arity}.

 @item @bf{ERROR : clause of @em{F/A} ignored : only facts are allowed
           as initial state.}

       You declared @em{F/A} as an attribute, then you put some clauses
       of that predicate in the form @em{Head :- Body}. For example:

       @tt{:- data my_attribute/1.}

       @tt{my_attribute(X) :- X>=0 , X<=2.}

       This is not allowed since attributes are assumed to hold simple facts.
       The correct usage for those @em{initialization clauses} is:

       @tt{:- data my_attribute/1.}

       @tt{my_attribute(0).}

       @tt{my_attribute(1).}

       @tt{my_attribute(2).}

 @item @bf{ERROR : multifile @em{F/A} is not allowed to be public.}

       The given @em{F/A} predicate is both present at multifile/1 and
       public/1 declarations. For example:

       @tt{:- public(p/1).}

       @tt{:- multifile(p/1).}

      This is not allowed since multifile predicates are not related to 
      Object Oriented Programming.

 @item @bf{ERROR : multifile @em{F/A} is not allowed to be inheritable.}

       Analogous to previous error.

 @item @bf{ERROR : multifile @em{F/A} is not allowed to be virtual.}

       Analogous to previous error.

 @item @bf{ERROR : virtual @em{F/A} must be a method or attribute 
           defined at this class.}
      
       There is a virtual/1 declaration for @em{F/A}, but there is not
       any clause of that predicate nor a @decl{data/1} declaration. 
       You must declare at least one clause for every virtual method.
       Virtual attributes does not require any clause but a @decl{data/1}
       declaration must be present.

 @item @bf{ERROR : implemented interface @em{Module} is not a valid interface.}

       There is an @decl{implements/1} declaration present at your code where
       given @em{Module} is not declared as class nor interface.

 @item @bf{ERROR : predicate @em{F/A} is required both as method
           (at @em{Itf1} interface) and attribute (at @em{Itf2} interface).}

       There is no chance to give a correct implementation for @em{F/A} 
       predicate since @em{Itf1} and @em{Itf2} interfaces require different
       definitions. To avoid this error, you must remove one of the related
       @decl{implements/1} declaration.

 @item @bf{ERROR : inherited @em{Source} must be a class.}

       There is an :- inherit_class(@em{Source}) declaration, but that source
       was not declared as a class.   

 @item @bf{ERROR : circular inheritance: @em{Source} is not a valid 
           super-class.}

       Establishing an inheritance relationship with @em{Source} will cause
       current class to be present twice in the inheritance line. This is not
       allowed. The cause of this is error is simple : 
       There is some inherited class from @em{Source} which also establishes
       an inheritance relationship with current source.

 @item @bf{ERROR : method/attribute @em{F/A} must be implemented.}

      Some of the implemented interfaces requires @em{F/A} to be defined,
      but there is no definition for such predicate, 
      even an inherited one.

 @item @bf{ERROR : local implementation of @em{F/A} hides inheritable/public 
	   definition.}

      There is an inherited definition for @em{F/A} which is been redefined
      at current class, but there is no valid inheritable/public 
      declaration for the last one.
      Overriden public predicates must be also declared as public.
      Overriden inheritable predicates must be declared either as 
      public or inheritable.

 @item @bf{ERROR : public predicate @em{F/A} was not defined nor inherited.}

      There is a @decl{public/1} declaration for @em{F/A}, but
      there is no definition for it at current class nor an inherited
      one.

 @item @bf{ERROR : argument to self/1 must be a free variable.}

      Argument to self/1 is not a variable, for example:
      @tt{self(abc)}. 

 @item @bf{ERROR : unknown inherited attribute in @em{Goal}.}

       @em{Goal} belongs to assert/retract family of predicates, and
       given argument is not a valid inherited attribute.
       The most probable causes of this error are:

       @begin{itemize}

        @item The given predicate is defined at super-class, but you
              forgot to mark it as inheritable (or public), at such class.
        @item The given predicate was not defined (at super-class) 
              as an attribute, just as a method.
       @end{itemize}

 @item @bf{ERROR : unknown inherited goal: @em{Goal}.}

       The given @em{Goal} was not found at super-class,
       or it is not accessible.
       Check whether @em{Goal} was marked as inheritable 
       (or public) at super-class.

 @item @bf{ERROR : invalid argument: @em{F/A} is not an attribute.}

      You are trying to pass a method as an argument to any predicate 
      which expect a @em{fact} predicate.

 @item @bf{ERROR : unknown inherited fact: @em{Fact}.}

      There is a call to any predicate which expects a @em{fact}
      argument (those declared as data or dynamic),but the actual
      argument is not an inherited attribute.For example:

      @tt{asserta_fact(inherited(not_an_attribute(8)))}

      where not_an_attribute/1 was not declared as data or dynamic by
      the super-class (or corresponding ascendant).

 @item @bf{ERROR : unknown inherited spec: @em{F/A}.}

      There is a reference to an inherited predicate specification, but
      the involved predicate has not been inherited.

 @item @bf{WARNING : meta-predicate specification of @em{F/A}
           ignored since this is an attribute.}
     
      You declared @em{F/A} both as an attribute and a meta-predicate.
      For example:

      @tt{:- meta_predicate attr(goal).}

      @tt{:- data attr/1.}

      There is no sense in declaring an attribute as meta-predicate.

 @item @bf{WARNING : class destructor is public}
 
      There is a :- public(destructor/0) declaration present at your code.
      Marking a destructor as public is a very bad idea since anybody may
      destroy or corrupt an instance before the proper time.

 @item @bf{WARNING : class destructor is inheritable}

      Analogous to previous error.

 @item @bf{WARNING : There is no call to inherited constructor/s}

      You have not declared any constructor at your class, but there is
      any inherited constructor that should be called. 
      Whenever you do not need constructors, but there is an inheritance
      relationship (where super-class declares a constructor), you
      should write a simple constructor as the following example:
@begin{verbatim}
   :- class(myclass).
   :- inherit_class(other_class).

   myclass :-
           other_class.
@end{verbatim}

 @item @bf{WARNING : multifile @em{F/A} hides inherited predicate.}

      You declared as multifle a predicate which matches an inherited
      predicate name. Any reference to the inherited predicate must be
      done by the ways of the inherited/1 qualificator.

@end{itemize}



 @subsection{Class and Interface error reporting at run time}



 @begin{itemize}


 @item @bf{EXCEPTION : error(existence_error(object_goal,@em{Goal}),@em{Mod}).}

 Called @em{Goal} from module (or class) @em{Mod} is unknown or has not
 been published.

 @end{itemize}

 @subsection{Normal Prolog module system interaction}

 O'Ciao works in conjunction with the Ciao Prolog module system,
 which also reports its own error messages. This will cause Ciao to 
 report a little criptic error messages due to the general mechanism of
 source-to-source expansion. Those are some tips you must consider when
 compiling a class:

 @begin{itemize}

 @item Any error relative to method 'm' with arity A will be reported
       for predicate 'obj$m'/A+1. For example :


 @tt{WARNING: (lns 28-30) [Item,Itema] - singleton variables in obj$remove/2}

        
       This error is relative to method remove/1.

 @item @decl{set_prolog_flag/1} declaration will be usefull when declaring
       multiple constructors. It will avoid some awful warnings. Example:

@begin{verbatim}
   :- class(myclass).

   %% Use this declaration whenever several constructors are needed.

   :- set_prolog_flag(multi_arity_warnings,off).

   myclass(_).

   myclass(_,_).

   :- set_prolog_flag(multi_arity_warnings,on).
@end{verbatim}
 @end{itemize}
        ").
