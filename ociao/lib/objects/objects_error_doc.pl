%%------------------------------------------------------------------------
%%
%% O'Ciao : OBJECT ORIENTED PROGRAMMING IN Ciao/Prolog
%%
%% DOCUMENTATION FILE ON OBJECT MANIPULATION
%%
%% AUTHOR : Angel Fernandez Pineda
%% CLIP Laboratory - Technical University Of Madrid.
%%
%%         - Distributed under the Ciao Prolog license terms -
%%
%%------------------------------------------------------------------------

%%------------------------------------------------------------------------


:- doc(appendix,
	"
 Compile-time errors are restricted to some
 local analysis. Since there is no type declaration in the Prolog
 language, there is no posibility to determine whenever a given
 variable will hold an instance of any class.

 However, little semantic analysis is performed. User may aid to perform
 such an analysis by the usage of run time checks (which are also detected
 at compile time), or static declarations. For example:

 @tt{clause(Obj) :- Obj:a_method(334).}

 O'Ciao may be not able to determine whenever a_method/1 is a valid method
 for instance Obj, unless some help is provided:

 @tt{clause(Obj) :- Obj instance_of myclass,Obj:a_method(334).}

 In such case, O'Ciao will report any semantic error at compile-time.


 Most of the run-time errors are related to normal Ciao Prolog 
 module system. Since objects are treated as normal Prolog 
 modules at run time, there is no further documentation 
 here about that stuff.


 @subsection{Error reporting at compile time (objects)}



@begin{itemize}

 @item @bf{ERROR : invalid instance identifier @em{ID}: must be an atom}

     There is a @decl{instance_of/2} or @decl{new/2} declaration where
     first argument @em{ID} must be an unique atom, but currently it is
     not. Statically declared instances needs an identifier to be
     provided by the user.

 @item @bf{ERROR : instance identifier @em{ID} already in use}

     There are two or more @decl{instance_of/2} declarations with the same
     first argument @em{ID}. Instance identifiers must be unique.

 @item @bf{ERROR : invalid use_class/1 declaration: @em{SourceFile} 
           is not a class}

 Those are the causes for this error:

   @begin{itemize}
      
     @item The given @em{SourceFile} does not exist, or is not accesible.

     @item The given @em{SourceFile} is not a Prolog source.

     @item The given @em{SourceFile} is a valid Prolog source, but it 
           does not declare a class.
 
   @end{itemize}

 @item @bf{ERROR : unknown class on @em{ID} instance declaration}

     The class defined on the @decl{instance_of/2} declaration 
     for @em{ID} instance has not been loaded by a 
     @decl{use_class/1} declaration.

 @item @bf{ERROR : instance identifier @em{ID} is an exisisting Prolog module}

     There is an statically declared instance whose identifier may cause
     interference with the Ciao Prolog module system. Use another instance
     identifier.

 @item @bf{ERROR : unknown constructor on @em{ID} instance declaration}

     The given constructor on the @decl{instance_of/2} declaration for
     @em{ID} has not been defined at the corresponding class.

 @item @bf{ERROR : constructor is needed on @em{ID} instance declaration}

     No constructor was defined on the @decl{instance_of/2} declaration for
     @em{ID} and default constructor is not allowed. You must provide a
     constructor.

 @item @bf{ERROR : static instance @em{ID} was derived from a different constructor 
           at @em{AnotherModule}}

     @em{ID} has been declared to be an static instance 
     both on @em{AnotherModule} and  current source, 
     but different constructors were used. The most probable causes for
     this error are:
     
     @begin{itemize}

       @item Occasionally, there is another module using the same 
             instance identifier and it was not noticed by you. 
             Another different identifier may be used instead.

       @item It was you intention to use the same object as 
             declared by the other module.
             In this case, the same constructor must be used.

     @end{itemize}

 @item @bf{ERROR : invalid first argument in call to new(@em{Arg},_)}

 There is a new/1 goal in your code where first argument is not a free
 variable. For example:

    @tt{myobj new myclass}

 First argument must be a variable in order to receive a run-time generated
 object identifier.

 @item @bf{ERROR : unknown class in call to new(?,@em{Constructor}) }

 The given @em{Constructor} in call to new/2 does not correspond to any
 used class at current code. The most probable cause of this may be:

   @begin{itemize}
     
     @item You forgot to include a @decl{use_class/1} declaration in
           your code.

     @item There is a spelling mistake in the constructor.For example:

           :- use_class(myclass).

            foo(X) :- X new mclass.

   @end{itemize}

 @item @bf{ERROR : can not create an instance from an interface: 
           new(?,@em{Constructor}) }

 Given @em{Constructor} references an interface rather than a class. Instances
 can not be derived from interface-expanded code.

 @item @bf{ERROR : unknown constructor in call to new(?,@em{Constructor}) }

 As the previous error, there is a mistake in the given @em{Constructor}.
 This error is reported when you are trying to call a constructor which
 was not defined at the corresponding class. Check the class definition
 to find what is going on. 

 Another cause for this error is the incorrect usage of the 
 @concept{default constructor}. Whenever there are one or more constructors
 defined at the involved class, you are restricted to chose one of them. 
 This seems that default constructor will be available, if and only if, 
 there are no constructors defined at the involved class.

 @item @bf{ERROR : call to non-public @em{ID:Goal}}

     You are trying to call a method which was not declared as public by the
     class specified in @decl{instance_of/2} declaration for @em{ID}.  

 @item @bf{ERROR : call to inaccessible predicate at instance @em{ID:Goal}}
 
     There is a call to @em{Goal} at statically declared instance @em{ID}
     which is unknown or was not declared as public.

 @item @bf{ERROR : unknown instance @em{ID} of class @em{Class} at @em{Goal}}
  
     There is a call to @em{Goal} where involved statically declared instance
     @em{ID} is unknown or is not derived from @em{Class}. Check whether it 
     was declared by a @decl{instance_of/2} declaration.

 @item @bf{ERROR : inaccessible attribute @em{Fact} at instance @em{ID}}
 
     There is an attempt to use @em{ID:Fact} but it was not declared as
     public.

 @item @bf{ERROR : unknown attribute @em{Fact} at instance @em{ID}}
 
     There is an attempt to use @em{ID:Fact} but it is unknown or it is not
     an attribute (may be a method).

 @item @bf{WARNING : invalid call to new(?,_)}

 There is a call to new/2 in you code where first argument variable has been
 determined to hold any other instance. For example:

 @tt{foo :- X new myclass,X new otherclass.}

 or

 @tt{foo(X) :- X instance_of myclass, X new myclass.}

 The related call to new/2 will allways fail.

 @item @bf{WARNING : called @em{Goal} is not public at any used class}

 There is a call to @em{Var}:@em{Goal} where @em{Var} has not been determined
 to be compatible with any class. However, @em{Goal} is not public at 
 any class specified by the @decl{use_class/1} declaration.

 This is a warning (not an error) since @em{Var}:@em{Goal} may be not related 
 to Object Oriented Programing.

 @end{itemize}

 @subsection{Error reporting at run time (objects)}


 @begin{itemize}

 @item @bf{EXCEPTION : instantiation_error(
           '1st argument must be free variable')}

       Calling to new/1 requieres first argument to be a free variable.
       For example:

       @tt{X = this_will_raise_an_exception,X new myclass.}

 @item @bf{EXCEPTION : instantiation_error('class not given')}

       You called new/2 using a free variable as second argument.

 @item @bf{EXCEPTION : instantiation_error(inaccesible_class(@em{Class}),
          from(@em{Module})) }

       @em{Module} tried to create an instance of @em{Class} by the ways of
       new/2, but there is no usage relationship between @em{Module} and
       @em{Class}.

 @item @bf{EXCEPTION : instantiation_error(invalid_constructor(
           @em{Constructor})) }

       @em{Constructor} was not defined by the corresponding class.

 @end{itemize}
         ").
