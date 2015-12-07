%%------------------------------------------------------------------------
%%
%% O'Ciao: Object Oriented Programming in Ciao/Prolog
%%
%% MAIN DOCUMENTATION FILE
%%
%% AUTHOR : Angel Fernandez Pineda
%%
%% CLIP Laboratory - Technical University Of Madrid.
%%
%%         - Distributed under the Ciao Prolog license terms -
%%
%%------------------------------------------------------------------------

:- module(ociao_doc,[],[assertions]).

:- doc(filetype, documentation).

:- doc(title, "Object Oriented Programming").
%:- doc(subtitle, "Object oriented programming in Ciao Prolog").

:- doc(author,"Angel Fernandez Pineda").

:- doc(copyright,"@include{DocCopyright.lpdoc}").

:- doc(summary,
	"O'Ciao is an object-oriented programming extension to Ciao
         Prolog, which enhances the Ciao Prolog module system
         by adding:

         @begin{itemize}
       
         @item Inheritance relationships.
         @item Module/class instantiation.
         @item Virtual method definitions.
         @item Constructor/destructor definitions.

         @end{itemize}
	").

:- doc(module,
	"O'Ciao is a set of libraries which allows
         object-oriented programming in Ciao Prolog.
         It extends the Ciao Prolog module system by introducing
         two new concepts:

          @begin{itemize}
           @item Inheritance.
           @item Instantiation.
          @end{itemize}
          
        @index{Polymorphism} is the third fundamental concept 
        provided by object oriented
        programming. This concept is not mentioned here since 
        @bf{traditional PROLOG systems are polymorphic by nature}.

        Classes are declared in the same way as modules.
        However, they may be enriched with inheritance declarations and other 
        object-oriented constructs. For an overview of the fundamentals
        of O'Ciao, see 
        @href{http://www.clip.dia.fi.upm.es/~clip/papers/ociao-tr.ps.gz}.
        However, we will introduce the concepts in a tutorial way via examples.

@section{Early examples}

        The following one is a very simple example which declares a class --
        a simple stack.
        Note that if you replace @em{class/1} declaration with 
        a @em{module/1} declaration, it will compile correctly, and 
        can be used as a normal Prolog module.

        @comment{-------- EXAMPLES -------}

@begin{verbatim}
@includeverbatim{class/examples/stack}
@end{verbatim}

        If we load this code at the Ciao toplevel shell:

@begin{verbatim}
        ?- use_package(objects).

        yes
        ?- use_class(library(class/examples/stack)).

        yes
        ?-
@end{verbatim}

        we can create two stack @em{instances} :

@begin{verbatim}
        ?- St1 new stack,St2 new stack.

        St1 = stack('9254074093385163'),
        St2 = stack('9254074091') ? ,
@end{verbatim}

        and then, we can operate on them separately: 

@begin{verbatim}
        1 ?- St1:push(8),St2:push(9).

        St1 = stack('9254074093385163'),
        St2 = stack('9254074091') ? 

        yes
        1 ?- St1:top(I),St2:top(K).

        I = 8,
        K = 9,
        St1 = stack('9254074093385163'),
        St2 = stack('9254074091') ? 

        yes
        1 ?-
@end{verbatim}

        The interesting point is that there are two stacks.
        If the previous example had been a normal module, we 
        would have a stack , but @bf{only one} stack. 
        
        The next example introduces the concepts of @em{inheritable}
        predicate, @em{constructor}, @em{destructor} 
        and @em{virtual method}. Refer to the following sections for
        further explanation.

@begin{verbatim}
@includeverbatim{class/examples/generic}
@end{verbatim}

        And the following example, is an extension of previous class.
        This is performed by establishing an inheritance relationship:

@begin{verbatim}
@includeverbatim{class/examples/specific}
@end{verbatim}

        @bf{Additional examples} may be found on the 
        @em{library/class/examples} directory relative to your Ciao 
        Prolog instalation.

@section{Recommendations on when to use objects}

               @comment{---- RECOMMENDATIONS -----}

        We would like to give some advice in the use of object
        oriented programming, in conjunction with the declarative 
        paradigm.

        You should reconsider using O'Ciao in the following cases:

        @begin{itemize}

         @item The pretended \"objects\" have no state,i.e., no data or
               dynamic predicates. In this case, a normal module will suffice.

         @item There is state, but there will be only one 
               instance of a pretended class. Again, a module suffices.

         @item The \"objects\" are data structures (list,trees,etc) already
               supported by Prolog. However, it does make sense to model, 
               using objects, data structures whose change implies a 
               side-effect such as drawing a particular window on the screen.

        @end{itemize} 

        We recommend the usage of O'Ciao in the following cases: 

        @begin{itemize}

         @item You feel you will need to have several copies of a 
               \"module\".
         @item Local copies of a module are needed instead of a global
               module beeing modified by several ones. 
         @item The \"classes\" are a representation of external
               entities to Prolog. For example: the X-Window system.
         @item There is state or code outside the Prolog system which 
               needs to be manipulated. For example: interfaces to
               Java or Tcl/Tk code.
         @item You are not familiar with Prolog, but you know about 
               object oriented programming. O'Ciao may be used as 
               a learning tool to introduce yourself on the declarative
               programming paradigm.

        @end{itemize} 

@section{Limitations on object usage}

        O'Ciao run-time speed is limited by the usage of meta-programming
        structures, for instance: @tt{X = (Object:mymethod(25)), call(X)}. 
        O'Ciao will optimize static manipulation of objects (those that
        can be determined at compile time).
	").

%%------------------------------------------------------------------------


