:- use_package(assertions).
:- doc(nodoc,assertions).

:- doc(title,"Pure Prolog package").

:- doc(author,"The CLIP Group").
 
:- doc(module,"This library package allows the use of 
   @index{pure Prolog} in a Ciao module/program.
   It is based on the fact that if an @index{engine module}
   is imported explicitly then all of them have to be imported explicitly.
   The engine modules are:
   @begin{itemize}
   @item @tt{engine(arithmetic)}

         @ref{Arithmetic}.
   @item @tt{engine(atomic_basic)}

         @ref{Basic predicates handling names of constants}.
   @item @tt{engine(attributes)}

         @ref{Attributed Variables Package}.
   @item @tt{engine(basic_props)}

         @ref{Basic data types and properties}.
   @item @tt{engine(basiccontrol)}

         @ref{Control constructs/predicates}.
   @item @tt{engine(data_facts)}

         @ref{Fast/concurrent update of facts}.
   @item @tt{engine(exceptions)}

         @ref{Exception and Signal handling}.
   @item @tt{engine(io_aux)}

         @ref{Message printing primitives}.
   @item @tt{engine(io_basic)}

         @ref{Basic input/output}.
   @item @tt{engine(prolog_flags)}

         @ref{Changing system behaviour and various flags}.
   @item @tt{engine(streams_basic)}

         @ref{Basic file/stream handling}.
   @item @tt{engine(system_info)}

         @ref{Internal Runtime Information}.
   @item @tt{engine(term_basic)}

         @ref{Basic term manipulation}.
   @item @tt{engine(term_compare)}

         @ref{Comparing terms}.
   @item @tt{engine(term_typing)}

         @ref{Extra-logical properties for typing}.
   @end{itemize}

   Note that if any of these modules is explicitely imported in a program
   then the language defaults to Pure Prolog, plus the functionality
   added by the modules explicitely imported.

   It is recommended that if you explicitely import an engine module you
   also use this package, which will guarantee that the predicate 
   @tt{true/0} is defined (note that this is the only Ciao builtin which
   cannot be redefined).
").

:- use_package(library(pure)).

:- doc(bug,"Currently, the following builtin predicates/program
	constructs cannot be redefined, in addition to @tt{true/0}:
        @tt{(->)/2} @tt{(,)/2} @tt{(\+)/1} @tt{if/3}").
