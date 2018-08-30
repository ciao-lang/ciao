:- use_package(assertions).
:- doc(nodoc,assertions).

:- doc(title,"Pure Prolog package").

:- doc(author,"The Ciao Development Team").
 
:- doc(module,"This library package allows the use of @index{pure
   Prolog} in a Ciao module/program. When using this package most of
   the @index{engine module}s defined in the basic language are not
   included. ").

:- use_package(library(pure)).

:- doc(bug,"Currently, the following builtin predicates/program
	constructs cannot be redefined, in addition to @tt{true/0}:
        @tt{(->)/2} @tt{(,)/2} @tt{(\+)/1} @tt{if/3}").
