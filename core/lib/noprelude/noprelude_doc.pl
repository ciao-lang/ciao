:- use_package(assertions).
:- doc(nodoc,assertions).
:- doc(nodoc,assertions_basic).

:- doc(title,"No-prelude").

:- doc(author,"The Ciao Development Team").
 
:- doc(module, "This special package disables the implicit inclusion
   of the @lib{prelude} in a Ciao module/program. When using this
   package most of the @index{engine module}s defined in the basic
   language are not included. It selects a minimal @em{kernel}
   language with very tight control on the language features.").

:- use_package(library(noprelude)).

:- doc(bug,"Currently, the following builtin predicates/program
	constructs cannot be redefined, in addition to @tt{true/0}:
        @tt{(->)/2} @tt{(,)/2} @tt{(\+)/1} @tt{if/3}").
