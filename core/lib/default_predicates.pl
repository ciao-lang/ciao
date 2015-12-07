:- module(default_predicates, [], [assertions]).

:- doc(title,"Other predicates and features defined by default").

:- doc(author, "Daniel Cabeza").

:- doc(module,"To simplify the use of Ciao Prolog to the
   @concept{first-timers}, some other predicates and features are
   defined by default in normal cases, to provide more or less what
   other prologs define by default.  Here are explicitly listed the
   predicates defined, coming from several libraries.  Apart from those,
   the features defined in @ref{Definite Clause Grammars} and
   @ref{Enabling operators at run-time} are also activated.").

:- doc(usage, "No need of explicit loading.  It is included by
   default in modules starting with a @decl{module/2} declaration or
   user files without a starting @decl{use_package/1} declaration.  In
   the Ciao shell, it is loaded by default when no @file{~/.ciaorc}
   exists.  Note that @tt{:- module(@em{modulename},@em{exports})}  is
   equivalent to @tt{:- module(@em{modulename},@em{exports},[default])}
   If you do not want these predicates/features loaded for a given file
   (in order to make the executable smaller) you can ask for this
   explicitly using @tt{:- module(@em{modulename},@em{exports},[])}  or
   in a user file @tt{:- use_package([])}.").

:- reexport(library(aggregates)).
:- reexport(library(dynamic)).
:- reexport(library(read)).
:- reexport(library(write)).
:- reexport(library(operators), [op/3, current_op/3]).
:- reexport(library(iso_char)).
:- reexport(library(iso_misc)).
:- reexport(library(format)).
:- reexport(library(lists),
        [append/3, delete/3, select/3, nth/3, last/2,reverse/2, length/2]).
:- reexport(library(sort)).
:- reexport(library(between)).
:- reexport(library(compiler), [use_module/1, use_module/2, ensure_loaded/1]).
:- reexport(library(system)).
:- reexport(library(prolog_sys)).
:- reexport(library(dec10_io)).
:- reexport(library(old_database)).
:- reexport(library(ttyout)).
