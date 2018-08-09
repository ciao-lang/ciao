:- module(default_predicates, [], [assertions]).

:- doc(title,"Classic Prolog").

:- doc(author, "Daniel Cabeza").
:- doc(author, "Jose F. Morales").
:- doc(author, "Manuel Hermenegildo").

:- doc(module,"This package loads the predicates and features that
   @concept{classical Prolog} implementations provide by default (the
   'buil-ins'). This includes the ISO-Prolog predicates and features
   as well as others that are de-facto standard built-ins in most
   Prolog implementations.  A listing is provided below of the
   concrete predicates made available and the Ciao libraries they come
   from.  Apart from these, the features defined in @ref{Definite
   Clause Grammars} and @ref{Enabling operators at run-time} are also
   activated.

   The loading of this package can be controlled as described in the
   @em{Library usage} below and @ref{The module system}.

").


% TODO: change the name of this package from default to classic_prolog (or something like that)
	
:- doc(usage, "All these modules, packages, and predicates are included by
   default in modules starting with a @decl{module/2} declaration or
   user files without a starting @decl{use_package/1} declaration.  In
   the Ciao shell, they are loaded by default when no @file{~/.ciaorc}
   exists.  

   Note that @tt{:- module(@em{modulename},@em{exports})}  is
   equivalent to @tt{:- module(@em{modulename},@em{exports},[default])}.

   If you do not want these predicates/features loaded for a given
   file (e.g., for purity considerations, to aid analysis, to help
   making the executable smaller, etc.) you can ask for this
   explicitly using @tt{:-
   module(@em{modulename},@em{exports},[])}. The same can be achieved
   in a user file using @tt{:- use_package([])} and in the top level
   by defining @file{~/.ciaorc} (which can then include whatever
   alternative prelude is preferred).").

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
