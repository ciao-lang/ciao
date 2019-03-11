:- use_package([assertions]).
:- doc(nodoc,assertions).
:- doc(nodoc,assertions_basic).

:- doc(filetype,package).

:- doc(title,"Classic Prolog").

:- doc(author, "Daniel Cabeza").
:- doc(author, "Jose F. Morales").
:- doc(author, "Manuel Hermenegildo").

:- doc(module,"This package loads the predicates and features that
   @concept{classical Prolog} implementations provide by default (the
   'built-ins'). This includes the ISO-Prolog predicates and features
   as well as others that are de-facto standard built-ins in most
   Prolog implementations. Consult the @lib{classic_predicates} module
   for the listing of the concrete predicates that are available and
   the Ciao libraries they come from (see pointer below).  Apart from
   these, the features defined in @ref{Definite Clause Grammars} and
   @ref{Enabling operators at run-time} are also activated.

   The loading of this package can be controlled as described in the
   @em{Library usage} below and in @ref{The module system}.

").

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

:- doc(hide,'\6\call_in_module'/2).
:- use_package(classic).
