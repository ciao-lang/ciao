:- module(modules, [], [assertions]).

:- doc(title, "The module system").

:- doc(author,"Daniel Cabeza").
:- doc(author,"The CLIP Group").

:- doc(usage, "Modules are an intrinsic feature of Ciao, so nothing
   special has to be done to use them.").

:- doc(summary, "Modularity is a basic notion in a modern computer
   language.  Modules allow dividing programs into several parts,
   which have their own independent name spaces.").

:- doc(module, "Modularity is a basic notion in a modern computer
   language.  Modules allow dividing programs into several parts,
   which have their own independent name spaces.  The module system in
   Ciao @cite{ciao-modules-cl2000} is, as in most Prolog
   implementations, @em{procedure based}.  This means that predicate
   names are local to a module, but functor/atom names in data are
   shared (at least by default).

   @subsubsection{Visibility rules}

   The @em{predicates visible in a module} are the predicates
   defined in that module, plus the predicates imported from other
   modules.  Only predicates exported by a module can be imported from
   other modules.  The default module of a given predicate name is the
   local one if the predicate is defined locally, else the @em{last}
   module from which the predicate is imported, where explicit imports
   have priority over implicit ones (that is, a predicate imported
   through a @tt{use_module/2} declaration is always preferred over a
   predicate imported through a @tt{use_module/1} declaration).  To
   refer to a predicate from a module which is not the default module
   for that predicate the name has to be module @cindex{module
   qualification}qualified.  A module-qualified predicate name has the
   form @var{Module}:@var{Predicate} as in the call
   @tt{debugger:debug_module(M)}.  Note that in Ciao this module
   qualification cannot be used for gaining access to predicates that
   have not been imported, nor for defining clauses of other modules.

   @subsubsection{Files with no mudule declaration ('user' files)}

   All predicates defined in files with no module declaration belong
   to a special module called @cindex{user module} @tt{user}, from
   which they are all implicitly exported.  This provides backward
   compatibility for programs written for Prolog implementations with
   no module system and allows dividing programs into several files
   without being aware of the module system at all.  Note that this
   feature is only supported for the above-mentioned
   backward-compatibility reasons, and the use of @tt{user} files is
   discouraged.  Many attractive compilation features of Ciao cannot
   be supported for @tt{user} modules.

   @subsubsection{Multifile predicates}

   The case of multifile predicates (defined with the declaration
   @decl{multifile/1}) is also special.  Multifile predicates can be
   defined by clauses distributed in several modules, and all modules
   which define a predicate as multifile can use that predicate.  The
   name space of multifile predicates is independent, as if they
   belonged to the special module @tt{multifile}.

   @subsubsection{Libraries imported by default ('builtins')}

   While in Ciao there are no 'built-in' predicates (i.e., predicates
   whose load cannot be avoided or that that cannot be redefined --see
   below) for convenience every module or @tt{user file} imports
   implicitly a number of modules called @concept{builtin modules}
   (also referred to as @concept{default modules}). Which exact
   modules are imported by default is controlled by the third argument
   of @decl{module/3} declarations, the lack thereof in
   @decl{module/2} declarations, some rules for user files, etc., as
   described below. For example, for backward compatibility with
   traditional Prolog systems, if a @decl{module/2} declaration is
   used, then the traditional predicates that are built in in most
   Prolog systems are imported in that module.

   Predicates coming from builtin/default modules are imported before
   all other importations of the module. This allows the
   @concept{redefinition of builtins}, i.e., the redefinition of any
   of the predicates imported by default from builtin/default modules
   (with the exception of @pred{true/0}) by either defining local
   versions of these predicates or by importing them from other
   modules.

   Importing explicitly from a builtin module, however, disables the
   implicit importation of the rest of the builtin modules that would
   be otherwise loaded (this feature is used for example by package
   @lib{library(pure)} to define pure modules that do not import any
   traditional Prolog builtins; i.e., @concept{pure Prolog} code).").

:- doc(doinclude,module/3).
:- true decl module(Name, Exports, Packages)
        : modulename * list(predname) * list(sourcename)

        # "Declares a module of name @var{Name} which exports the
          predicates in @var{Exports}, and uses the packages in
          @var{Packages}.  @var{Name} must match with the name of the
          file where the module resides, without extension.  For each
          source in @var{Packages}, a @concept{package file} is used.
          If the source is specified with a @concept{path alias}, this
          is the file included, if it is an atom, the library paths
          are searched. See @decl{package/1} for a brief description
          of package files.

          This directive must appear the first in the file.

          Also, if the compiler finds an unknown declaration as the
          first term in a file, the name of the declaration is regarded
          as a package library to be included, and the arguments of the
          declaration (if present) are interpreted like the arguments of
          @decl{module/3}.".

:- doc(doinclude,module/2).
:- true decl module(Name, Exports) : modulename * list(predname)

        # "Same as directive @decl{module/3}, with an implicit package
          @tt{default}. This default package provides all the standard
          features provided by most Prolog systems so that Prolog
          programs with traditional @decl{module/2} declarations can
          run without any change.".

:- doc(doinclude,package/1).
:- true decl package(Name)
        : modulename

        # "Declares a package of name @var{Name}. Like in modules,
          @var{Name} must match with the name of the file where the
          package resides, without extension. This directive must
          appear the first in the file.

          Package files provide syntactic extensions and their related
          functionalities by defining operators, new declarations,
          code translations, etc., as well as declaring imports from
          other modules and defining additional code. Most Ciao
          syntactic and semantic extensions, such as functional
          syntax, constraint solving, or breadth-first search are
          implemented as packages.".

:- doc(doinclude,export/1).
:- true decl export(Pred) : predname
        # "Adds @var{Pred} to the set of exported predicates.".
:- true decl export(Exports) : list(predname)
        # "Adds @var{Exports} to the set of exported predicates.".

:- doc(doinclude,use_module/2).
:- true decl use_module(Module, Imports) : sourcename * list(predname)
        # "Specifies that this code imports from the module defined in
          @var{Module} the predicates in @var{Imports}.  The imported
          predicates must be exported by the other module.".

:- doc(doinclude,use_module/1).
:- true decl use_module(Module) : sourcename
        # "Specifies that this code imports from the module defined in
          @var{Module} all the predicates exported by it.  The previous
          version with the explicit import list is preferred to this as
          it minimizes the chances to have to recompile this code if the
          other module changes.".

:- doc(doinclude,import/2).
:- true decl import(Module, Imports) : modulename * list(predname)
        # "Declares that this code imports from the module with name
          @var{Module} the predicates in @var{Imports}.

          @bf{Important note:} this declaration is intended to be used
          when the current module or the imported module is going to be
          dynamically loaded, and so the compiler does not include the
          code of the imported module in the current executable (if only
          because the compiler cannot know the location of the module
          file at the time of compilation).  For the same reason the
          predicates imported are not checked to be exported by
          @var{Module}.  Its use in other cases is strongly discouraged,
          as it disallows many compiler optimizations.

          This is an example of such a case for a dynamically loaded
          module:

@begin{verbatim}
:- module(_,_).

:- import(bar,[b/1]).

main(X) :- 
     use_module(bar),
     b(X).
@end{verbatim}
".

:- doc(doinclude,reexport/2).
:- true decl reexport(Module, Preds) : sourcename * list(predname)
        # "Specifies that this code reexports from the module defined in
          @var{Module} the predicates in @var{Preds}. This implies that
          this module imports from the module defined in @var{Module}
          the predicates in @var{Preds}, an also that this module
          exports the predicates in @var{Preds} .".

:- doc(doinclude,reexport/1).
:- true decl reexport(Module) : sourcename
        # "Specifies that this code reexports from the module defined in
          @var{Module} all the predicates exported by it. This implies that
          this module imports from the module defined in @var{Module}
          all the predicates exported by it, an also that this module
          exports all such predicates .".

:- doc(doinclude,meta_predicate/1).

:- true decl meta_predicate(MetaSpecs) : sequence(metaspec)
        # "Specifies that the predicates in @var{MetaSpecs} have
          arguments which have to be module expanded (predicates,
          goals, etc).  @decl{meta_predicate/1} directives are only
          mandatory for exported predicates (in modules).  This
          directive is defined as a prefix operator in the compiler.".

:- doc(doinclude, modulename/1).

:- doc(modulename/1, "A module name is an atom, not containing
        characters `:' or `$'.  Also, @tt{user} and @tt{multifile} are
        reserved, as well as the module names of all builtin modules
        (because in an executable all modules must have distinct
        names).").

:- prop modulename(M) + regtype # "@var{M} is a module name (an atom).".

modulename(M) :- atm(M).

:- doc(doinclude, metaspec/1).

:- doc(metaspec/1, "A meta-predicate specification for a predicate
        is the functor of that predicate applied to terms which
        represent the kind of module expansion that should be applied to
        each argument.  Possible contents are represented as:

        @begin{description}

        @item{@tt{?,+,-,_}} These values denote that this argument is not
        module expanded.

        @item{@tt{goal}} This argument will be a term denoting a goal
        (either a simple or complex one) which will be called.  For
        commpatibility reasons it can be named as @tt{:} as well.

        @item{@tt{clause}} This argument will be a term denoting a clause.

        @item{@tt{fact}} This argument should be instantiated to a term
        denoting a fact (head-only clause).

        @item{@tt{spec}} This argument should be instantiated to a predicate
        name, as Functor/Arity.

        @item{@tt{pred(@em{N})}} This argument should be instantiated to
        a predicate construct to be called by means of a
        @tt{call/@em{N}} predicate call (see @pred{call/2}).

        @item{@tt{list(@em{Meta})}} This argument should be instantiated
        to a list of terms as described by @em{Meta}
        (e.g. @tt{list(goal)}).

        @item{@tt{addterm(Meta)}} This argument should be instantiated
        to the meta-data specified by @em{Meta}, and an argument added
        after this one will carry the original data without module
        expansion.  Not intended to be used by normal users. 

        @item{@tt{addmodule(Meta)}} This argument should be instantiated
        to the meta-data specified by @em{Meta}, and in an argument
        added after this one will be passed the calling module, for
        example to allow handling more involved meta-data by using
        conversion builtins.  @tt{addmodule} is an alias of
        @tt{addmodule(?)}. Not intended to be used by normal users. 

        @end{description}").

:- prop metaspec(M) + regtype # "@var{M} is a meta-predicate specification.".

metaspec(M) :-
	var(M),
	!,
	atm(F),
	list(A, argspec),
	M =.. [F|A].
metaspec(M) :-
	M =.. [F|A],
	atm(F),
	list(A, argspec).

:- prop argspec(A) + regtype # "@var{A} is an argument of a
	meta-predicate specificacion.".

argspec(goal).
argspec(clause).
argspec(fact).
argspec(spec).
argspec(list(Meta)) :-
	argspec(Meta).
argspec(addterm(Meta)) :-
	argspec(Meta).
argspec(addmodule(Meta)) :-
	argspec(Meta).
argspec(pred(N)) :-
	nnegint(N).
