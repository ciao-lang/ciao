:- module(modules, [], [assertions]).

:- doc(title, "The module system").

:- doc(author, "Daniel Cabeza").
:- doc(author, "The Ciao Development Team").

:- doc(usage, "Modules are an intrinsic feature of Ciao, so nothing
   special has to be done to use them.").

:- doc(summary, "Modularity is a basic notion in a modern computer
   language.  Modules allow dividing programs into several parts,
   which have their own independent name spaces.").

:- doc(module, "Modularity is a basic notion in a modern computer
   language.  Modules allow dividing programs into several parts,
   which have their own independent name spaces.  Each module is
   written in its own file (see @decl{module/2} and @decl{module/3})
   and consists of a sequence of @concept{directives} and
   @concept{predicate} definitions. 

   Modules provide functionality to other modules by @em{exporting}
   some of the predicates defined inside the module (and also through
   @concept{multifile} predicates). However, a module does not modify
   the @em{syntax} that can be used in another module that loads
   it. This is done instead through the mechanism of
   @concept{package}s (see @ref{Packages and language extension}).

   See @cite{ciao-modules-cl2000} for a detailed description of the
   Ciao module system.

   @subsubsection{Visibility rules}

   The module system in Ciao is, as in most Prolog implementations,
   @em{procedure based}.  This means that predicate names are local to
   a module, but functor/atom names in data are shared (at least by
   default).

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

   @subsubsection{Files with no module declaration ('user' files)}

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

   @subsubsection{Basic directives}

   Unlike in other Prolog systems, directives in Ciao are not goals to
   be @em{executed} by the compiler or top level. Instead, they are
   @em{read} and acted upon by these programs. The advantage of this
   is that the effect of the directives is consistent for executables,
   code loaded in the top level, code analyzed by the preprocessor,
   etc.

   As a result, by default only the builtin directives or declarations
   defined in this section are available in user programs. However, it is
   possible to define new declarations @cindex{declarations, user
   defined} using the @decl{new_declaration/1} and
   @decl{new_declaration/2} directives (or using packages including
   them). Also, packages may define new directives via code
   translations.

   @subsubsection{Libraries imported by default ('builtins')}

   While in Ciao there are no 'built-in' predicates (i.e., predicates
   whose load cannot be avoided or that that cannot be redefined --see
   below) for convenience every module or @tt{user} file imports
   implicitly a number of modules called @concept{builtin modules}
   (also referred to as @concept{default modules}). Which exact
   modules are imported by default is controlled by the third argument
   of @decl{module/3} declarations, the lack thereof in
   @decl{module/2} declarations, some rules for user files, etc., as
   described below. For example, for backward compatibility with
   traditional Prolog systems, if a @decl{module/2} declaration is
   used, then the traditional predicates that are built-in in most
   Prolog systems are imported in that module (see package
   @lib{classic}, @ref{Classic Prolog}).

   Predicates coming from builtin/default modules are imported before
   all other importations of the module. This allows the
   @concept{redefinition of builtins}, i.e., the redefinition of any
   of the predicates imported by default from builtin/default modules
   (with the exception of @pred{true/0}) by either defining local
   versions of these predicates or by importing them from other
   modules.

   Moreover, the implicit importation of the basic modules can be
   fully disabled by some special packages (for example, omitting all
   default imports with @lib{noprelude}, or defining @concept{pure
   Prolog} modules with the @lib{pure} package).").

% ---------------------------------------------------------------------------

:- use_module(engine(stream_basic), [sourcename/1]).

:- doc(doinclude,module/3).
:- decl module(Name, Exports, Packages)
    : modulename * list(predname) * list(sourcename)

    # "Declares a module of name @var{Name} which exports the
      predicates in @var{Exports}, and uses the packages in
      @var{Packages}.  @var{Name} must match the name of the file
      where the module resides, without extension.

      For each source in @var{Packages}, the corresponding file, which
      must be a @concept{package} is used.  If the source is specified
      with a @concept{path alias}, this is the file included; if it is
      an atom, the library paths are searched. See @decl{package/1},
      @ref{Packages and language extension} for a description of
      package files. If @var{Packages} is @tt{[]} no packages are
      loaded.

      The @dec{module} directive must appear first in the file.

      As a special case @var{Exports} can be @tt{_} which means that
      all predicates in the module are exported.  Also, @var{Name} can
      be @tt{_} which means that the name of the module is the name of
      the file. Thus, the following declaration at the beginning of a
      file:

      @tt{:- module(_,_,[]).}

      takes the module name from the file name, exports all predicates
      defined, and does itself not load any packages.

      Also, if the compiler finds an unknown declaration as the first
      term in a file, the name of the declaration is considered as a
      package library to be included, and the arguments of the
      declaration (if present) are interpreted like the arguments of
      @decl{module/3}.".

:- impl_defined(module/3).

:- doc(doinclude,module/2).
:- decl module(Name, Exports) : modulename * list(predname)

    # "Same as directive @decl{module/3}, but loads implicitly the
      @tt{classic} package. This default package provides all the
      standard features provided by most Prolog systems so that Prolog
      programs with traditional @decl{module/2} declarations can run
      without any change. See @ref{Classic Prolog}.".

:- impl_defined(module/2).

% ---------------------------------------------------------------------------

:- doc(doinclude,use_package/1).
:- doc(use_package(Package),"Specifies the use in this file of the
      packages defined in @var{Package}.  See the description of the
      third argument of @decl{module/3} for an explanation of
      @concept{package file}s.

      This directive must appear the first in the file, or just after
      a @decl{module/3} declaration.

      A file with no module declaration, and with no
      @decl{use_package} directives, uses implicitly the @lib{classic}
      package (see @ref{Classic Prolog}). However, if at least one
      @decl{use_package} directive is present, then @lib{classic} is
      not loaded. If needed, must be loaded via @decl{use_package},
      together with any other packages required.").

:- impl_defined(use_package/1).

:- decl use_package(Package) : sourcename.
:- decl use_package(Package) : list(sourcename).

% ---------------------------------------------------------------------------

:- doc(doinclude,use_module/2).
:- decl use_module(Module, Imports) : sourcename * list(predname)
    # "Specifies that this code imports from the module defined in
      @var{Module} the predicates in @var{Imports}.  The imported
      predicates must be exported by the other module.".
:- impl_defined(use_module/2).

:- doc(doinclude,use_module/1).
:- decl use_module(Module) : sourcename
    # "Specifies that this code imports from the module defined in
      @var{Module} all the predicates exported by it.  The previous
      version with the explicit import list is preferred to this as
      it minimizes the chances to have to recompile this code if the
      other module changes.".
:- impl_defined(use_module/1).

:- doc(doinclude,import/2).
:- decl import(Module, Imports) : modulename * list(predname)
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
:- impl_defined(import/2).

:- doc(doinclude,reexport/2).
:- decl reexport(Module, Preds) : sourcename * list(predname)
    # "Specifies that this code reexports from the module defined in
      @var{Module} the predicates in @var{Preds}. This implies that
      this module imports from the module defined in @var{Module}
      the predicates in @var{Preds}, an also that this module
      exports the predicates in @var{Preds} .".
:- impl_defined(reexport/2).

:- doc(doinclude,reexport/1).
:- decl reexport(Module) : sourcename
    # "Specifies that this code reexports from the module defined in
      @var{Module} all the predicates exported by it. This implies that
      this module imports from the module defined in @var{Module}
      all the predicates exported by it, an also that this module
      exports all such predicates .".
:- impl_defined(reexport/1).

:- doc(doinclude,ensure_loaded/1).
:- decl ensure_loaded(File) : sourcename + iso
    # "Specifies that the code present in @var{File} will be
      included in the executable being prepared, in the @tt{user}
      module.  The file @var{File} cannot have a module
      declaration.  This directive is intended to be used by
      programs not divided in modules.  Dividing programs into
      modules is however strongly encouraged, since most of the
      attractive features of Ciao (such as static debugging and
      global optimization) are only partially available for
      @tt{user} modules.".
:- impl_defined(ensure_loaded/1).

% ---------------------------------------------------------------------------

:- doc(doinclude,include/1).
:- decl include(File) : sourcename + iso
    # "The contents of the file @var{File} are included in the
      current program text exactly as if they had been written in
      place of this directive.".
:- impl_defined(include/1).

% ---------------------------------------------------------------------------

:- doc(doinclude,export/1).
:- impl_defined(export/1).
:- decl export(Pred) : predname
    # "Adds @var{Pred} to the set of exported predicates.".
:- decl export(Exports) : list(predname)
    # "Adds @var{Exports} to the set of exported predicates.".

:- doc(doinclude,multifile/1).
:- decl multifile(Predicates) : sequence_or_list(predname) + iso
    # "Specifies that each predicate in @var{Predicates} may have
      clauses in more than one file.  Each file that contains
      clauses for a @concept{multifile predicate} must contain a
      directive multifile for the predicate.  The directive should
      precede all clauses of the affected predicates, and also
      dynamic/data declarations for the predicate.  This directive
      is defined as a prefix operator in the compiler.".
:- impl_defined(multifile/1).

:- doc(doinclude,meta_predicate/1).
:- decl meta_predicate(MetaSpecs) : sequence(metaspec)
    # "Specifies that the predicates in @var{MetaSpecs} have
      arguments which have to be module expanded (predicates,
      goals, etc).  @decl{meta_predicate/1} directives are only
      mandatory for exported predicates (in modules).  This
      directive is defined as a prefix operator in the compiler.".
:- impl_defined(meta_predicate/1).

:- doc(doinclude,redefining/1).
:- decl redefining(Predicate) : compat(predname)
    # "Specifies that this module redefines predicate
      @var{Predicate}, also imported from other module, or imports
      it from more than one module.  This prevents the compiler
      giving warnings about redefinitions of that predicate.
      @var{Predicate} can be partially (or totally) uninstantiated,
      to allow disabling those warnings for several (or all) predicates at
      once.".
:- impl_defined(redefining/1).

:- doc(doinclude,discontiguous/1).
:- decl discontiguous(Predicates) : sequence_or_list(predname) + iso
    # "Specifies that each predicate in @var{Predicates} may be
      defined in this file by clauses which are not in consecutive
      order.  Otherwise, a warning is signaled by the compiler when
      clauses of a predicate are not consecutive (this behavior is
      controllable by the @concept{prolog flag}
      @em{discontiguous_warnings}).  The directive should
      precede all clauses of the affected predicates.  This
      directive is defined as a prefix operator in the compiler.".
:- impl_defined(discontiguous/1).

:- doc(doinclude,impl_defined/1).
:- decl impl_defined(Predicates) : sequence_or_list(predname)
    # "Specifies that each predicate in @var{Predicates} is
      @em{impl}icitly @em{defined} in the current prolog source,
      either because it is a builtin predicate or because it is
      defined in a C file.  Otherwise, a warning is signaled by
      the compiler when an exported predicate is not defined in
      the module or imported from other module.".
:- impl_defined(impl_defined/1).

% ---------------------------------------------------------------------------

:- export(modulename/1).
:- doc(doinclude, modulename/1).
:- doc(modulename/1, "A module name is an atom, not containing
    characters `:' or `$'.  Also, @tt{user} and @tt{multifile} are
    reserved, as well as the module names of all builtin modules
    (because in an executable all modules must have distinct
    names).").

:- prop modulename(M) + regtype # "@var{M} is a module name (an atom).".

modulename(M) :- atm(M).

% ---------------------------------------------------------------------------

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
    list(argspec, A),
    M =.. [F|A]. % TODO: This is not a regtype!
metaspec(M) :-
    M =.. [F|A], % TODO: This is not a regtype!
    atm(F),
    list(argspec, A).

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

% ---------------------------------------------------------------------------

:- doc(doinclude,initialization/1).
:- decl initialization(Goal) : cgoal + iso
    # "@var{Goal} will be executed at the start of the execution of
      any program containing the current code. The initialization of a
      module/file never runs before the initializations of the modules
      from which the module/file imports (excluding circular 
      dependencies).".
:- impl_defined(initialization/1).

:- doc(doinclude,on_abort/1).
:- decl on_abort(Goal) : cgoal
    # "@var{Goal} will be executed after an abort of the execution of
      any program containing the current code.".
:- impl_defined(on_abort/1).
