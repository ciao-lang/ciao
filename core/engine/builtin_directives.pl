:- module(builtin_directives, [], [assertions]).

% ----------------------------------------------------------------------------
:- doc(title, "Basic builtin directives").

:- doc(author, "Daniel Cabeza").

:- doc(usage, "These directives are builtin in Ciao, so nothing special
   has to be done to use them.").

:- doc(module, "This chapter documents the basic @concept{builtin
   directives} in Ciao, additional to the documented in other chapters.
   These @concept{directives} are natively interpreted by the Ciao
   compiler (@apl{ciaoc}).

   Unlike in other Prolog systems, directives in Ciao are not goals to
   be @em{executed} by the compiler or top level. Instead, they are
   @em{read} and acted upon by these programs. The advantage of this
   is that the effect of the directives is consistent for executables,
   code loaded in the top level, code analyzed by the preprocessor,
   etc. 

   As a result, by default only the builtin directives or declarations
   defined in this manual can be used in user programs. However, it is
   possible to define new declarations @cindex{declarations, user
   defined} using the @decl{new_declaration/1} and
   @decl{new_declaration/2} directives (or using packages including
   them). Also, packages may define new directives via code
   translations.").

%  ----------------------------------------------------------------------------

:- doc(doinclude,multifile/1).
:- decl multifile(Predicates) : sequence_or_list(predname) + iso
        # "Specifies that each predicate in @var{Predicates} may have
          clauses in more than one file.  Each file that contains
          clauses for a @concept{multifile predicate} must contain a
          directive multifile for the predicate.  The directive should
          precede all clauses of the affected predicates, and also
          dynamic/data declarations for the predicate.  This directive
          is defined as a prefix operator in the compiler.".

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

:- doc(doinclude,impl_defined/1).
:- decl impl_defined(Predicates) : sequence_or_list(predname)
        # "Specifies that each predicate in @var{Predicates} is
          @em{impl}icitly @em{defined} in the current prolog source,
          either because it is a builtin predicate or because it is
          defined in a C file.  Otherwise, a warning is signaled by
          the compiler when an exported predicate is not defined in
          the module or imported from other module.".

:- doc(doinclude,redefining/1).
:- decl redefining(Predicate) : compat(predname)
        # "Specifies that this module redefines predicate
          @var{Predicate}, also imported from other module, or imports
          it from more than one module.  This prevents the compiler
          giving warnings about redefinitions of that predicate.
          @var{Predicate} can be partially (or totally) uninstantiated,
          to allow disabling those warnings for several (or all) predicates at
          once.".

:- doc(doinclude,initialization/1).
:- decl initialization(Goal) : callable + iso
        # "@var{Goal} will be executed at the start of the execution of
          any program containing the current code. The initialization of a
          module/file never runs before the initializations of the modules
          from which the module/file imports (excluding circular 
          dependences).".

:- doc(doinclude,on_abort/1).
:- decl on_abort(Goal) : callable
        # "@var{Goal} will be executed after an abort of the execution of
          any program containing the current code.".
