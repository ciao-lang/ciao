:- module(syntax_extensions, [], [assertions]).

:- doc(title, "Extending the syntax").

:- doc(author, "Daniel Cabeza").

:- doc(usage, "These directives are builtin in Ciao, so nothing special
   has to be done to use them.").

:- doc(module, "This chapter documents the @concept{builtin
   directives} in Ciao for extending the syntax of source files.

   Note that the @concept{ISO-Prolog} directive
   @decl{char_conversion/2} is not implemented, since Ciao does not
   (yet) have a character conversion table.").

:- doc(doinclude,op/3).
:- true decl op(Priority, Op_spec, Operator) :
        int * operator_specifier * atm_or_atm_list + iso
        # "Updates the @concept{operator table} for reading the terms in
          the rest of the current text, in the same way as the builtin
          @pred{op/3} does.  Its scope is local to the current text.
          Usually included in @concept{package file}s.".

:- doc(doinclude,new_declaration/1).
:- true decl new_declaration(Predicate) : predname
        # "Declares @var{Predicate} to be a valid declaration in the
          rest of the current text.  Such declarations are simply
          ignored by the compiler or top level, but can be used by other
          code processing programs such as an automatic documentator.
          Also, they can easily translated into standard code (a set of
          facts and/or rules) by defining a suitable @concept{expansion}
          (e.g., by @decl{add_sentence_trans/1}, etc.). This is
          tipically done in @concept{package file}s.

          Equivalent to @tt{new_declaration(Predicate, off)}.".


:- doc(doinclude,new_declaration/2).
:- true decl new_declaration(Predicate, In_Itf) : predname * switch
        # "Declares @var{Predicate} to be a valid declaration in the
          rest of the current text.  Such declarations will be
          included in the @concept{interface file} for this file if
          @var{In_Itf} is 'on', not if it is 'off'. Including such
          declarations in interface files makes them visible while processing
          other modules which make use of this one.".

:- doc(doinclude,load_compilation_module/1).
:- true decl load_compilation_module(File) : sourcename
        # "Loads code defined in @var{File} into the compiler, usually
          including predicates which define translations of clauses,
          sentences, and terms, for use with the declarations
          @decl{add_sentence_trans/2} and similar ones. The
          application order of translations is determined by ascending
          @em{priority} numbers. Normally included in
          @concept{package file}s.".

:- doc(doinclude,add_sentence_trans/2).
:- true decl add_sentence_trans(Predicate, Priority) :
	translation_predname * int
        # "Starts a translation, defined by @var{Predicate}, of the
          terms read by the compiler in the rest of the current text.
          For each subsequent term read by the compiler, the
          translation predicate is called to obtain a new term which
          will be used by the compiler as if it where the term present
          in the file.  If the call fails, the term is used as such.
          A list may be returned also, to translate a single term into
          several terms.  Before calling the translation predicate
          with actual program terms, it is called with an input of
          @tt{0} to give an opportunity of making initializations for
          the module, discarding the result (note that normally a 0
          could not be there).  @var{Predicate} must be exported by a
          module previously loaded with a
          @decl{load_compilation_module/1} declaration.  Normally
          included in @concept{package file}s.".

:- doc(doinclude,add_term_trans/2).
:- true decl add_term_trans(P, Priority) : translation_predname * int
        # "Starts a translation, defined by @var{Predicate}, of the
          terms and sub-terms read by the compiler in the rest of the
          current text.  This translation is performed after all
          translations defined by @decl{add_sentence_trans/1} are done.
          For each subsequent term read by the compiler, and recursively
          any subterm included, the translation predicate is called
          to possibly obtain a new term to replace the old one.
          Care must be taken of not introducing an endless loop of
          translations.  @var{Predicate} must be exported by a module
          previously loaded with a @decl{load_compilation_module/1}
          declaration.  Normally included in @concept{package file}s.".

:- doc(doinclude,add_goal_trans/2).
:- true decl add_goal_trans(Predicate, Priority) :
	translation_predname * int
        # "Declares a translation, defined by @var{Predicate}, of the
          goals present in the clauses of the current text.  This
          translation is performed after all translations defined by
          @decl{add_sentence_trans/1} and @decl{add_term_trans/1} are
          done.  For each clause read by the compiler, the
          translation predicate is called with each goal present in the
          clause to possibly obtain other goal to substitute the
          original one, and the translation is subsequently applied to
          the resulting goal.  Care must be taken of not introducing an
          endless loop of translations.  @var{Predicate} must be
          exported by a module previously loaded with a
          @decl{load_compilation_module/1} declaration.  Bear in mind
          that this type of translation noticeably slows down
          compilation.  Normally included in @concept{package file}s.".

:- doc(doinclude,add_clause_trans/2).
:- true decl add_clause_trans(Predicate, Priority) :
	translation_predname * int
        # "Declares a translation, defined by @var{Predicate}, of the
          clauses of the current text.  The translation is performed
          before @decl{add_goal_trans/1} translations but after
          @decl{add_sentence_trans/1} and @decl{add_term_trans/1}
          translations. The usefulness of this translation is that
          information of the interface of related modules is available
          when it is performed.  For each clause read by the compiler,
          the translation predicate is called with the first argument
          instantiated to a structure @tt{clause(Head,Body)}, and the
          predicate must return in the second argument a similar
          structure, without changing the functor in @tt{Head} (or fail,
          in which case the clause is used as is).  Before executing the
          translation predicate with actual clauses it is called with an
          input of @tt{clause(0,0)}, discarding the result.".

:- doc(doinclude, translation_predname/1).

:- doc(translation_predname/1, "A translation predicate is a predicate
        of arity 2 or 3 used to make compile-time translations.  The compiler
        invokes a translation predicate instantiating its first argument with
        the item to be translated, and if the predicate is of arity 3 its
        third argument with the name of the module where the translation is
        done.  If the call is successful, the second argument is used as if
        that item were in the place of the original, else the original item
        is used.").

:- prop translation_predname(P) + regtype
        # "@var{P} is a translation predicate spec (has arity 2 or 3).".

translation_predname(F/A) :- atm(F), two_or_three(A).

:- prop two_or_three/1 + regtype.

two_or_three(2).
two_or_three(3).

:- prop switch(S) + regtype # "@var{S} is 'on' or 'off'".

switch(on).
switch(off).
