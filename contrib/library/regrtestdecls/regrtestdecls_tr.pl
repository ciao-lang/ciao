:- module(regrtestdecls_tr,[sentence_tr/4],[assertions]).

:- doc(title, "Regression testing syntax support").

:- doc(author, "Nataliia Stulova").
:- doc(author, "Jose F. Morales").

:- doc(module,"
This module provides and handles a regression testing extension to the
unittest library. When listed in the packages list of a module, allows
to transform @decl{regr_texec/1} declarations into test queries, and
writes all such queries of a module into an auxiliary file
@file{<modulename>_queries.pl}, which can be used later as input by the
@lib{regrtest} library.
").

:- doc(usage, ":- module(...,...,[...,regrtestdecls]).").

:- doc(bug,"
Planned syntactic extensions:
@begin{itemize}
@item @decl{regr_all_tests/0} : do regression on all test assertions;
@item @decl{regr_benchmark/1} : this is a benchmark (for integration
      with OptimComp tests);
@item @decl{regr_comp/1} : this is a regression test for the compiler
      itself;
@item @decl{regr_pp/1} : this is a regression test for ciaopp;
@item @decl{regr_mtsys/1} : this is a multi-system test;
@end{itemize}
").

:- doc(bug,"
We may also need something like @tt{:- regr_db(Subdir)} to specify the
relative location w.r.t. the @tt{regr-db} repo of the output for the
test.
").

:- use_module(library(write),          [write/2]).
:- use_module(library(compiler/c_itf), [defines_module/2]).

% -------------------------------------------------------- reading
:- data query/2.

sentence_tr(end_of_file, end_of_file, Mod,_Dic) :- !,
        dump_queries_to_file(Mod),
        retractall_fact(query(_,_)).
sentence_tr(:-(regr_texec(Query)),[], Mod,_Dic) :- !,
        assertz_fact(query(Query,Mod)).
sentence_tr(X, X,_Mod,_Dic).

% -------------------------------------------------------- writing
dump_queries_to_file(Mod) :-
        defines_module(Base,Mod),
        atom_concat(Base, '_queries.pl', OutFile),
        open(OutFile, write, IO),
        write_queries(IO,Mod),
        close(IO).

write_queries(IO,Mod) :-
        query(Q,Mod),
        write(IO,Q),
        write(IO,'.\n'),
        fail.
write_queries(_IO,_Mod).
