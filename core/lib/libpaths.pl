:- module(libpaths, [get_alias_path/0], [assertions]).

:- doc(title, "Customizing library paths and path aliases").

:- doc(author,"Daniel Cabeza").

:- doc(module, "This library provides means for customizing, from
   environment variables, the libraries and @concept{path
   alias}es known by an executable.  Many applications of Ciao,
   including @apl{ciaoc}, @apl{ciaosh}, and @apl{ciao-shell} make use of
   this library.  Note that if an executable is created dynamic, it will
   try to load its components at startup, before the procedures of this
   module can be invoked, so in this case all the components should be
   in standard locations.").

:- use_module(library(system)).
:- use_module(library(lists)).

:- doc(library_directory/1, "See @ref{Basic file/stream handling}.").
:- doc(file_search_path/2, "See @ref{Basic file/stream handling}.").


:- trust success file_search_path(X,Y) => (gnd(X), gnd(Y)).

:- trust success library_directory(X) => gnd(X).
:- multifile file_search_path/2, library_directory/1.
:- dynamic file_search_path/2, library_directory/1.

:- doc(get_alias_path, "Consult the environment variable
   @tt{CIAOALIASPATH} and add facts to predicates
   @pred{library_directory/1} and @pred{file_search_path/2} to define
   new library paths and @concept{path alias}es.  The format of
   @tt{CIAOALIASPATH} is a sequence of paths or alias assignments
   separated by a @concept{path list separator character} (see
   @pred{extract_paths/2}), an alias assignment is the name of the
   alias, an @tt{=} and the path represented by that alias (no blanks
   allowed).  For example, given
@begin{verbatim}
   CIAOALIASPATH=/home/bardo/ciao:contrib=/usr/local/lib/ciao
@end{verbatim}
   the predicate will define @tt{/home/bardo/ciao} as a library path
   and @tt{/usr/local/lib/ciao} as the path represented by
   @tt{contrib}.").

get_alias_path :-
        getenvstr('CIAOALIASPATH', ALIAS), !,
        extract_paths(ALIAS, Paths),
        record_alias(Paths).
get_alias_path.

record_alias([]).
record_alias([AliasPath|Paths]) :-
        append(AliasStr, "="||PathStr, AliasPath), !,
        atom_codes(Alias, AliasStr),
        atom_codes(Path, PathStr),
        asserta_fact(file_search_path(Alias, Path)),
        record_alias(Paths).
record_alias([LibStr|Paths]) :-
        atom_codes(Lib, LibStr),
        asserta_fact(library_directory(Lib)),
        record_alias(Paths).
