:- module(openpg, [], [assertions, regtypes, fsyntax]).

:- doc(title, "Open a Ciao playground").
:- doc(author, "The Ciao Development Team").

:- doc(module, "This module provides predicates to open documents
   using the Ciao Playground. It relies on @lib{opendoc} to open
   arbritrary URLs.

   This is just a small client library. The playground code is
   distributed seperately in the @tt{ciao_playground} bundle.
").

:- use_module(library(opendoc)).
:- use_module(library(streams), [fixed_absolute_file_name/3]).

:- export(pgloc/1).
:- regtype pgloc/1 # "Ciao Playground location:
@begin{itemize}
@item @tt{ciaolang}: alias for @tt{https://ciao-lang.org/playground}
@item @tt{local}: alias for @tt{http://localhost:8001/playground}
  (development local playground, see @tt{ciao-serve-mt})
@end{itemize}
".
pgloc(ciaolang).
pgloc(local).

pg_url(ciaolang, 'https://ciao-lang.org/playground').
pg_url(local, 'http://localhost:8001/playground').

:- export(openpg/1).
:- pred openpg(PGLoc) # "Open a new Ciao Playground".
openpg(PGLoc) :-
    openpg(PGLoc, '').

:- export(openpg/2).
:- pred openpg(PGLoc, Path) # "Open a Ciao Playground from the
   specified location".

openpg(PGLoc, Path) :-
    ( pg_url(PGLoc, URL) -> true
    ; throw(error(bad_pgloc, openpg/2))
    ),
    ( Path = '' ->
        opendoc(URL)
    ; PGLoc = local ->
        Path1 = ~fixed_absolute_file_name(Path, '.'),
        path_split(Path1, _, Name),
        create_site_symlink(Path1, Name),
        URL2 = ~atom_concat(URL, ~atom_concat('#/tmp/', Name)),
        opendoc(URL2)
    ; throw(error(unsupported_path, openpg/2))
    ).

% ---------------------------------------------------------------------------
% TODO: implement a better way; use FileSystem API?

:- use_module(library(pathnames)).
:- use_module(library(system),[copy_file/3]).
:- use_module(library(system_extra),[mkpath/1]).

:- use_module(ciaobld(config_common), [site_root_dir/1]).

% Create symbolic link under site tmp
create_site_symlink(OrigPath, DestName) :-
    SiteDir = ~site_root_dir,
    TmpSite = ~path_concat(SiteDir, 'tmp'),
    mkpath(TmpSite),
    DestPath = ~path_concat(TmpSite, DestName),
    copy_file(OrigPath, DestPath, [overwrite, symlink]).
