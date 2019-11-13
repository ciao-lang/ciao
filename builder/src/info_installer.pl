:- module(info_installer, [], [assertions, regtypes, fsyntax]).

:- doc(title, "Installation of info files").
:- doc(author, "The Ciao Development Team").

:- doc(module, "This module defines predicates to update (register and
   unregister) the @tt{dir} Info directory files.").

% NOTE: LPdoc delegates to the builder the task of (un)installing info
% files.

:- use_module(engine(stream_basic), [absolute_file_name/2]).
:- use_module(library(pathnames), [path_concat/3]).
:- use_module(library(process), [process_call/3]).
:- use_module(library(system), [file_exists/1, copy_file/3]).

:- export(dirfile_install_info/2).
:- pred dirfile_install_info(InfoFile, TargetDir) 
   # "Includes @var{InfoFile} in the @tt{dir} file at
     @var{TargetDir}. If no @tt{dir} file exist, a default one is
     created.".

% TODO: check behavior on error
dirfile_install_info(InfoFile, TargetDir) :-
    DirFile = ~info_dirfile(TargetDir),
    % Create a info 'dir' file, if it does not exist
    ensure_dirfile(DirFile),
    % Then install InfoFile
    process_call(path('install-info'), 
           [~atom_concat('--dir-file=', DirFile),
            InfoFile],
           [stderr(null), stdout(null)]).

:- export(dirfile_uninstall_info/2).
:- pred dirfile_uninstall_info(InfoFile, TargetDir) 
   # "Removes @var{InfoFile} from the @tt{dir} file at
     @var{TargetDir}.".

% TODO: check behavior on error
dirfile_uninstall_info(InfoFile, TargetDir) :-
    DirFile = ~info_dirfile(TargetDir),
    process_call(path('install-info'),
           ['--remove',
            ~atom_concat('--dir-file=', DirFile),
            InfoFile],
           [stderr(null), stdout(null), status(0)]).

info_dirfile(Path) := ~path_concat(Path, 'dir').

% TODO: section name in "infodir" must be synchronized with INFO-DIR-SECTION in .infoindex
ensure_dirfile(DirFile) :-
    ( file_exists(DirFile) ->
        true
    ; % TODO: 'infodir' should live in the lpdoc source (lpdoc/etc)
      copy_file(~absolute_file_name(ciaobld(infodir)),
        DirFile, [append])
    ).
