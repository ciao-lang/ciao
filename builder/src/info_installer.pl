:- module(info_installer, [], [assertions, regtypes, fsyntax]).

:- doc(title, "Installation of info files").
:- doc(author, "The CLIP group").

:- doc(module, "This module defines predicates to install @tt{info}
   files").

% TODO: It seems that both this module and builder/src/infodir
%       should be part of LPdoc.

:- use_module(library(process), [process_call/3]).
:- use_module(library(system), [file_exists/1, copy_file/3]).

:- export(dirfile_install_info/2).
:- pred dirfile_install_info(InstallDir, InfoFile) 
   # "Install @var{InfoFile} in the @tt{dir} file at
     @var{InstallDir}. If no @tt{dir} file exist, a default one is
     created.".

% TODO: behavior on error?
dirfile_install_info(InstallDir, InfoFile) :-
	DirFile = ~info_dirfile(InstallDir),
	% Create a info 'dir' file, if it does not exist
	ensure_dirfile(DirFile),
	process_call(path('install-info'), 
	       [~atom_concat('--dir-file=', DirFile),
		InfoFile],
	       [stderr(null), stdout(null)]).

:- export(dirfile_uninstall_info/2).
:- pred dirfile_uninstall_info(InstallDir, InfoFile) 
   # "Uninstall @var{InfoFile} from the @tt{dir} file at 
     @var{InstallDir}.".

% TODO: behavior on error?
dirfile_uninstall_info(InstallDir, InfoFile) :-
	DirFile = ~info_dirfile(InstallDir),
	process_call(path('install-info'),
	       ['--remove',
		~atom_concat('--dir-file=', DirFile),
		InfoFile],
	       [stderr(null), stdout(null), status(0)]).

info_dirfile(Path) := ~atom_concat(Path, '/dir').

% TODO: section name in "infodir" must be synchronized with INFO-DIR-SECTION in .infoindex
ensure_dirfile(DirFile) :-
	( file_exists(DirFile) ->
	    true
	; % TODO: 'infodir' should live in the lpdoc source (lpdoc/lib)
	  copy_file(~absolute_file_name(ciaobld(infodir)),
	    DirFile, [append])
	).
