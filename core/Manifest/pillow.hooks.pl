% (included file)

:- doc(section, "PiLLoW bundle").

:- use_module(library(pathnames), [path_concat/3]).
:- use_module(library(write), [portray_clause/2]).

pillow_base_htmldir := ~get_bundle_flag(core:pillow_base_htmldir).
pillow_base_htmlurl := ~get_bundle_flag(core:pillow_base_htmlurl).

pillow_destname := 'pillow'.

pillow_desturl := ~path_concat(~pillow_base_htmlurl, ~pillow_destname).
pillow_destdir := ~path_concat(~pillow_base_htmldir, ~pillow_destname).

'$builder_hook'(pillow:item_def([
    % TODO: subtarget for installation
    files_from('library/pillow/images', ~pillow_destdir, [del_rec])
    ])).
% Prepare source for build
'$builder_hook'(pillow:prebuild_nodocs) :- icon_address_auto.
'$builder_hook'(pillow:build_nodocs) :- true. % (force no build)

% TODO: ask bundle instead
icon_address_auto :-
	open(~bundle_path(core, 'library/pillow/icon_address_auto.pl'), write, OS),
	portray_clause(OS, icon_base_address(~pillow_desturl)),
	close(OS).

