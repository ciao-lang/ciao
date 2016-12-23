% (included file)

:- doc(section, "PiLLoW bundle").

% ---------------------------------------------------------------------------
% TODO: Depends on LPdoc options!

:- bundle_flag(pillow_base_htmldir, [
    comment("Base for PiLLoW HTML assets"),
    details(
      % .....................................................................
      "Base directory for PiLLoW HTML assets (by default same as for lpdoc)."),
    rule_default(DefValue, flag(lpdoc:htmldir(DefValue))),
    %
    interactive
]).

:- bundle_flag(pillow_base_htmlurl, [
    comment("Base URL for PiLLoW HTML assets"),
    details(
      % .....................................................................
      "Base URL for PiLLoW HTML assets (by default same as for lpdoc)."),
    rule_default(DefValue, flag(lpdoc:htmlurl(DefValue))),
    %
    interactive
]).

% ---------------------------------------------------------------------------

:- use_module(library(bundle/bundle_flags), [get_bundle_flag/2]).
:- use_module(library(bundle/bundle_paths), [bundle_path/3]).
:- use_module(library(pathnames), [path_concat/3]).
:- use_module(library(write), [portray_clause/2]).

pillow_base_htmldir := ~get_bundle_flag(core:pillow_base_htmldir).
pillow_base_htmlurl := ~get_bundle_flag(core:pillow_base_htmlurl).

pillow_destname := 'pillow'.

pillow_desturl := ~path_concat(~pillow_base_htmlurl, ~pillow_destname).
pillow_destdir := ~path_concat(~pillow_base_htmldir, ~pillow_destname).

% TODO: subtarget for installation
'$builder_hook'(pillow:files_from('library/pillow/images', ~pillow_destdir, [del_rec])).
% Prepare source for build
'$builder_hook'(pillow:prebuild_bin) :- icon_address_auto.
'$builder_hook'(pillow:build_bin) :- true. % (force no build)

% TODO: ask bundle instead
icon_address_auto :-
	open(~bundle_path(core, 'library/pillow/icon_address_auto.pl'), write, OS),
	portray_clause(OS, icon_base_address(~pillow_desturl)),
	close(OS).

