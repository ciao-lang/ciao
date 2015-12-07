:- module(_, [], [ciaobld(bundlehooks)]).

:- doc(title,  "Bundle Hooks for LPdoc").
:- doc(author, "Ciao Development Team").

'$builder_hook'(desc_name('LPdoc')).

'$builder_hook'(manual_dir(as('doc', 'lpdoc'))). % TODO: call it manual(Name, SettingsFile)
'$builder_hook'(readme_path(as('doc/readmes/INSTALLATION_LPDOC', 'INSTALLATION'))). % TODO: call it readme(RelPath, Source)
'$builder_hook'(readme_path(as('doc/readmes/README_LPDOC', 'README'))).

% ============================================================================

:- use_module(ciaobld(ciaoc_aux), [build_libs/2]).

'$builder_hook'(build_libraries) :-
	build_libs(lpdoc, 'src').

'$builder_hook'(build_bin) :-
	bundleitem_do(lpdoccl, lpdoc, build_nodocs).

'$builder_hook'(prebuild_nodocs) :-
	generate_version_auto_lpdoc.

% TODO: just say cmd('cmds/lpdoccl', [...])
'$builder_hook'(lpdoccl:item_def( 
    cmds_list(lpdoc, bundle_src(lpdoc)/'cmds', [
        'lpdoccl'-[
          output='lpdoc', % (executable will be called 'lpdoc')
	  plexe,
	  final_ciaoc
	]
    ]))).

'$builder_hook'(install) :- bundleitem_do(only_global_ins(~lpdoc_desc), lpdoc, install).

'$builder_hook'(uninstall) :- bundleitem_do(only_global_ins(~lpdoc_desc), lpdoc, uninstall).

lpdoc_desc := [
  lpdoccl,
  lib(lpdoc, 'src'),
  lib(lpdoc, 'lib')
].

%% ---------------------------------------------------------------------------

:- use_module(library(bundle/paths_extra), [fsR/2]).
:- use_module(ciaobld(builder_aux), [generate_version_auto/2]).

% TODO: generate a config_auto.pl and put there some config flags (for condcomp)

% TODO: generalize for all bundles
% TODO: change modiftime only if there are changes
% TODO: include config, etc. (for runtime)?
generate_version_auto_lpdoc :-
	Bundle = lpdoc,
	File = ~fsR(bundle_src(Bundle)/'src'/'version_auto.pl'),
	generate_version_auto(Bundle, File).

% ===========================================================================

:- doc(section, "Tests and Benchmarks").

:- use_module(ciaobld(ciaoc_aux), [runtests_dir/2]).

'$builder_hook'(runtests) :- !,
	runtests_dir(lpdoc, 'src').


