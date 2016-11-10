:- module(_, [], [ciaobld(bundlehooks)]).

:- doc(title,  "Bundle Hooks for LPdoc").

% ============================================================================

'$builder_hook'(prebuild_nodocs) :-
	generate_version_auto_lpdoc.

'$builder_hook'(bundle_def([
  cmds,
  lib('src'),
  lib('lib'),
  %
  manual('lpdoc', [main='doc/reference/SETTINGS.pl']),
  readme('INSTALLATION', [main='doc/readmes/INSTALLATION_LPDOC']),
  readme('README', [main='doc/readmes/README_LPDOC'])
])).

'$builder_hook'(cmds:item_def( 
  cmd('lpdoc', [main='cmds/lpdoccl'])
)).

%% ---------------------------------------------------------------------------

:- use_module(library(bundle/bundle_paths), [bundle_path/3]).
:- use_module(ciaobld(builder_aux), [generate_version_auto/2]).

% TODO: Add a version package instead?
% TODO: generate a config_auto.pl and put there some config flags (for condcomp)

% TODO: generalize for all bundles
% TODO: change modiftime only if there are changes
% TODO: include config, etc. (for runtime)?
generate_version_auto_lpdoc :-
	Bundle = lpdoc,
	File = ~bundle_path(Bundle, 'src/version_auto.pl'),
	generate_version_auto(Bundle, File).

% ===========================================================================

:- doc(section, "Tests and Benchmarks").

:- use_module(ciaobld(ciaoc_aux), [runtests_dir/2]).

'$builder_hook'(test) :- !,
	runtests_dir(lpdoc, 'src').


