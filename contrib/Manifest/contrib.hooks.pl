:- module(_, [], [ciaobld(bundlehooks)]).

:- doc(title,  "Bundle Hooks for Contrib libraries").
:- doc(author, "Ciao Development Team").

'$builder_hook'(desc_name('Contrib')).

% ===========================================================================

:- use_module(library(bundle/bundle_flags), [get_bundle_flag/2]).

% TODO: This should be part of ciao_builder, not config hooks
:- use_module(ciaobld(bundle_configure), [
    foreign_config_var/3,
    foreign_config_version/2
]).

:- use_module(library(bundle/paths_extra), [fsR/2]).

% ============================================================================

:- include(.('profiler.hooks')).
:- include(.('timingmodel.hooks')).
:- include(.('gsl.hooks')).
:- include(.('ppl.hooks')).
:- include(.('mathematica.hooks')).

% ============================================================================

:- doc(section, "Build").
% (engine, libraries, and compiler)

:- use_module(ciaobld(ciaoc_aux), [build_libs/2]).

'$builder_hook'(build_libraries) :- build_libs(contrib, 'library').

'$builder_hook'(build_bin) :- bundleitem_do(contrib_cmds, contrib, build_nodocs).

% Prepare source for build
% (e.g., for automatically generated code, foreign interfaces, etc.)
'$builder_hook'(prebuild_nodocs) :-
	bundleitem_do([mathlibs, ppl, mathematica, timingmodel], contrib, prebuild_nodocs).

% ============================================================================

:- doc(section, "Installation").

'$builder_hook'(install) :- bundleitem_do(only_global_ins(~contrib_desc), contrib, install).

'$builder_hook'(uninstall) :- bundleitem_do(only_global_ins(~contrib_desc), contrib, uninstall).

contrib_desc := [
  %
  contrib_cmds,
  %
  lib(contrib, 'library')
].

% ===========================================================================

:- doc(section, "Tests and Benchmarks").
% TODO: Add bundle defs for unit tests, integration tests, regression
%   tests, etc.

:- use_module(ciaobld(ciaoc_aux), [runtests_dir/2]).

'$builder_hook'(runtests) :- !,
	runtests_dir(contrib, 'library').

% ===========================================================================
% Enumeration of the standalone utilities in */cmds/
% TODO: Generalize and split

:- use_module(library(aggregates), [findall/3]).

cmds_contrib_dir := bundle_src(contrib)/cmds.

'$builder_hook'(contrib_cmds:item_def(
	  cmds_list(contrib, ~cmds_contrib_dir, ~contrib_cmds))).

contrib_cmds := ~findall(B-[K], contrib_cmd(B, K)).

% TODO: really distribute those utils?
contrib_cmd('synch_actions', plexe).
contrib_cmd('cleandirs', plexe).


