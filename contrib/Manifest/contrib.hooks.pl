:- module(_, [], [ciaobld(bundlehooks)]).

:- doc(title,  "Bundle Hooks for Contrib libraries").

% ===========================================================================

:- use_module(library(bundle/bundle_flags), [get_bundle_flag/2]).

% TODO: This should be part of ciao_builder, not config hooks
:- use_module(ciaobld(bundle_configure), [
    foreign_config_var/3,
    foreign_config_version/2
]).

:- use_module(library(bundle/bundle_paths), [bundle_path/3]).

% ============================================================================

'$builder_hook'(item_subs(['contrib/profiler', 'contrib/timingmodel', 'contrib/gsl', 'contrib/ppl', 'contrib/mathematica'])).

:- include(.('profiler.hooks')).
:- include(.('timingmodel.hooks')).
:- include(.('gsl.hooks')).
:- include(.('ppl.hooks')).
:- include(.('mathematica.hooks')).

% ============================================================================

'$builder_hook'(bundle_def([
  cmds,
  lib('library')
])).

% TODO: move to its own bundle
'$builder_hook'(cmds:item_def(
    cmds_list('cmds', [
        'synch_actions'-[plexe],
        'cleandirs'-[plexe]
    ]))).

% ===========================================================================

:- doc(section, "Tests and Benchmarks").
% TODO: Add bundle defs for unit tests, integration tests, regression
%   tests, etc.

:- use_module(ciaobld(ciaoc_aux), [runtests_dir/2]).

'$builder_hook'(test) :- !,
	runtests_dir(contrib, 'library').

