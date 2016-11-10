:- module(_, [], [ciaobld(bundlehooks)]).

:- doc(title,  "Bundle Hooks for Contrib libraries").

'$builder_hook'(item_subs(['contrib/profiler', 'contrib/timingmodel', 'contrib/gsl', 'contrib/ppl', 'contrib/mathematica'])).

:- include(.('profiler.hooks')).
:- include(.('timingmodel.hooks')).
:- include(.('gsl.hooks')).
:- include(.('ppl.hooks')).
:- include(.('mathematica.hooks')).

'$builder_hook'(bundle_def([
  cmds,
  lib('library')
])).

% TODO: move to its own bundle
'$builder_hook'(cmds:item_def([
  cmd('cmds/synch_actions'),
  cmd('cmds/cleandirs')
])).

% ===========================================================================

:- doc(section, "Tests and Benchmarks").
% TODO: Add bundle defs for unit tests, integration tests, regression
%   tests, etc.

:- use_module(ciaobld(ciaoc_aux), [runtests_dir/2]).

'$builder_hook'(test) :- !,
	runtests_dir(contrib, 'library').

