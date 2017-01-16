:- module(_, [], [ciaobld(bundlehooks)]).

:- doc(title,  "Bundle Hooks for Contrib libraries").

% ===========================================================================
:- doc(section, "Configuration rules").

:- use_module(library(process), [process_call/3]).
:- use_module(library(system), [find_executable/2]).
:- use_module(library(bundle/bundle_paths), [bundle_path/3]).

:- discontiguous(m_bundle_foreign_config_tool/3).

% ===========================================================================
:- doc(section, "Build rules").

'$builder_hook'(lib('library')).

% TODO: move to its own bundle
'$builder_hook'(item_nested(cmds)).
'$builder_hook'(cmds:cmd('cmds/synch_actions')).
'$builder_hook'(cmds:cmd('cmds/cleandirs')).

% ===========================================================================
% (nested)

'$builder_hook'(item_nested(timingmodel)).
:- include(.('timingmodel.hooks')).

'$builder_hook'(item_nested(ppl)).
:- include(.('ppl.hooks')).

% ===========================================================================
:- doc(section, "Tests and Benchmarks").
% TODO: Add bundle defs for unit tests, integration tests, regression
%   tests, etc.

:- use_module(ciaobld(ciaoc_aux), [runtests_dir/2]).

'$builder_hook'(test) :- !,
	display(runtests_dir(contrib, 'library')), nl,
	runtests_dir(contrib, 'library').
