:- module(_, [], [ciaobld(bundlehooks)]).

:- doc(title,  "Bundle Hooks for CiaoJS").

'$builder_hook'(desc_name('CiaoJS')).

'$builder_hook'(manual_dir(as('doc', 'ciaojs'))).

% ============================================================================

:- use_module(ciaobld(ciaoc_aux), [build_libs/2]).

'$builder_hook'(build_libraries) :-
	true.
	%build_libs(ciaojs, 'src'). % TODO: NOCOMPILE ignored?

'$builder_hook'(install) :- bundleitem_do(only_global_ins(~ciaojs_desc), ciaojs, install).

'$builder_hook'(uninstall) :- bundleitem_do(only_global_ins(~ciaojs_desc), ciaojs, uninstall).

ciaojs_desc := [
  enginejs,
  lib(ciaojs, 'src')
].

% (used from 'ciaobase' target)
'$builder_hook'(enginejs:build_nodocs) :- bundleitem_do(enginejs, ciaojs, build_nodocs).
'$builder_hook'(enginejs:build_docs) :- !.
'$builder_hook'(enginejs:clean_norec) :- !, bundleitem_do(enginejs, ciaojs, clean_norec).
'$builder_hook'(enginejs:install) :- !, bundleitem_do(enginejs, ciaojs, install).
'$builder_hook'(enginejs:uninstall) :- !, bundleitem_do(enginejs, ciaojs, uninstall).

'$builder_hook'(enginejs:item_def([
    eng(bundle_src(ciaojs)/'src'/'ciaoenginejs', [
        usepath(bundle_src(core)/'engine'),
        cross('LINUX', x86_JS) % Emscripten
    ])
])).

