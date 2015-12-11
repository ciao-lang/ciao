:- module(_, [], [ciaobld(bundlehooks)]).

:- doc(title,  "Bundle Hooks for CiaoBuilder").
:- doc(author, "Ciao Development Team").

'$builder_hook'(desc_name('CiaoBuilder')).

'$builder_hook'(manual_dir(as('doc', 'ciao_builder'))).

% ============================================================================

:- use_module(ciaobld(ciaoc_aux), [build_libs/2]).

'$builder_hook'(build_libraries) :-
	build_libs(builder, 'src').

'$builder_hook'(build_bin) :-
	bundleitem_do(ciao_builder, builder, build_nodocs).

% TODO: just say cmd('cmds/ciao_builder', [...])
'$builder_hook'(ciao_builder:item_def( 
    cmds_list(builder, bundle_src(builder)/'cmds', [
        'ciao_builder'-[
          output='ciao_builder', % (executable will be called 'builder')
	  plexe,
	  final_ciaoc
	]
    ]))).

'$builder_hook'(install) :- bundleitem_do(only_global_ins(~builder_desc), builder, install).

'$builder_hook'(uninstall) :- bundleitem_do(only_global_ins(~builder_desc), builder, uninstall).

builder_desc := [
  ciao_builder,
  lib(builder, 'src'),
  lib(builder, 'sh_src') % TODO: not for windows!
].

