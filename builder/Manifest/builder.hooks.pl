:- module(_, [], [ciaobld(bundlehooks)]).

:- doc(title,  "Bundle Hooks for CiaoBuilder").

'$builder_hook'(manual_dir(as('doc', 'ciao_builder'))).

'$builder_hook'(bundle_def([
  cmds,
  lib('src'),
  lib('sh_src') % TODO: not for windows!
])).

'$builder_hook'(cmds:item_def( 
  cmd('ciao_builder', [main='cmds/ciao_builder'])
)).

