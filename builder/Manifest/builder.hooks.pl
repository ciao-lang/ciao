:- module(_, [], [ciaobld(bundlehooks)]).

:- doc(title,  "Bundle Hooks for CiaoBuilder").

'$builder_hook'(bundle_def([
  cmds,
  lib('src'),
  lib('sh_src'), % TODO: not for windows!
  %
  manual('ciao_builder', [main='doc/SETTINGS.pl'])
])).

'$builder_hook'(cmds:item_def( 
  cmd('ciao_builder', [main='cmds/ciao_builder'])
)).

