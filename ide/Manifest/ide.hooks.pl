:- module(_, [], [ciaobld(bundlehooks)]).

:- doc(title,  "Bundle Hooks for Ciao IDE").

'$builder_hook'(bundle_def([
  manual('ciao_ide', [main='doc/SETTINGS.pl'])
])).













