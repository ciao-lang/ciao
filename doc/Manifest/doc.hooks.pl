:- module(_, [], [ciaobld(bundlehooks)]).

:- doc(title,  "Bundle Hooks for Ciao Documentation").

'$builder_hook'(bundle_def([
  manual('ciao', [main='reference/SETTINGS.pl']),
  manual('ciao_devel', [main='developers/SETTINGS.pl']),
  readme('INSTALLATION', [main='common/INSTALLATION_CIAO']),
  readme('INSTALLATION_Win32', [main='common/INSTALLATION_CIAO_Win32']),
  readme('README', [main='common/README_CIAO'])
])).  
