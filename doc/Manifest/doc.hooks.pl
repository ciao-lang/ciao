:- module(_, [], [ciaobld(bundlehooks)]).

:- doc(title,  "Bundle Hooks for Ciao Documentation").
:- doc(author, "Ciao Development Team").

'$builder_hook'(desc_name('CiaoDocumentation')).

'$builder_hook'(manual_dir(as('reference', 'ciao'))).
'$builder_hook'(manual_dir(as('developers', 'ciao_devel'))).
'$builder_hook'(readme_path(as('common/INSTALLATION_CIAO', 'INSTALLATION'))).
'$builder_hook'(readme_path(as('common/INSTALLATION_CIAO_Win32', 'INSTALLATION_Win32'))).
'$builder_hook'(readme_path(as('common/README_CIAO', 'README'))).
