:- module(_, [], [ciaobld(bundlehooks)]).

:- doc(title,  "Bundle Hooks for CiaoBuilder").
:- doc(author, "Ciao Development Team").

'$builder_hook'(desc_name('CiaoBuilder')).

'$builder_hook'(manual_dir(as('doc', 'ciao_builder'))).
