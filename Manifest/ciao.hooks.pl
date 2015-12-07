:- module(_, [], [ciaobld(bundlehooks)]).

:- doc(title,  "Bundle Hooks for Ciao").
:- doc(author, "Ciao Development Team").

'$builder_hook'(desc_name('Ciao')). % the whole system...

'$builder_hook'(manual_dir(as('doc/reference', 'ciao'))).
'$builder_hook'(manual_dir(as('doc/developers', 'ciao_devel'))).
'$builder_hook'(readme_path(as('doc/common/INSTALLATION_CIAO', 'INSTALLATION'))).
'$builder_hook'(readme_path(as('doc/common/INSTALLATION_CIAO_Win32', 'INSTALLATION_Win32'))).
'$builder_hook'(readme_path(as('doc/common/README_CIAO', 'README'))).
% TODO: Strange, use a prepare_doc or remove 
'$builder_hook'(readme_path('doc/common/NewUser')).

:- doc(section, "Tests and Benchmarks").

:- use_module(ciaobld(ciaoc_aux), [runtests_dir/2]).

% TODO: accept input tag and tags per directory and do (DirTags |- FilterTags)
%   e.g., [expensive] |- cheap is false
%   e.g., [] |- cheap is true
'$builder_hook'(runtests) :- !,
	% Run ISO-prolog tests
	% TODO: acceptance tests?
	runtests_dir(ciao, 'tests/iso_tests').
