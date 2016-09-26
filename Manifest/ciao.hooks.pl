:- module(_, [], [ciaobld(bundlehooks)]).

:- doc(title,  "Bundle Hooks for Ciao").
:- doc(author, "Ciao Development Team").

'$builder_hook'(desc_name('Ciao')). % the whole system...

:- doc(section, "Tests and Benchmarks").

:- use_module(ciaobld(ciaoc_aux), [runtests_dir/2]).

% TODO: accept input tag and tags per directory and do (DirTags |- FilterTags)
%   e.g., [expensive] |- cheap is false
%   e.g., [] |- cheap is true
'$builder_hook'(test) :- !,
	% Run ISO-prolog tests
	% TODO: acceptance tests?
	runtests_dir(ciao, 'tests/iso_tests').
