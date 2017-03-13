:- module(pbundle_gen_src, [], [assertions, fsyntax]).

:- doc(title,  "Source distributions").
:- doc(author, "Ciao Development Team").
:- doc(module, "Generate clean source distributions.").

:- use_module(ciaobld(pbundle_generator)).

% (hooks for gen_pbundle)
:- include(ciaobld(pbundle_gen_hookdefs)).

% (hook)
gen_pbundle_hook(src, Target, _Options) :- !,
	dist_gen_commit_info(Target),
	gen_pbundle_common(Target, src, [tgz, tbz]).
gen_pbundle_hook(tgz, Target, _Options) :- !,
	dist_gen_commit_info(Target),
	gen_pbundle_common(Target, src, [tgz]).
gen_pbundle_hook(tbz, Target, _Options) :- !,
	dist_gen_commit_info(Target),
	gen_pbundle_common(Target, src, [tbz]).

