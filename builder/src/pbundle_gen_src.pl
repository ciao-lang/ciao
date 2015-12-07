:- module(pbundle_gen_src, [], [assertions, fsyntax]).

:- doc(title,  "Source distributions").
:- doc(author, "Ciao Development Team").
:- doc(module, "Generate clean source distributions.").

:- use_module(library(bundle/paths_extra), [fsR/2]).
:- use_module(ciaobld(bundle_hash), [
	bundle_versioned_packname/2, gen_bundle_commit_info/1]).
:- use_module(ciaobld(pbundle_generator)).

% (hooks for gen_pbundle)
:- include(ciaobld(pbundle_gen_hookdefs)).

% (hook)
gen_pbundle_hook(src, Bundle, _Options) :- !,
	gen_bundle_commit_info(Bundle),
	gen_pbundle_common(Bundle, src, [tgz, tbz]).
gen_pbundle_hook(tgz, Bundle, _Options) :- !,
	gen_bundle_commit_info(Bundle),
	gen_pbundle_common(Bundle, src, [tgz]).
gen_pbundle_hook(tbz, Bundle, _Options) :- !,
	gen_bundle_commit_info(Bundle),
	gen_pbundle_common(Bundle, src, [tbz]).

