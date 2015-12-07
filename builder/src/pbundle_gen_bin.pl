:- module(pbundle_gen_bin, [], [assertions, fsyntax]).

:- doc(title,  "Binary distributions").
:- doc(author, "Ciao Development Team").

:- doc(module, "Binary distributions contains precompiled code (both
   portable and platform dependent.").

:- use_module(ciaobld(pbundle_generator)).
:- use_module(ciaobld(bundle_hash), [gen_bundle_commit_info/1]).

% TODO: THIS IS WRONG! distribute binaries based on instype=global with a destdir

% (hooks for gen_pbundle)
:- include(ciaobld(pbundle_gen_hookdefs)).

% (hook)
gen_pbundle_hook(bin, Bundle, _Options) :- !,
	gen_bundle_commit_info(Bundle),
	gen_pbundle_common(Bundle, bin, [tgz, tbz]).
gen_pbundle_hook(bin_tgz, Bundle, _Options) :- !,
	gen_bundle_commit_info(Bundle),
	gen_pbundle_common(Bundle, bin, [tgz]).
gen_pbundle_hook(bin_tbz, Bundle, _Options) :- !,
	gen_bundle_commit_info(Bundle),
	gen_pbundle_common(Bundle, bin, [tbz]).
gen_pbundle_hook(noa, Bundle, _Options) :- !,
	gen_bundle_commit_info(Bundle),
	gen_pbundle_common(Bundle, noa, [tgz, tbz]).
gen_pbundle_hook(noa_tgz, Bundle, _Options) :- !,
	gen_bundle_commit_info(Bundle),
	gen_pbundle_common(Bundle, noa, [tgz]).
gen_pbundle_hook(noa_tbz, Bundle, _Options) :- !,
	gen_bundle_commit_info(Bundle),
	gen_pbundle_common(Bundle, noa, [tbz]).
