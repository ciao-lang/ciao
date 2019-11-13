:- module(pbundle_gen_bin, [], [assertions, fsyntax]).

:- doc(title,  "Binary distributions").
:- doc(author, "The Ciao Development Team").

:- doc(module, "Binary distributions contain precompiled code (both
   portable and platform dependent.").

:- use_module(ciaobld(pbundle_generator)).

% TODO: Make sure that this works both as a mechanism to distribute
%   compiled workspaces (and instype=local) and installed bundles with
%   a destdir (instype=global)

% (hooks for gen_pbundle)
:- include(ciaobld(pbundle_gen_hookdefs)).

% (hook)
gen_pbundle_hook(bin, Target, _Options) :- !,
    dist_gen_commit_info(Target),
    gen_pbundle_common(Target, bin, [tgz, tbz]).
gen_pbundle_hook(bin_tgz, Target, _Options) :- !,
    dist_gen_commit_info(Target),
    gen_pbundle_common(Target, bin, [tgz]).
gen_pbundle_hook(bin_tbz, Target, _Options) :- !,
    dist_gen_commit_info(Target),
    gen_pbundle_common(Target, bin, [tbz]).
gen_pbundle_hook(noa, Target, _Options) :- !,
    dist_gen_commit_info(Target),
    gen_pbundle_common(Target, noa, [tgz, tbz]).
gen_pbundle_hook(noa_tgz, Target, _Options) :- !,
    dist_gen_commit_info(Target),
    gen_pbundle_common(Target, noa, [tgz]).
gen_pbundle_hook(noa_tbz, Target, _Options) :- !,
    dist_gen_commit_info(Target),
    gen_pbundle_common(Target, noa, [tbz]).
