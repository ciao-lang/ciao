:- package(bundlehooks).

% Package for the definition of bundle hooks
:- use_package(fsyntax).
:- use_package(hiord).
:- use_package(assertions).
:- use_package(regtypes).
:- use_package(isomodes).

:- load_compilation_module(ciaobld(bundlehooks/bundlehooks_tr)).
:- add_sentence_trans(bundlehooks_tr:defdep/3, 320).

:- discontiguous '$bundlehook_decl'/2.
:- discontiguous '$bundlehook_do'/2.
% '$bundleconfig_entry'(Name, Bundle, Definition).
:- discontiguous('$bundleconfig_entry'/3).

'$bundleconfig_entry'(_,_,_) :- fail.

% Hooks for bundle description
:- include(ciaobld(bundlehooks/bundlehooks_defs)).

:- use_module(ciaobld(bundlehooks/bundlehooks_rt)).
