:- package(bundleconfig).

% Package for the definition of bundle configuration rules
:- use_package(fsyntax).
:- use_package(hiord).
:- use_package(assertions).
:- use_package(regtypes).
:- use_package(isomodes).

:- load_compilation_module(ciaobld(bundleconfig/bundleconfig_tr)).
:- add_sentence_trans(bundleconfig_tr:sent/3, 320).

:- include(ciaobld(bundleconfig/bundleconfig_defs)).

% '$bundleconfig_entry'(Name, Bundle, Definition).
:- discontiguous('$bundleconfig_entry'/3).



