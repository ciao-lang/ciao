:- package(bundlehooks).

% Package for the definition of bundle hooks
:- use_package(fsyntax).
:- use_package(hiord).
:- use_package(assertions).
:- use_package(regtypes).
:- use_package(isomodes).

:- load_compilation_module(ciaobld(bundlehooks/bundlehooks_tr)).
:- add_sentence_trans(bundlehooks_tr:defdep/3, 320).

:- use_module(ciaobld(messages_aux),
	[cmd_message/3, normal_message/2, verbose_message/2]).
:- use_module(ciaobld(builder_cmds)).

:- discontiguous '$bundlehook_decl'/2.
:- discontiguous '$bundlehook_do'/2.

% Hooks for bundle description
:- include(ciaobld(bundlehooks/bundlehooks_defs)).

% Bundle configuration
:- use_module(library(bundle/bundle_params), [bundle_param_value/2]).


