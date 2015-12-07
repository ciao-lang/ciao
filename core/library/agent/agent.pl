:- package(agent).
%:- export(main/0).
:- use_package(andprolog).
:- doc(nodoc,andprolog).

:- include(agent_ops).

:- load_compilation_module(library(agent/agent_tr)).
:- add_sentence_trans(agent_tr:agent_s/3, 750).

%:- use_module(library(actmods/actmod_server),[actmodmain/0]).

%main:- actmod_server:actmodmain.

:- use_module(library(agent/agent_call)).

% TODO: Do not use initialization here (at least, add priorities)
:- initialization(agent&&).
