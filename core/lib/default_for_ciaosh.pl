:- package(default_for_ciaosh).

:- use_module(library(default_predicates)).

:- use_package(dcg).
% TODO: It should be:
%  :- use_package(library(dcg/dcg_phrase)).
:- use_module(library(dcg/dcg_phrase_rt), [phrase/2, phrase/3]).
'\6\call_from_phrase'(X) :- call(X).

:- use_package(condcomp).
% (used to write packages compatible with the toplevel)
:- compilation_fact('SHELL').

