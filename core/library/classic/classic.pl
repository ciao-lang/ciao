:- package(classic).
% Package implicitly used in Ciao modules defined with module/2
% (specially for backward compatibility with other Prolog systems)

:- use_package(runtime_ops). % TODO: Not modular! Alters runtime behavior of modules not compiled with the default package!

:- use_package(dcg).
:- if(defined('SHELL')).
:- use_module(library(dcg/dcg_phrase_rt), [phrase/2, phrase/3]).
'\6\call_from_phrase'(X) :- call(X).
:- else.
:- use_package(library(dcg/dcg_phrase)).
:- endif.

:- use_module(library(default_predicates)).

