:- package(classic).
% Package implicitly used in Ciao modules defined with module/2
% (specially for backward compatibility with other Prolog systems)

:- use_package(runtime_ops). % TODO: Not modular! Alters runtime behavior of modules not compiled with the default package!
:- use_package(dcg).
:- use_package(library(dcg/dcg_phrase)).

% TODO: reexport or include "use_module"s here
:- use_module(library(classic/classic_predicates)).

