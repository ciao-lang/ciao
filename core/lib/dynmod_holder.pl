:- package(dynmod_holder).

% @title Define a dynamic module holder
% @author Jose F. Morales
%
% @module A module using this package implements a context to load
% modules dynamically. That module isolates the effects of
% meta-programming to just this particular module, without interfering
% with the rest of the code.
%
% (similar to optim_comp's translation_module_holder)

:- use_module(library(compiler), [use_module/1, unload/1]).

:- export(do_use_module/1).
do_use_module(A) :- use_module(A).

:- export(do_unload/1).
do_unload(A) :- unload(A).

