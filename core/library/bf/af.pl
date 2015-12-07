:- package(af).
% (See bf_doc for details about this package)

:- discontiguous '$bfcl'/3, '$bfpred'/1.

:- include(library(bf/ops)).

:- load_compilation_module(library(bf/aftr)).
:- add_sentence_trans(aftr:aftr/3, 750). % TODO: Probably not right priority

% TODO: Priorities are not enough to make it work with other
%       translations, such as fsyntax. See "Modular Extensions for
%       Modular (Logic) Languages (LOPSTR'11)" paper for details.
