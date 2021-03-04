:- package(sr).
% Generic package to select custom search rules

:- discontiguous '$bfcl'/3.
:- discontiguous '$bfpred'/1.

:- load_compilation_module(library(bf/sr_tr)).
:- add_sentence_trans(sr_tr:sent_tr/3, 750). % TODO: Probably not right priority

% TODO: Priorities are not enough to make it work with other
%       translations, such as fsyntax. See "Modular Extensions for
%       Modular (Logic) Languages (LOPSTR'11)" paper for details.
