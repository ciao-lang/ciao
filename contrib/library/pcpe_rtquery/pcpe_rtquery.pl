:- package(pcpe_rtquery).

% The PCPE runtime-query package
:- load_compilation_module(library(pcpe_rtquery/pcpe_rtquery_trans)).
:- add_sentence_trans(pcpe_rtquery_trans:pcpe_rtquery_tr/2, 750).

% this directive will be used to retrieve a runtime query for PCPE.
:- new_declaration(pcpe_rtquery/1, on). 
:- op(1150, fx, [pcpe_rtquery]).
