:- package(mycin_rulebase).
%%----------------------------------------------------------------------
%%
%% MYCIN PROGRAMMING LIBRARY
%%
%% AUTHOR:  Angel Fernandez Pineda
%% DATE:    2002
%%
%% Distributed under Ciao Prolog License Terms
%%
%%----------------------------------------------------------------------

:- op(700,xfy,[(cf)]).

%%----------------------------------------------------------------------

:- use_module(mycin_rulebase_rt).
:- use_module(library(mycin/mycin_support)).
:- use_module(library(hiordlib), [map/3]). %% Needed to compute \+/1 goals.

%%----------------------------------------------------------------------

%% For internal use only.
:- new_declaration(mycin_pred/1,on).

%%----------------------------------------------------------------------

% NOTE: add_clause_trans/1 MUST be located before add_sentence_trans/1...
:- load_compilation_module(library(mycin_rulebase/mycin_tr)).
:- add_clause_trans(mycin_tr:mycin_clause_tr/3, 750). % TODO: Right priority?
:- add_sentence_trans(mycin_tr:mycin_sentence_tr/3, 750). % TODO: Right priority?
