:- package(class).

%% O'Ciao: Object Oriented Programming in Ciao/Prolog
%%
%% SYNTAX FILE FOR CLASSES
%%
%% AUTHOR : Angel Fernandez Pineda
%% DATE   : July 1999

:- use_module(library(class/class_rt)).
:- use_module(library(class/virtual)).

:- new_declaration(inherit_class/1,on).
:- new_declaration(implements/1,on).
:- new_declaration(inheritable/1,on).
:- new_declaration(public/1,on).
:- new_declaration(virtual/1,on).
:- new_declaration(persistent/1,on).

% The following declarations are for internal use:
:- new_declaration(method/1,on).
:- new_declaration(attribute/1,on).
:- new_declaration(super/1,on).

:- op(1150,fx,[(public),(inheritable),(virtual),(method)]).
:- op(900,fy,[(inherited)]).

% TODO: uncertain priority: duplicates the compiler logic, not really compatible with other extensions
:- load_compilation_module(library(class/class_tr)).
:- add_sentence_trans(class_tr:class_sentence_translation/3, 8110).
:- add_clause_trans(class_tr:class_clause_translation/3, 8110).
