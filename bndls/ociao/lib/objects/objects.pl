:- package(objects).

%% O'Ciao: Object Oriented Programming in Ciao/Prolog
%%
%% SYNTAX FILE FOR OBJECT USAGE
%%
%% AUTHOR : Angel Fernandez Pineda
%% DATE   : March 1999

:- use_module(library(objects/objects_rt)).

:- op(700,xfx,[(new),(instance_of),(derived_from),(interface)]).
:- op(900,fx,[(destroy)]).

:- multifile 'class$used'/2.

:- new_declaration(use_class/1,on).
:- new_declaration(instance_of/2,on).

% TODO: uncertain priority: duplicates the compiler logic, not really compatible with other extensions
:- load_compilation_module(library(objects/objects_tr)).
:- add_sentence_trans(objects_tr:obj_sentence_trans/3, 8110).
:- add_clause_trans(objects_tr:obj_clause_trans/3, 8110).
