:- module(example_static, [main/0], [persdb]).

:- use_module(library(aggregates)).
:- use_module(library(read)).
:- use_module(library(write)).

% :- include(library(persdb)).

%% Declare the directory associated to the key "db" where the
%% persistence sets of the persistent predicates are stored:
persistent_dir(db,'./pers_queue').

%% Declare a persistent predicate:
:- persistent(bar/1, db).

%% Read a term, storing it in a new fact of the persistent predicate
%% and list all the current facts of that predicate
main:-
     read(X),
     assertz_fact(bar(X)),
     findall(Y,bar(Y),L),
     display('This is the current list so far: '),
     write(L),
     nl.
