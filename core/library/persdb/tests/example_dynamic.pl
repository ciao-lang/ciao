:- module(example_dynamic, [main/0], [persdb]).

:- use_module(library(aggregates)).
:- use_module(library(read)).
:- use_module(library(write)).

:- data foo/1.

main:-
%%   Declare the directory associated to the key "db" 
     asserta_fact(persistent_dir(db,'./pers_queue')),
%%   Declare the predicate foo/1 as persistent at run-time  
     make_persistent(foo/1, db),
     read(X),
     assertz_fact(foo(X)),
     findall(Y, foo(Y), L),
     write(L).    
