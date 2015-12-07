%% :- module(examples, [main/0], []).

:- use_module(library(concurrency)).
:- use_module(library(system)).

main:-
        eng_call(pause(1), create, create),
        pause(2).
