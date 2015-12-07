:- module(_,[main/1],[]).

:- use_module(library(streams)).
:- use_module(library(fastrw)).
:- use_module(library(write)).
:- use_module(library(read)).

main([File]) :-
        set_prolog_flag(write_strings,on),
        open_input(File,OldInput),
        read(Vstruct),
        arg(1,Vstruct,V),
        message(['Version ',V]),
        show_fast_read,
        close_input(OldInput).

show_fast_read :-
        fast_read(R), !,
        write(R), nl,
        show_fast_read.
show_fast_read.
