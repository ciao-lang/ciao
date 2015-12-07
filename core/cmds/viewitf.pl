:- module(viewitf, [main/1,viewitf/1], [assertions]).
:- use_module(library(streams)).
:- use_module(library(read)).
:- use_module(library(fastrw)).

main([File]) :-
        viewitf(File).


viewitf(File) :-
        open_input(File, InState),
        read(v(V,Format)),
        message(['Version : ',V]),
        message(['Format : ',Format]),
        repeat,
          do_read(Format,ITF),
        ( ITF = end_of_file, !
        ; display(ITF), nl,
          fail
        ),
        close_input(InState).

do_read(f,Term) :- fast_read(Term), ! ; Term = end_of_file.
do_read(r,Term) :- read(Term).
