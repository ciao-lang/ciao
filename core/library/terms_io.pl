:- module(terms_io, [
	  file_to_terms/2,
	  terms_to_file/2],
        [assertions,isomodes]).

:- doc(title, "Reading/writting list of terms").
:- doc(author,"The Ciao Development Team").

:- doc(module,"This module implements predicates to read/write list of
   terms to files.").

:- use_module(engine(stream_basic), [sourcename/1]).
:- use_module(engine(io_aux), [display_term/1]).
:- use_module(library(read), [read/1]).
:- use_module(library(streams)).

:- pred file_to_terms(File, Terms) :
	(sourcename(File), var(Terms)) => list(Terms)
   # "Unifies @var{Terms} with the list of all terms in @var{File}.".

file_to_terms(File, Terms) :-
        open_input(File, IO),
        read(T),
        read_terms(T, Terms),
        close_input(IO).

read_terms(end_of_file, []) :- !.
read_terms(T, [T|Ts]) :-
        read(T1),
        read_terms(T1, Ts).

:- pred terms_to_file(File, Terms) : sourcename * list
   # "Writes the terms in list @var{Terms} (including the ending '.')
      onto file @var{File}.".

terms_to_file(Terms, File) :-
	check_is_list(Terms, term_to_file/2-1), !,
        open_output(File, IO),
        display_term_list(Terms),
        close_output(IO).

display_term_list([]).
display_term_list([T|Ts]) :-
        display_term(T),
        display_term_list(Ts).

check_is_list(X, Loc):-
	var(X),
	throw(error(instantiation_error(X), Loc)).
check_is_list([], _Loc).
check_is_list([_|T], Loc):-
	check_is_list(T, Loc).
check_is_list(X, Loc):-
	throw(error(type_error(list, X), Loc)).

