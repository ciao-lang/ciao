:- module(viewfastrw, [main/1], [assertions]).
:- doc(title,"Printing the contents of a file written with fast writing facilities").

:- use_module(library(fastrw), [fast_read/2]).

main(['-h']) :-
	usage.
main([File]) :-
	view_fastrw(File).
main(Args) :-
	inform_user(['error: invalid arguments ',Args]),
	nl(user_error),
	usage.

view_fastrw(File):-
        open(File, read, Stream),
	repeat,
	(
	    fast_read(Stream, Goal) ->
	    display(Goal), nl, fail
	;
	    true
	), !,
	close(Stream).

usage :-
	usage_text(TextS),
	atom_codes(Text,TextS),
	inform_user(['Usage: ']),
	inform_user([Text]).


usage_text("
	viewfastrw <file1>
	   : print the contents in symbolic form

	viewfastrw -h
	   : print this information
").


:- doc(author,"Remy Haemmerle").

:- doc(module,"This simple program takes as an argument a file written
   with @pred{fast_write} predicates from @pred{fastrw} libry and
   prints out in standard term representation.

   @section{Usage (viewfastrw)}

   @begin{verbatim}
   @includefact{usage_text/1}
   @end{verbatim}

   ").

