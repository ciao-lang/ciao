:- use_package([assertions]).  
:- use_module(engine(internals)).

:- doc(title,"Printing the contents of a bytecode file").

main(['-h']) :-
	usage.
main([File]) :-
	viewql(File).
main(Args) :-
	inform_user(['error: invalid arguments ',Args]),
	nl(user_error),
	usage.

usage :-
	usage_text(TextS),
	atom_codes(Text,TextS),
	inform_user(['Usage: ']),
	inform_user([Text]).

viewql(File) :-
	find_pl_filename(File, _PlName, Base, _Dir),
	po_filename(Base, AbsName),
	'$push_qlinfo',
        '$open'(AbsName, r, Stream),            % Gives errors
	repeat,
	    '$qread'(Stream, Goal),
	    (   Goal= -1
	    ;   display(Goal), nl, fail
	    ), !,
	'$pop_qlinfo',
	close(Stream).

usage_text("
	viewpo <file1>.po
	   : print .po contents in symbolic form

	viewpo -h
	   : print this information
").

:- doc(author,"Daniel Cabeza").

:- doc(module,"This simple program takes as an argument a bytecode
   (.po) file and prints out in symbolic form the information
   contained in the file. It uses compiler and engine builtins to do
   so, so that it keeps track with changes in bytecode format.

   @section{Usage (viewpo)}

   @begin{verbatim}
   @includefact{usage_text/1}
   @end{verbatim}

   ").
