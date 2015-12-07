:- module(dec10_io, [
        see/1, seeing/1, seen/0, tell/1, telling/1, told/0, close_file/1],
	[assertions,regtypes]).

:- use_module(engine(internals)).

:- doc(title,"DEC-10 Prolog file IO").

:- doc(module,"This module implements the support for DEC-10
   Prolog style file I/O.").

% Support DEC-10 style I/O.  We maintain a database
% 	current_file_stream(File, Mode, Stream)
% of Streams opened by see/1 and tell/1.  

:- data current_file_stream/3.

builtin_stream(user, read, user_input).
builtin_stream(user, write, user_output).
builtin_stream(user_input, read, user_input).
builtin_stream(user_output, write, user_output).
builtin_stream(user_error, write, user_error).

close_file(File) :-
	nonvar(File), !,
	(   current_fact(current_file_stream(File, _, S), Ref) ->
	    close(S),
	    erase(Ref)
	;   current_fact(current_file_stream(_, _, S), Ref) ->
	    close(S),
	    erase(Ref)
	;   close(File)
	).
close_file(_File) :-
        throw(error(instantiation_error,close_file/1-1)).

:- pred see(File) : atom(File).

see(File) :-
	nonvar(File),
	(   builtin_stream(File, read, S) -> true
	;   current_fact(current_file_stream(_, read, S)),
	    File=S -> true
	;   current_fact(current_file_stream(File, read, S)) -> true
	;   absolute_file_name(File, '', '', '.', AbsFileName, _, _),
	    '$open'(AbsFileName, r, S), 
	    assertz_fact(current_file_stream(File, read, S))
	),
	set_input(S).

:- pred seeing(File) => atom(File).

seeing(File) :-
	current_input(S), 
	(   builtin_stream(File, read, S) -> true
	;   current_fact(current_file_stream(File, read, S)) -> true
	;   File=S
	).

seen :- seeing(X), close_file(X).

:- pred tell(File) : atom(File).

tell(File) :-
	nonvar(File),
	(   builtin_stream(File, write, S) -> true
	;   current_fact(current_file_stream(_, write, S)),
	    File=S -> true
	;   current_fact(current_file_stream(File, write, S)) -> true
	;   absolute_file_name(File, '', '', '.', AbsFileName, _, _),
	    '$open'(AbsFileName, w, S), 
	    assertz_fact(current_file_stream(File, write, S))
	),
	set_output(S).

:- pred telling(File) => atom(File).

telling(File) :-
	current_output(S), 
	(   builtin_stream(File, write, S) -> true
	;   current_fact(current_file_stream(File, write, S)) -> true
        ;   File=S
        ).

told :- telling(X), close_file(X).
