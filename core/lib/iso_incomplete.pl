:- module(iso_incomplete, [close/2, stream_property/2], [assertions,isomodes]).

:- doc(title, "Incomplete ISO Prolog predicates").

:- doc(author, "The CLIP Group").

:- doc(module, "This module implements some ISO Prolog predicates,
   but that are not complete yet.").

% :- entry open(+sourcename, +io_mode, ?stream, +open_option_list).

% original:
/*
:- pred open(+sourcename, +io_mode, ?stream, +open_option_list).

open(F, M, S, _) :- open(F, M, S).
*/

%:- entry close(+stream,+close_options). 
:- pred close(@stream,@close_options). 

% Options not completely implemented.

close(S, _) :- close(S).

:- true prop close_options(L) + regtype.
close_options([]).
close_options([O|Os]):-
	close_option(O),
	close_options(Os).

:- true prop close_option(O) + regtype.
%% Incomplete list of options. 
close_option(force(true)).
close_option(force(false)).


:- pred stream_property(?stream, ?stream_prop).

stream_property(S, P) :- % It is not complete
        current_stream(File, Mode, S),
        ( P = file_name(File)
        ; P = mode(Mode)
        ; Mode = read ->
            P = input
        ; P = output
        ).

:- true prop stream_prop(P) + regtype # "@var{P} is a valid stream
property.".  

%It is not complete wrt iso standard.
stream_prop(input).
stream_prop(output).
stream_prop(file_name(File)):-
	atm(File).
stream_prop(mode(Mode)):-
	atm(Mode).

% at_end_of_stream :- not_yet_implemented.
% at_end_of_stream(_) :- not_yet_implemented.
%
% set_stream_position(_,_) :- not_yet_implemented.
% 
% char_conversion(_,_) :- not_yet_implemented.
% current_char_conversion(_,_) :- not_yet_implemented.



