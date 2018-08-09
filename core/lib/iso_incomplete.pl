:- module(iso_incomplete,
	[close/2,
	 close_options/1,
	 close_option/1,
	 stream_property/2,
	 stream_prop/1],
	 [assertions,isomodes]).

:- doc(title, "Incomplete ISO Prolog predicates").

:- doc(bug, "Merge with iso.pl and simply mark these predicates in the documentation as incomplete?"). 

:- doc(author, "The Ciao Development Team").

:- doc(module, "This module provides some additional ISO Prolog
   predicates whose implementation is not yet complete.").

% :- entry open(+sourcename, +io_mode, ?stream, +open_option_list).

% original:
/*
:- pred open(+sourcename, +io_mode, ?stream, +open_option_list).

open(F, M, S, _) :- open(F, M, S).
*/

%:- entry close(+stream,+close_options). 
:- pred close(@stream,@close_options). 

:- doc(bug,"Options not completely implemented.").

close(S, _) :- close(S).

:- true prop close_options(L) + regtype
# "@var{L} is a list of @prop{close_option/1}.".

close_options([]).
close_options([O|Os]):-
	close_option(O),
	close_options(Os).

:- true prop close_option(O) + regtype 
# "@var{O} is an option for close/2: @includedef{close_option/1}.".

:- doc(bug, "Incomplete list of options."). 
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

:- true prop stream_prop(P) + regtype
# "@var{P} is a valid stream property: @includedef{stream_prop/1}".  

:- doc(bug, "stream_prop/1 is not complete wrt iso standard.").

stream_prop(input).
stream_prop(output).
stream_prop(file_name(File)):-
	atm(File).
stream_prop(mode(Mode)):-
	atm(Mode).

:- doc(bug, "at_end_of_stream :- not_yet_implemented.").
:- doc(bug, "at_end_of_stream(_) :- not_yet_implemented.").

:- doc(bug, "set_stream_position(_,_) :- not_yet_implemented.").
 
:- doc(bug, "char_conversion(_,_) :- not_yet_implemented.").
:- doc(bug, "current_char_conversion(_,_) :- not_yet_implemented.").



