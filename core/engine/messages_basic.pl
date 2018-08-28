:- module(messages_basic, [
		message/2, message_lns/4, messages/1,
	        lformat/1, display_list/1,
		% regtypes
		message_info/1, message_type/1
	    ],
	    [assertions, nativeprops, nortchecks]).

% TODO: rename to messages_basic.pl, move IO preds to stream_utils.pl

:- doc(title, "Message printing primitives").

:- doc(author, "Daniel Cabeza").
:- doc(author, "Edison Mera (improvements)").
:- doc(author, "Jose F. Morales").

:- doc(usage, "@include{InPrelude.lpdoc}").

:- doc(module, "This module provides predicates for printing in a
   unified way informational messages. It is designed to be small and
   do not have (strict) dependencies with other larger printing and
   formatting libraries in the system.").

:- use_module(engine(internals), ['$quiet_flag'/2]).
:- use_module(engine(stream_basic)).
:- use_module(engine(io_basic)).
:- use_module(engine(system_info), [current_module/1]).
:- use_module(library(stream_utils), [write_string/1]).

:- import(write, [write/1, writeq/1, print/1, printq/1]).

% ---------------------------------------------------------------------------

:- doc(doinclude, lformat_text/1).
:- prop lformat_text(Message) + regtype # "@var{Message} is an item or
   a list of items from this list:

@begin{description}

@item{@tt{$$(String)}} @tt{String} is a string, which is output with
   @pred{write_string/1}.

@item{@tt{''(Term)}} @tt{Term} is output quoted.  If the module
   @lib{write} is loaded, the term is output with @pred{writeq/1}, else
   with @pred{displayq/1}.

@item{@tt{~~(Term)}} @tt{Term} is output unquoted.  If the module
   @lib{write} is loaded, the term is output with @pred{write/1}, else
   with @pred{display/1}.

@item{@tt{''(@{Term@})}} @tt{Term} is output quoted.  If the module
   @lib{write} is loaded, the term is output with @pred{printq/1}, else
   with @pred{displayq/1}.
@item{@tt{@{Term@}}} @tt{Term} is output unquoted.  If the module
   @lib{write} is loaded, the term is output with @pred{print/1}, else
   with @pred{display/1}.

@item{@tt{[](Term)}} @tt{Term} is recursively output as a message, can
   be an item or a list of items from this list.

@item{@tt{Term}} Any other term is output with @pred{display/1}.

@end{description} ".

lformat_text(T) :- term(T).

:- pred lformat(T) : ( lformat_text(T) )
 # "Output a formatted text @var{T}. See @prop{lformat_text/1} for a
    description of the text formatting.".

lformat(Ms) :-
	output_message(Ms).

output_message([M|Ms]) :- !,
	output_item(M),
	output_message(Ms).
output_message([]) :- !.
output_message(M) :-
	output_item(M).

output_item(V) :- var(V), !, display(V).
output_item($$(M)) :- !, write_string(M).
output_item({M}) :- !, (current_module(write) -> print(M) ; display(M)).
output_item(''({M})) :- !, (current_module(write) -> printq(M) ; displayq(M)).
output_item(''(M)) :- !, (current_module(write) -> writeq(M) ; displayq(M)).
output_item(~~(M)) :- !, (current_module(write) -> write(M) ;  display(M)).
output_item([](M)) :- !, output_message(M).
output_item(M) :- display(M).

:- doc(display_list(List), "Outputs @var{List}.  If @var{List} is a
   list, do @pred{display/1} on each of its elements, else do
   @pred{display/1} on @var{List}.").

display_list([M|Ms]) :- !,
	display(M),
	display_list(Ms).
display_list([]) :- !.
display_list(M) :-
	display(M).

:- doc(bug, "@pred{message/2} assumes that a module with name 'write'
   is library(write).").

% ---------------------------------------------------------------------------

:- pred message(Type, Message) : ( message_type(Type),
	    lformat_text(Message) ) # "Output to standard error @var{Message},
   which is of type @var{Type}. The @tt{quiet} @index{prolog flag}
   (see @ref{Changing system behaviour and various flags}) controls
   which messages are actually output, depending on its type. Also,
   for @tt{error}, @tt{warning} and @tt{note} messages, a prefix is
   output which denotes the severity of the message.".

:- pred message_lns(Type, L0, L1, Message) : ( message_type(Type),
	    nnegint(L0), nnegint(L1), lformat_text(Message) ) # "Output to
	standard error @var{Message}, which is of type @var{Type}, and
	occurs between lines @var{L0} and @var{L1}.  This is the same
	as @pred{message/2}, but printing the lines where the message
	occurs in a unified way (this is useful because automatic
	tools such as the emacs mode know how to parse them).".

:- pred messages(Messages) : list(Messages, message_info) # "Print
	each element in @var{Messages} using @pred{message/2} or
	@pred{message_lns/4} predicates.".

% TODO: Make the library stateful such that this applies to other
%   preds here: "If the element should be printed using
%   @pred{message_lns/4}, it is printed in a compact way, avoiding to
%   print the same file name several times."

:- prop message_info/1 + regtype # "The type of the elements to be
	printed using the @pred{messages/1} predicate.  Defined as
	@includedef{message_info/1}.".

% Auxiliary IO predicates:

message(Type, Message) :-
	message_output(Type, Output),
	message_(Type, Output, Message).

message_(Type, Output, Message) :-
	'$quiet_flag'(Q, Q),
	allowed_type(Q, Type), !,
	add_head(Type, Message, MessL),
	current_output(S),
	set_output(Output),
	output_message(MessL), nl,
	set_output(S).
message_(_, _, _).

message_lns(Type, L0, L1, Message) :-
	'$quiet_flag'(Q, Q),
	allowed_type(Q, Type), !,
	add_lines(L0, L1, Message, Messlns),
	add_head(Type, Messlns, MessL),
	current_output(S),
	message_output(Type, Output),
	set_output(Output),
	output_message(MessL), nl,
	set_output(S).
message_lns(_, _, _, _).

allowed_type(error,   error) :- !.
allowed_type(warning, error) :- !.
allowed_type(warning, warning) :- !.
allowed_type(off,     error) :- !.
allowed_type(off,     warning) :- !.
allowed_type(off,     note) :- !.
allowed_type(off,     message) :- !.
allowed_type(off,     inform) :- !. % TODO: 'inform' is temporary
allowed_type(debug,   _).

add_head(user, Mess, Mess) :- !. % TODO: needed?
add_head(inform, Mess, Mess) :- !. % TODO: 'inform' is temporary
add_head(debug,   Mess, Mess) :- !.
add_head(Type,    Mess, NewMess) :-
	label(Type, Label),
	NewMess = [Label|Mess].

label(error,   'ERROR: ').
label(warning, 'WARNING: ').
label(note,    'Note: ').

:- export(add_lines/4).
add_lines(L0, L1, Message, ['(lns ', L0, '-', L1, ') '|Message]).

:- prop message_type/1 + regtype # "Specifies the different types of
	messages.".

message_type(error).
message_type(warning).
message_type(note).
message_type(user). % TODO: needed?
message_type(inform). % TODO: 'inform' is temporary
message_type(debug).

:- pred message_output/2 :: message_type * atm.

message_output(error,   user_error).
message_output(warning, user_error).
message_output(note,    user_output). % TODO: really?
message_output(user, user). % TODO: needed?
message_output(inform,  user_error). % TODO: 'inform' is temporary
message_output(debug,   user_error).

message_info(message_lns(Source, Ln0, Ln1, Type, Text)) :-
	atm(Source),
	nnegint(Ln0),
	nnegint(Ln1),
	message_type(Type),
	lformat_text(Text).
message_info(message(Type, Text)) :- atm(Type), lformat_text(Text).
message_info(error(Text)) :- lformat_text(Text).
message_info(warning(Text)) :- lformat_text(Text).
message_info(note(Text)) :- lformat_text(Text).
message_info(message(Text)) :- lformat_text(Text).
message_info(debug(Text)) :- lformat_text(Text).

show_close('', _) :- !.
show_close(_,  Output) :-
	message_(user, Output, '}\n'). % TODO: possibly at wrong stream!

show_source(SourceOutput,  SourceOutput) :- !.
show_source(Source-Output, Source0-Output0) :-
	show_close(Source0, Output0),
	message_(user, Output, ['{In ', Source]). % TODO: possibly at wrong stream!

show_message(MessageInfo, SourceOutput, SourceOutput) :-
	show_message0(MessageInfo),
	!.
show_message(MessageInfo0, SourceOutput0, Source-Output) :-
	MessageInfo0 =.. [FuncName, Source|Args],
	MessageInfo =.. [FuncName|Args],
	message_info_type(MessageInfo0, Type),
	message_output(Type, Output),
	show_source(Source-Output, SourceOutput0),
	show_message0(MessageInfo).

:- pred message_info_type/2 : nonvar * term + no_choicepoints.
:- pred message_info_type/2 : var * term.

message_info_type(message_lns(_, _, _, Type, _), Type).
message_info_type(message(Type, _),              Type).
message_info_type(message(_),                    user). % TODO: needed?
message_info_type(error(_),                      error).
message_info_type(warning(_),                    warning).
message_info_type(note(_),                       note).
message_info_type(debug(_),                      debug).

show_message0(message_lns(Ln0, Ln1, Type, Text)) :-
	message_lns(Type, Ln0, Ln1, Text).
show_message0(message(Type, Text)) :- !, message(Type, Text).
show_message0(error(Text)) :- message(error, Text).
show_message0(warning(Text)) :- message(warning, Text).
show_message0(note(Text)) :- message(note, Text).
show_message0(message(Text)) :- message(user, Text). % TODO: needed?
show_message0(debug(Text)) :- message(debug, Text).

messages([]).
messages([Message|Messages]) :-
	show_message(Message, ''-'', SourceOutput),
	messages2(Messages, SourceOutput).

messages2([],                 Source-Output) :- show_close(Source, Output).
messages2([Message|Messages], SourceOutput0) :-
	show_message(Message, SourceOutput0, SourceOutput),
	messages2(Messages, SourceOutput).
