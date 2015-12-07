:- module(io_aux, [
		message/2, message_lns/4, messages/1,
		error/1, warning/1, note/1, message/1, debug/1,
		inform_user/1, display_string/1, display_list/1, display_term/1,
		% regtypes
		message_info/1, message_type/1
	    ],
	    [assertions, nativeprops, nortchecks]).

:- doc(title, "Message printing primitives").

:- doc(author, "Daniel Cabeza").
:- doc(author, "Edison Mera (improvements)").

:- doc(usage, "These predicates are builtin in Ciao, so nothing special
   has to be done to use them.").

:- doc(module, "This module provides predicates for printing in a
   unified way informational messages, and also for printing some terms
   in a specific way.").


:- use_module(engine(internals), ['$quiet_flag'/2]).

:- import(write, [write/1, writeq/1, print/1, printq/1]).

:- pred message(Type, Message) : ( message_type(Type),
	    message_text(Message) ) # "Output to standard error @var{Message},
   which is of type @var{Type}. The @tt{quiet} @index{prolog flag}
   (see @ref{Changing system behaviour and various flags}) controls
   which messages are actually output, depending on its type. Also,
   for @tt{error}, @tt{warning} and @tt{note} messages, a prefix is
   output which denotes the severity of the message.".

:- prop message_text(Message) + regtype # "@var{Message} is an item or
   a list of items from this list:

@begin{description}

@item{@tt{$$(String)}} @tt{String} is a string, which is output with
   @pred{display_string/1}.

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

:- pred message_lns(Type, L0, L1, Message) : ( message_type(Type),
	    nnegint(L0), nnegint(L1), message_text(Message) ) # "Output to
	standard error @var{Message}, which is of type @var{Type}, and
	occurs between lines @var{L0} and @var{L1}.  This is the same
	as @pred{message/2}, but printing the lines where the message
	occurs in a unified way (this is useful because automatic
	tools such as the emacs mode know how to parse them).".

:- pred messages(Messages) : list(Messages, message_info) # "Print
	each element in @var{Messages} using @pred{message/2},
	@pred{message_lns/4}, @pred{message/1}, @pred{error/1},
	@pred{warning/1}, @pred{note/1} or @pred{debug/1} predicate.
	If the element should be printed using message_lns/4, it is
	printed in a compact way, avoiding to print the same file name
	several times.".

:- prop message_info/1 + regtype # "The type of the elements to be
	printed using the @pred{messages/1} predicate.  Defined as
	@includedef{message_info/1}.".

% Auxiliary IO predicates:

:- doc(error/1,   "Defined as @includedef{error/1}.").
:- doc(warning/1, "Defined as @includedef{warning/1}.").
:- doc(note/1,    "Defined as @includedef{note/1}.").
:- doc(message/1, "Defined as @includedef{message/1}.").
:- doc(debug/1,   "Defined as @includedef{debug/1}.").

error(Message) :- message(error, Message).
warning(Message) :- message(warning, Message).
note(Message) :- message(note, Message).
message(Message) :- message(message, Message).
debug(Message) :- message(debug, Message).

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

output_message([M|Ms]) :- !,
	output_item(M),
	output_message(Ms).
output_message([]) :- !.
output_message(M) :-
	output_item(M).

output_item(V) :- var(V), !, display(V).
output_item($$(M)) :- !, display_string(M).
output_item({M}) :- !, (current_module(write) -> print(M) ; display(M)).
output_item(''({M})) :- !, (current_module(write) -> printq(M) ; displayq(M)).
output_item(''(M)) :- !, (current_module(write) -> writeq(M) ; displayq(M)).
output_item(~~(M)) :- !, (current_module(write) -> write(M) ;  display(M)).
output_item([](M)) :- !, output_message(M).
output_item(M) :- display(M).

allowed_type(error,   error) :- !.
allowed_type(warning, error) :- !.
allowed_type(warning, warning) :- !.
allowed_type(off,     error) :- !.
allowed_type(off,     warning) :- !.
allowed_type(off,     note) :- !.
allowed_type(off,     message) :- !.
allowed_type(debug,   _).

add_head(message, Mess, Mess) :- !.
add_head(debug,   Mess, Mess) :- !.
add_head(Type,    Mess, NewMess) :-
	label(Type, Label),
	NewMess = [Label|Mess].

label(error,   'ERROR: ').
label(warning, 'WARNING: ').
label(note,    'Note: ').

:- export(add_lines/4).
add_lines(L0, L1, Message, ['(lns ', L0, '-', L1, ') '|Message]).

:- doc(inform_user(Message), "Similar to @pred{message/1}, but
   @var{Message} is output with @pred{display_list/1}.  This predicate
   is obsolete, and may disappear in future versions.").

inform_user(MessL) :-
	'$quiet_flag'(Q, Q),
	allowed_type(Q, message), !,
	current_output(S),
	set_output(user_error),
	display_list(MessL), nl,
	set_output(S).
inform_user(_).

:- doc(display_list(List), "Outputs @var{List}.  If @var{List} is a
   list, do @pred{display/1} on each of its elements, else do
   @pred{display/1} on @var{List}.").

display_list([M|Ms]) :- !,
	display(M),
	display_list(Ms).
display_list([]) :- !.
display_list(M) :-
	display(M).

:- doc(display_term(Term), "Output @var{Term} in a way that a
   @pred{read/1} will be able to read it back, even if operators
   change.").

display_term(T) :- displayq(T), display(' .\n').

:- doc(display_string(String), "Output @var{String} as the sequence
   of characters it represents.").


:- pred display_string(String) : string.

display_string([]).
display_string([C|Cs]) :- put_code(C), display_string(Cs).

:- doc(bug, "@pred{message/2} assumes that a module with name 'write'
   is library(write).").


:- prop message_type/1 + regtype # "Specifies the different types of
	messages.".

message_type(error).
message_type(warning).
message_type(note).
message_type(message).
message_type(debug).

:- pred message_output/2 :: message_type * atm.

message_output(error,   user_error).
message_output(warning, user_error).
message_output(note,    user_output).
message_output(message, user).
message_output(debug,   user_error).

message_text(T) :- term(T).

message_info(message_lns(Source, Ln0, Ln1, Type, Text)) :-
	atm(Source),
	nnegint(Ln0),
	nnegint(Ln1),
	message_type(Type),
	message_text(Text).
message_info(message(Type, Text)) :- atm(Type), message_text(Text).
message_info(error(Text)) :- message_text(Text).
message_info(warning(Text)) :- message_text(Text).
message_info(note(Text)) :- message_text(Text).
message_info(message(Text)) :- message_text(Text).
message_info(debug(Text)) :- message_text(Text).

show_close('', _) :- !.
show_close(_,  Output) :-
	message_(message, Output, '}\n').

show_source(SourceOutput,  SourceOutput) :- !.
show_source(Source-Output, Source0-Output0) :-
	show_close(Source0, Output0),
	message_(message, Output, ['{In ', Source]).

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
message_info_type(message(_),                    message).
message_info_type(error(_),                      error).
message_info_type(warning(_),                    warning).
message_info_type(note(_),                       note).
message_info_type(debug(_),                      debug).

show_message0(message_lns(Ln0, Ln1, Type, Text)) :-
	message_lns(Type, Ln0, Ln1, Text).
show_message0(message(Type, Text)) :- !, message(Type, Text).
show_message0(error(Text)) :- error(Text).
show_message0(warning(Text)) :- warning(Text).
show_message0(note(Text)) :- note(Text).
show_message0(message(Text)) :- message(Text).
show_message0(debug(Text)) :- debug(Text).

messages([]).
messages([Message|Messages]) :-
	show_message(Message, ''-'', SourceOutput),
	messages2(Messages, SourceOutput).

messages2([],                 Source-Output) :- show_close(Source, Output).
messages2([Message|Messages], SourceOutput0) :-
	show_message(Message, SourceOutput0, SourceOutput),
	messages2(Messages, SourceOutput).
