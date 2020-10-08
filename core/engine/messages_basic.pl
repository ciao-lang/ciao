:- module(messages_basic, [
            message/2, message_lns/4, messages/1,
            message_type_visible/1,
            lformat/1, display_list/1,
            % regtypes
            message_info/1, message_type/1
        ],
    [assertions, nativeprops, nortchecks]).

% TODO: move lformat/1, display_list/1 to format_basic.pl?

:- doc(title, "Message printing primitives").

:- doc(author, "Daniel Cabeza").
:- doc(author, "Edison Mera (improvements)").
:- doc(author, "Jose F. Morales").
:- doc(author, "Manuel V. Hermenegildo").

:- doc(module, "This module provides predicates for printing in a
   unified way informational messages. It is designed to be small and
   do not have (strict) dependencies with other larger printing and
   formatting libraries in the system.").

:- use_module(engine(internals), ['$quiet_flag'/2]).
:- use_module(engine(stream_basic)).
:- use_module(engine(io_basic)).
:- use_module(engine(runtime_control), [current_module/1]).
:- use_module(library(stream_utils), [write_string/1]).

:- import(write, [write/1, writeq/1, print/1, printq/1]).

% ---------------------------------------------------------------------------

:- doc(doinclude, lformat_text/1).
:- prop lformat_text(Message) + regtype # "@var{Message} is an item or
   a list of items from the following:

@begin{description}

@item{@tt{$$(String)}} @tt{String} is a string, which is output with
   @pred{write_string/1}.

@item{@tt{'}@tt{'}@tt{(Term)}} @tt{Term} is output quoted.  If the module
   @lib{write} is loaded, the term is output with @pred{writeq/1}, else
   with @pred{displayq/1}.

@item{@tt{~~(Term)}} @tt{Term} is output unquoted.  If the module
   @lib{write} is loaded, the term is output with @pred{write/1}, else
   with @pred{display/1}.

@item{@tt{'}@tt{'}@tt{(@{Term@})}} @tt{Term} is output quoted.  If the module
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
   for @tt{error}, @tt{warning}, and @tt{note} messages, a prefix is
   output which denotes the severity of the message.".

:- pred message_lns(Type, L0, L1, Message) : ( message_type(Type),
        nnegint(L0), nnegint(L1), lformat_text(Message) ) # "Output to
    standard error @var{Message}, which is of type @var{Type}, and
    occurs between lines @var{L0} and @var{L1}.  This is the same
    as @pred{message/2}, but printing the lines where the message
    occurs in a unified way (this is useful because automatic
    tools such as the emacs mode know how to parse them).".

:- pred messages(Messages) : list(message_info, Messages) # "Print
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

message(Type, Message) :- message_type_visible(Type), !,
    add_head(Type, Message, MessL),
    message_(Type, MessL).
message(_,_).

message_lns(Type, L0, L1, Message) :- message_type_visible(Type), !,
    add_lines(L0, L1, Message, Messlns),
    add_head(Type, Messlns, MessL),
    message_(Type, MessL).
message_lns(_,_,_,_).

message_(Type, MessL) :-
    message_output(Type, Output),
    current_output(S),
    set_output(Output),
    output_message(MessL), nl,
    set_output(S).


:- pred message_type_visible(Type) : message_type(Type) # "Succeeds if
  message type @var{Type} is visible according to the current value of
  the @tt{quiet} @index{prolog flag}".

message_type_visible(Type) :-
    '$quiet_flag'(Q, Q),
    allowed_type(Q, Type).

allowed_type(error,   error) :- !.
allowed_type(error,   error0) :- !.
allowed_type(warning, error) :- !.
allowed_type(warning, error0) :- !.
allowed_type(warning, warning) :- !.
allowed_type(off,     error) :- !.
allowed_type(off,     error0) :- !.
allowed_type(off,     warning) :- !.
allowed_type(off,     note) :- !.
allowed_type(off,     user) :- !.
allowed_type(off,     inform) :- !.
allowed_type(off,     passed) :- !.
allowed_type(off,     failed) :- !.
allowed_type(off,     aborted) :- !.
allowed_type(debug,   _).


add_head(user, Mess, Mess) :- !. % TODO: needed?
add_head(inform, Mess, Mess) :- !.
add_head(debug,   Mess, Mess) :- !.
add_head(error0, Mess, Mess) :- !.
add_head(Type,    Mess, NewMess) :-
    label(Type, Label),
    NewMess = [Label|Mess].

label(error,   'ERROR: ').
label(warning, 'WARNING: ').
label(note,    'Note: ').
label(passed,  'PASSED: ').
label(failed,  'FAILED: ').
label(aborted, 'ABORTED: ').


:- export(add_lines/4).
add_lines(L0, L1, Message, ['(lns ', L0, '-', L1, ') '|Message]).

:- prop message_type(M) + regtype # "@var{M} is one of the accepted message types.".
:- doc(message_type(M), "@includedef{message_type/1}").

message_type(error).
message_type(error0). % like 'error' without any prefix
message_type(warning).
message_type(note). % to user_output % TODO: really?
message_type(user). % to user % TODO: user_output? needed?
message_type(inform). % to user_error, without any prefix
message_type(debug).
message_type(passed).
message_type(failed).
message_type(aborted).
% TODO: unify with core/lib/messages. Missing here: simple. Missing
% there: error0, user, inform, passed, failed, aborted


:- pred message_output/2 :: message_type * atm.

message_output(error,   user_error).
message_output(error0,  user_error).
message_output(warning, user_error).
message_output(note,    user_error). % TODO: was user_output, simplify code now?
message_output(user,    user). % TODO: needed?
message_output(inform,  user_error).
message_output(debug,   user_error).
message_output(passed,  user_error).
message_output(failed,  user_error).
message_output(aborted, user_error).
% TODO: unify with core/lib/messages. Common types have already the
% same output


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
%% message_info(passed(Text)) :- lformat_text(Text).
%% message_info(failed(Text)) :- lformat_text(Text).
%% message_info(aborted(Text)) :- lformat_text(Text).

% -------------------------------------------------------------------------------
% messages/2
% -------------------------------------------------------------------------------

% messages/2 is used to allow wrapping a set of messages in a source
% context, with an initial "{In Source" message and a final "}"
% message.

% TODO: unify with compiler mechanism to handle that source
% context. Move elsewhere in the meantime.


show_close('', _) :- !.
show_close(_,  Type) :-
    message_(Type, '}\n').

show_source(Source-Type,  Source-Type0) :-
    message_output(Type0,Output),
    message_output(Type,Output),
    !.
show_source(Source-Type, Source0-Type0) :-
    show_close(Source0, Type0),
    message_(Type, ['{In ', Source]).

show_message(message_lns(Source,LB,LE,Type,Msg), SourceType0, Source-Type) :-
    message_type_visible(Type), !,
    show_source(Source-Type, SourceType0),
    show_message0(message_lns(LB,LE,Type,Msg)).
show_message(message_lns(_,_,_,_,_), SourceType, SourceType) :- !.
show_message(message(Msg), Source-Type, Source-Type) :- !,
    message_(Type, Msg).
show_message(MessageInfo, SourceOutput, SourceOutput) :-
    show_message0(MessageInfo).

show_message0(message_lns(Ln0, Ln1, Type, Text)) :-
    message_lns(Type, Ln0, Ln1, Text).
show_message0(message(Type, Text)) :- !, message(Type, Text).
show_message0(error(Text)) :- message(error, Text).
show_message0(warning(Text)) :- message(warning, Text).
show_message0(note(Text)) :- message(note, Text).
% show_message0(message(Text)) :- message(user, Text). % Hijacked for showing message without header in latest output
show_message0(debug(Text)) :- message(debug, Text).

messages([]).
messages([Message|Messages]) :-
    show_message(Message, ''-'', SourceType),
    messages2(Messages, SourceType).

messages2([],                 Source-Type) :- show_close(Source, Type).
messages2([Message|Messages], SourceType0) :-
    show_message(Message, SourceType0, SourceType),
    messages2(Messages, SourceType).
