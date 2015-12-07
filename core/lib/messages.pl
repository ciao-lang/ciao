:- module(messages, [
		error_message/1,
		error_message/2,
		error_message/3,
		warning_message/1,
		warning_message/2,
		warning_message/3,
		note_message/1,
		note_message/2,
		note_message/3,
		simple_message/1,
		simple_message/2,
		optional_message/2,
		optional_message/3,
		debug_message/1,
		debug_message/2,
		debug_goal/2,
		debug_goal/3,
		show_message/2,
		show_message/3,
		show_message/4,
% types
		message_t/1
	    ],
	    [
		assertions, regtypes, isomodes
	    ]).

%% NOTE: if you change the output format of messages you 
%%       will probably also want to change ciao.el

:- use_module(library(format)).

% Other libraries
:- use_module(library(lists)).
:- use_module(library(write)).
:- use_module(library(pathnames), [path_basename/2]).

:- set_prolog_flag(multi_arity_warnings, off).

%% ---------------------------------------------------------------------------

:- doc(title, "Printing status and error messages").

:- doc(author, "The CLIP Group").

:- doc(module, "This is a very simple library for printing status
     and error messages to the console.").

:- doc(bug, "Debug message switching should really be done with an
   expansion, for performance.").

:- reexport(library(compiler/c_itf_internal), [location_t/1]).
%% ---------------------------------------------------------------------------


:- true pred message_type_label(Type, Label) :: message_t * string.

message_type_label(error,   "ERROR").
message_type_label(warning, "WARNING").
message_type_label(note,    "NOTE").

:- regtype message_t/1 # "The types of messaes supported by the
   message predicate".

message_t(error).
message_t(warning).
message_t(note).
message_t(simple).
message_t(debug).

:- pred message_output_type(Type, Output) :: message_t *
	stream_alias # "Specifies where the message will be written".

message_output_type(simple,  user).
message_output_type(debug,   user_error).
message_output_type(error,   user_error).
message_output_type(warning, user_error).
message_output_type(note,    user_output).

:- pred show_message(Type, Text) : message_t * string # "The text
   provided in @var{Text} is printed as a message of type
   @var{Type}.".

:- meta_predicate show_message(?, addmodule).

show_message(Type, Message, Module) :-
	show_message__(Type, _, Message, [], Module).

:- pred show_message(Type, Text, ArgList) : message_t * format_control *
	list # "The text provided in @var{Text} is printed as a message of
   type @var{Type}, using the arguments in @var{ArgList} to interpret
   any variable-related formatting commands embedded in @var{Text}.".

:- meta_predicate show_message(?, ?, addmodule).

show_message(Type, Message, A, Module) :-
	show_message__(Type, _, Message, A, Module).

:- pred show_message(Type, Lc, Text, ArgList) : message_t * location_t *
	format_control * list # "The text provided in @var{Text} is printed
   as a message of type @var{Type}, using the arguments in
   @var{ArgList} to interpret any variable-related formatting commands
   embedded in @var{Text}, and reporting error location @var{Lc} (file
   and line numbers).".

:- meta_predicate show_message(?, ?, ?, addmodule).

show_message(Type, Loc, Message, A, Module) :-
	show_message__(Type, Loc, Message, A, Module).

show_message__(Type, Loc, Message, A, Module) :-
	message_output_type(Type, SO),
	show_message_(Type, SO, Loc, Message, A, Module).

show_message_(simple, SO, _, Message, A, _) :-
	!,
	simple_message(SO, Message, A).
show_message_(debug, SO, _, Message, A, Module) :-
	!,
	debug_message(SO, Message, A, Module).
show_message_(Type, SO, Loc, Message, A, Module) :-
	message_type_label(Type, Label),
	(
	    nonvar(Loc),
	    Loc= loc(File, LB, LE) ->
	    compose(Label, SO, Module, File, LB, LE, Message, A)
	;
	    compose(Label, SO, Module, Message, A)
	).

%% ---------------------------------------------------------------------------

:- pred error_message(Text) : string # "Same as
   @tt{message(error,Text)}.".

:- meta_predicate error_message(addmodule).

error_message(Message, Module) :-
	show_message__(error, _, Message, [], Module).

:- pred error_message(Text, ArgList) : format_control * list # "Same as
   @tt{message(error,Text,ArgList)}.".

:- meta_predicate error_message(?, addmodule).

error_message(Message, A, Module) :-
	show_message__(error, _, Message, A, Module).

:- pred error_message(Lc, Text, ArgList) : location_t * format_control *
	list # "Same as @tt{message(error,Lc,Text,ArgList)}.".

:- meta_predicate error_message(?, ?, addmodule).

error_message(Loc, Message, A, Module) :-
	show_message__(error, Loc, Message, A, Module).

%% ---------------------------------------------------------------------------

:- pred warning_message(Text) : string # "Same as
   @tt{message(warning,Text)}.".

:- meta_predicate warning_message(addmodule).

warning_message(Message, Module) :-
	show_message__(warning, _, Message, [], Module).

:- pred warning_message(Text, ArgList) : format_control * list # "Same as
   @tt{message(warning,Text,ArgList)}.".

:- meta_predicate warning_message(?, addmodule).

warning_message(Message, A, Module) :-
	show_message__(warning, _, Message, A, Module).

:- pred warning_message(Lc, Text, ArgList) : location_t * format_control *
	list # "Same as @tt{message(warning,Lc,Text,ArgList)}.".

:- meta_predicate warning_message(?, ?, addmodule).

warning_message(Loc, Message, A, Module) :-
	show_message__(warning, Loc, Message, A, Module).

%% ---------------------------------------------------------------------------

:- pred note_message(Text) : string # "Same as
   @tt{message(note,Text)}.".

:- meta_predicate note_message(addmodule).

note_message(Message, Module) :-
	show_message__(note, _, Message, [], Module).

:- pred note_message(Text, ArgList) : format_control * list # "Same as
   @tt{message(note,Text,ArgList)}.".

:- meta_predicate note_message(?, addmodule).

note_message(Message, A, Module) :-
	show_message__(note, _, Message, A, Module).

:- pred note_message(Lc, Text, ArgList) : location_t * format_control *
	list # "Same as @tt{message(note,Lc,Text,ArgList)}.".

:- meta_predicate note_message(?, ?, addmodule).

note_message(Loc, Message, A, Module) :-
	show_message__(note, Loc, Message, A, Module).

%% ---------------------------------------------------------------------------

:- pred simple_message(Text) : string
# "The text provided in @var{Text} is printed.".

simple_message(Message) :-
	message_output_type(simple, SO),
	simple_message(SO, Message, []).

:- pred simple_message(Text, ArgList) : format_control * list
# "The text provided in @var{Text} is printed as a message,
     using the arguments in @var{ArgList}.".

simple_message(Message, A) :-
	message_output_type(simple, SO),
	simple_message(SO, Message, A).

simple_message(SO, Message, A) :-
	format(SO, "{",     []),
	format(SO, Message, A),
	format(SO, "}\n",   []).

%% ---------------------------------------------------------------------------

:- pred optional_message(Text, Opts) : string * list(atm)
# "The text provided in @var{Text} is printed as a message, but
     only if the atom @tt{-v} is a member of @var{Opts}. These
     predicates are meant to be used for optional messages, which are
     only to be printed when @em{verbose} output is requested
     explicitly.".

optional_message(Message, Opts) :-
	optional_message(Message, [], Opts).

:- pred optional_message(Text, ArgList, Opts)
	: format_control * list * list(atm)
# "The text provided in @var{Text} is printed as a message, using
     the arguments in @var{ArgList}, but only if the atom @tt{-v} is a
     member of @var{Opts}. These predicates are meant to be used for
     optional messages, which are only to be printed when @em{verbose}
     output is requested explicitly.".

optional_message(Message, A, Opts) :-
	member('-v', Opts),
	!,
	simple_message(Message, A).
optional_message(_Message, _A, _Opts).

%% ---------------------------------------------------------------------------

:- pred debug_message(Text) : format_control

# "The text provided in @var{Text} is printed as a debugging
      message.  These messages are turned @tt{on} by defining a fact
      of @pred{issue_debug_messages/1} with the module name as
      argument.".

:- meta_predicate debug_message(addmodule).

debug_message(Message, Module) :-
	debug_message(user_error, Message, [], Module).

:- pred debug_message(Text, ArgList) : format_control * list

# "The text provided in @var{Text} is printed as a debugging
      message, using the arguments in @var{ArgList} to interpret any
      variable-related formatting commands embedded in
      @var{Text}. These messages are turned @tt{on} by defining a fact
      of @pred{issue_debug_messages/1} which the module name as
      argument.".

:- meta_predicate debug_message(?, addmodule).

debug_message(Message, A, Module) :-
	debug_message(user_error, Message, A, Module).

:- meta_predicate debug_message(?, ?, addmodule).

debug_message(SO, Message, A, Module) :-
	( issue_debug_messages(Module)
	-> compose("DEBUG", SO, Module, Message, A)
	; true ).

:- trust pred issue_debug_messages(Module) => atm

# "Printing of debugging messages is enabled for module @var{Module}.".

:- multifile issue_debug_messages/1.
:- data issue_debug_messages/1.

%% ---------------------------------------------------------------------------

:- pred debug_goal(Goal, Text)

# "@var{Goal} is called.  The text provided in @var{Text} is then
      printed as a debugging message.  The whole process (including
      running @var{Goal}) is turned @tt{on} by defining a fact of
      @pred{issue_debug_messages/1} with the module name as
      argument.".

:- meta_predicate debug_goal(goal, addmodule).

debug_goal(Goal, Message, Module) :-
	debug_goal(Goal, Message, [], Module).

:- pred debug_goal(Goal, Text, ArgList)

# "@var{Goal} is called.  The text provided in @var{Text} is then
      printed as a debugging message, using the arguments in
      @var{ArgList} to interpret any variable-related formatting
      commands embedded in @var{Text}. Note that the variables in
      @var{ArgList} can be computed by @var{Goal}.  The whole process
      (including running @var{Goal}) is turned @tt{on} by defining a
      fact of @pred{issue_debug_messages/1} with the module name as
      argument.".

:- meta_predicate debug_goal(goal, ?, addmodule).

debug_goal(Goal, Message, A, Module) :-
	( issue_debug_messages(Module)
	-> call(Goal),
	    compose("DEBUG", user_error, Module, Message, A)
	; true ).

%% ---------------------------------------------------------------------------

:- pred compose(Type, SO, Module, Mess, Args)
	: string * stream * atm * format_control * list

# "Print a generic message of type @var{Type}, to the stream
      @var{SO} flagged in module @var{Module}, with error message
      @var{Mess} containing arguments @var{Args}.".

compose(Type, SO, Module, Mess, Args) :-
	append("{~s (~q): ", Mess,  T1),
	append(T1,           "}~n", CMess),
	simplify_module(Module, SimplifiedModule),
	AllArgs = [Type, SimplifiedModule|Args],
	compose_common(SO, CMess, AllArgs).

compose_common(SO, CMess, AllArgs) :-
	(
	    prettyvars(AllArgs),
%           OLD METHOD
	    prolog_flag(write_strings, Old, on),
	    format(SO, CMess, AllArgs),
	    set_prolog_flag(write_strings, Old),
%           NEW METHOD
%	    sformat(S, CMess, AllArgs),
%	    display_long_string(S, SO),
	    fail
	;
	    true
	).

simplify_module(user(Path), SimplifiedModule) :-
	path_basename(Path, SimplifiedModule),
	!.
simplify_module(Path, SimplifiedModule) :-
	path_basename(Path, SimplifiedModule),
	!.
simplify_module(Module, Module).


:- pred compose(Type, SO, Module, File, LB, LE, Mess, Args)
	: string * stream * atm * atm * int * int * format_control * list

# "Print a generic message of type @var{Type}, to te stream
      @var{SO}, flagged in module @var{Module}, while processing file
      @var{File}, between line numbers @var{LB} and @var{LE}, with
      error message @var{Mess} containing arguments @var{Args}.".

compose(Type, SO, Module, File, LB, LE, Mess, Args) :-
	append("{In ~w~n~s (~q): (lns ~w-~w) ", Mess,    T1),
	append(T1,                              "~n}~n", CMess),
	simplify_module(Module, SimplifiedModule),
	AllArgs = [File, Type, SimplifiedModule, LB, LE|Args],
	compose_common(SO, CMess, AllArgs).

/*
%% ---------------------------------------------------------------------------
:- pred space(N, S) : (int(N), stream(S))
# "prints @var{N} spaces.".


%space( 0, S ) :- !.
space(N, S) :-
	N > 0,
	!,
	N1 is N - 1,
	display(S, ' '),
	space(N1, S).
space(_, _).

display_long_string(X, S) :-
	line_position(S, NInit),
	display_long_string_n(X, S, NInit, 79).

display_long_string_n(X, S, I, Max) :-
	append(WORD_WO_S, [SEP|XE], X),
	member(SEP, ",[]()"),
	!,
	append(WORD_WO_S, [SEP], WORD),
	nl_if_necessary(WORD, XE, S, I, Max, Current),
	display__compute_next_space(WORD, Current, I, FI),
	write_string(S, WORD),
	display_long_string_n(XE, S, FI, Max).

display_long_string_n(WORD, S, I, Max) :-
	nl_if_necessary(WORD, "", S, I, Max, Current),
	display__compute_next_space(WORD, Current, I, _),
	write_string(S, WORD).




% nil
display__compute_next_space([], _, I, I) :-
	!.

% push
display__compute_next_space([SEP|R], Current, I, FI) :-
	member(SEP, "[({"),
	!,
	NCurrent is Current + 1,
	NSpace is Current + 1,
	display__compute_next_space(R, NCurrent, [NSpace|I], FI).

% pop
display__compute_next_space([SEP|R], Current, [_|NI], FI) :-
	member(SEP, "])}"),
	!,
	NCurrent is Current + 1,
	display__compute_next_space(R, NCurrent, NI, FI).

% normal char	
display__compute_next_space([_|R], Current, I, FI) :-
	NCurrent is Current + 1,
	display__compute_next_space(R, NCurrent, I, FI),
	!.

% if it fails (?)
display__compute_next_space(_, _, I, I).


nl_if_necessary(WORD, REST, S, I, Max, C) :-
	line_position(S, Current),
	word_length(WORD, REST, Current, Max, WL),
	(
	    WL + Current + 1 > Max
	->
	    nl(S),
	    I = [C|_],
	    space(C, S)
	;
	    C = Current
	),
	!.

nl_if_necessary(_, _, S, _, _, C) :-
	line_position(S, C).




% The lenght of a "word" is:
% * if it a list -> sumatory of the length of its members
% * lenght( word )
%
% Optimization: We can stop counting if
%  Current + CurrentComputedLen > Max
word_length([0'[|WORD], REST, Current, Max, WL2) :-
	!,
	word__count(WORD, REST, 1, 0, Current, Max, WL),
	WL2 is WL - Current + 1.

word_length([0'(|WORD], REST, Current, Max, WL2) :-
	!,
	word__count(WORD, REST, 0, 1, Current, Max, WL),
	WL2 is WL - Current + 1.

word_length(WORD, REST, Current, Max, WL2) :-
	word__count(WORD, REST, 0, 0, Current, Max, WL),
	WL2 is WL - Current.
%word_length( WORD, _, _, _, WL ) :-
%	length( WORD, WL ).




:- pred word__count(_, _, _, _, Current, Max, Current) ::
	list * list * num * num * num * num * num.

% Start counting till balanced separator is reached
% S = number of [ ] (square parentheris) not yet balanced
% P = number of ( ) (parentheris) not yet balanced
word__count([], _, 0, 0, WL, _, WL) :-
	!.

word__count(_, [], 0, 0, WL, _, WL) :-
	!.


word__count(_, _, _, _, Current, Max, WL) :-
	Current > Max,
	WL = Current,
	!.

word__count([0'[|WORD], REST, S, P, Current, Max, WL) :-
	!,
	S1 is S + 1,
	C1 is Current + 1,
	word__count(WORD, REST, S1, P, C1, Max, WL).

word__count([0'(|WORD], REST, S, P, Current, Max, WL) :-
	!,
	P1 is P + 1,
	C1 is Current + 1,
	word__count(WORD, REST, S, P1, C1, Max, WL).

word__count([0']|WORD], REST, S, P, Current, Max, WL) :-
	!,
	S1 is S - 1,
	C1 is Current + 1,
	(
	    S1 >= 0
	->
	    word__count(WORD, REST, S1, P, C1, Max, WL)
	;
	    C1 = WL
	).

word__count([0')|WORD], REST, S, P, Current, Max, WL) :-
	!,
	P1 is P - 1,
	C1 is Current + 1,
	(
	    P1 >= 0
	->
	    word__count(WORD, REST, S, P1, C1, Max, WL)
	;
	    C1 = WL
	).

word__count([_|WORD], REST, S, P, Current, Max, WL) :-
	!,
	C1 is Current + 1,
	word__count(WORD, REST, S, P, C1, Max, WL).

word__count([], [], _, _, WL, _, WL) :-
	!.

word__count([], REST, S, P, Current, Max, WL) :-
	!,
	word__count(REST, [], S, P, Current, Max, WL).

word__count([], _, 0, 0, WL, _, WL) :-
	!.
*/
