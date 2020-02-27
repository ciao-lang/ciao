:- module(messages, [
    show_message/2,
    show_message/3,
    show_message/4,
    % (internal)
    show_message__/5,
    % types
    message_t/1,
    %
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
    debug_message/2
], [assertions, regtypes, isomodes, datafacts]).

:- doc(title, "Printing status and error messages").
:- doc(author, "The Ciao Development Team").

:- doc(module, "This is a very simple library for printing status
     and error messages to the console.").

%% NOTE: if you change the output format of messages you 
%%       will probably also want to change ciao.el

:- doc(bug, "Debug message switching should really be done with an
   expansion, for performance (see other debug/trace packages).").

%% ---------------------------------------------------------------------------

:- use_module(engine(runtime_control), [prolog_flag/3, set_prolog_flag/2]).
:- use_module(library(lists)).
:- use_module(library(streams)).
:- use_module(library(write), [prettyvars/1]).
:- use_module(library(format), [format/3, format_control/1]).
:- use_module(library(pathnames), [path_basename/2]).

%% ---------------------------------------------------------------------------

:- if(defined(optim_comp)).
:- doc(doinclude,location_t/1).
:- regtype location_t/1 # "Identifies a program source line.".
location_t(loc(File,L1,L2)):- atm(File), int(L1), int(L2).
:- else.
:- reexport(library(compiler/c_itf), [location_t/1]).
:- endif.

%% ---------------------------------------------------------------------------

:- pred message_type_label(Type, Label) :: message_t * atm.

message_type_label(error,   'ERROR').
message_type_label(warning, 'WARNING').
message_type_label(note,    'NOTE').

:- doc(message_t/1, "This type defines the types of messages supported
   by the message priting predicates. @includedef{message_t/1}").

:- regtype message_t/1 # "The types of messages supported by the
   message predicate.".

message_t(error).
message_t(warning).
message_t(note).
message_t(simple).
message_t(debug).

:- doc(message_output_type/2, "This predicte specifies to what stream
   (@texttt{user}, @texttt{user_error}, etc.) the message will be
   written. @includedef{message_output_type/2}").

:- pred message_output_type(Type, Output) :: message_t * stream_alias
   # "Specifies where the message will be written.".

message_output_type(simple,  user).
message_output_type(debug,   user_error).
message_output_type(error,   user_error).
message_output_type(warning, user_error).
message_output_type(note,    user_output).

:- pred show_message(Type, Text) : message_t * string
   # "The text provided in @var{Text} is printed as a message of type
   @var{Type}.".

:- if(defined(optim_comp)).
:- '$context'(show_message/2, module).
show_message(Type, Message) :- '$module'(Module),
    show_message__(Type, _, Message, [], Module).
:- else.
:- meta_predicate show_message(?, addmodule).
show_message(Type, Message, Module) :-
    show_message__(Type, _, Message, [], Module).
:- endif.

:- pred show_message(Type, Text, ArgList) : message_t * format_control * list
   # "The text provided in @var{Text} is printed as a message of type
   @var{Type}, using the arguments in @var{ArgList} to interpret any
   variable-related formatting commands embedded in @var{Text}.".

:- if(defined(optim_comp)).
:- '$context'(show_message/3, module).
show_message(Type, Message, A) :- '$module'(Module),
    show_message__(Type, _, Message, A, Module).
:- else.
:- meta_predicate show_message(?, ?, addmodule).
show_message(Type, Message, A, Module) :-
    show_message__(Type, _, Message, A, Module).
:- endif.

:- pred show_message(Type, Lc, Text, ArgList)
   : message_t * location_t * format_control * list
   # "The text provided in @var{Text} is printed as a message of type
   @var{Type}, using the arguments in @var{ArgList} to interpret any
   variable-related formatting commands embedded in @var{Text}, and
   reporting error location @var{Lc} (file and line numbers).".

:- if(defined(optim_comp)).
:- '$context'(show_message/4, module).
show_message(Type, Loc, Message, A) :- '$module'(Module),
    show_message__(Type, Loc, Message, A, Module).
:- else.
:- meta_predicate show_message(?, ?, ?, addmodule).
show_message(Type, Loc, Message, A, Module) :-
    show_message__(Type, Loc, Message, A, Module).
:- endif.

:- doc(hide,show_message__/5).

show_message__(Type, Loc, Message, A, Module) :-
    message_output_type(Type, SO),
    show_message_(Type, SO, Loc, Message, A, Module).

show_message_(optional, SO, _, Message, A, _) :- !,
    optional_message(SO, Message, A).
show_message_(simple, SO, _, Message, A, _) :- !,
    simple_message_(SO, Message, A).
show_message_(debug, SO, _, Message, A, Module) :- !,
    debug_message_(SO, Message, A, Module).
show_message_(Type, SO, Loc, Message, A, Module) :-
    message_type_label(Type, Label),
    compose(Label, SO, Module, Loc, Message, A).

%% ---------------------------------------------------------------------------

:- pred error_message(Text) : string
   # "Same as @tt{message(error,Text)}.".

:- if(defined(optim_comp)).
:- '$context'(error_message/1, module).
error_message(Message) :- '$module'(Module),
    show_message__(error, _, Message, [], Module).
:- else.
:- meta_predicate error_message(addmodule).
error_message(Message, Module) :-
    show_message__(error, _, Message, [], Module).
:- endif.

:- pred error_message(Text, ArgList) : format_control * list
   # "Same as @tt{message(error,Text,ArgList)}.".

:- if(defined(optim_comp)).
:- '$context'(error_message/2, module).
error_message(Message,A) :- '$module'(Module),
    show_message__(error, _, Message, A, Module).
:- else.
:- meta_predicate error_message(?, addmodule).
error_message(Message, A, Module) :-
    show_message__(error, _, Message, A, Module).
:- endif.

:- pred error_message(Lc, Text, ArgList) 
   : location_t * format_control * list
   # "Same as @tt{message(error,Lc,Text,ArgList)}.".

:- if(defined(optim_comp)).
:- '$context'(error_message/3, module).
error_message(Loc,Message,A) :- '$module'(Module),
    show_message__(error, Loc, Message, A, Module).
:- else.
:- meta_predicate error_message(?, ?, addmodule).
error_message(Loc, Message, A, Module) :-
    show_message__(error, Loc, Message, A, Module).
:- endif.

%% ---------------------------------------------------------------------------

:- pred warning_message(Text) : string
   # "Same as @tt{message(warning,Text)}.".

:- if(defined(optim_comp)).
:- '$context'(warning_message/1, module).
warning_message(Message) :- '$module'(Module),
    show_message__(warning, _, Message, [], Module).
:- else.
:- meta_predicate warning_message(addmodule).
warning_message(Message, Module) :-
    show_message__(warning, _, Message, [], Module).
:- endif.

:- pred warning_message(Text, ArgList) : format_control * list
   # "Same as @tt{message(warning,Text,ArgList)}.".

:- if(defined(optim_comp)).
:- '$context'(warning_message/2, module).
warning_message(Message,A) :- '$module'(Module),
    show_message__(warning, _, Message, A, Module).
:- else.
:- meta_predicate warning_message(?, addmodule).
warning_message(Message, A, Module) :-
    show_message__(warning, _, Message, A, Module).
:- endif.

:- pred warning_message(Lc, Text, ArgList)
   : location_t * format_control * list
   # "Same as @tt{message(warning,Lc,Text,ArgList)}.".

:- if(defined(optim_comp)).
:- '$context'(warning_message/3, module).
warning_message(Loc,Message,A) :- '$module'(Module),
    show_message__(warning, Loc, Message, A, Module).
:- else.
:- meta_predicate warning_message(?, ?, addmodule).
warning_message(Loc, Message, A, Module) :-
    show_message__(warning, Loc, Message, A, Module).
:- endif.

%% ---------------------------------------------------------------------------

:- pred note_message(Text) : string
   # "Same as @tt{message(note,Text)}.".

:- if(defined(optim_comp)).
:- '$context'(note_message/1, module).
note_message(Message) :- '$module'(Module),
    show_message__(note, _, Message, [], Module).
:- else.
:- meta_predicate note_message(addmodule).
note_message(Message, Module) :-
    show_message__(note, _, Message, [], Module).
:- endif.

:- pred note_message(Text, ArgList) : format_control * list
   # "Same as @tt{message(note,Text,ArgList)}.".

:- if(defined(optim_comp)).
:- '$context'(note_message/2, module).
note_message(Message,A) :- '$module'(Module),
    show_message__(note, _, Message, A, Module).
:- else.
:- meta_predicate note_message(?, addmodule).
note_message(Message, A, Module) :-
    show_message__(note, _, Message, A, Module).
:- endif.

:- pred note_message(Lc, Text, ArgList)
   : location_t * format_control * list
   # "Same as @tt{message(note,Lc,Text,ArgList)}.".

:- if(defined(optim_comp)).
:- '$context'(note_message/3, module).
note_message(Loc,Message,A) :- '$module'(Module),
    show_message__(note, Loc, Message, A, Module).
:- else.
:- meta_predicate note_message(?, ?, addmodule).
note_message(Loc, Message, A, Module) :-
    show_message__(note, Loc, Message, A, Module).
:- endif.

%% ---------------------------------------------------------------------------

:- pred simple_message(Text) : string
   # "The text provided in @var{Text} is printed.".

simple_message(Message) :-
    message_output_type(simple, SO),
    simple_message_(SO, Message, []).

:- pred simple_message(Text, ArgList) : format_control * list
   # "The text provided in @var{Text} is printed as a message, using
   the arguments in @var{ArgList}.".

simple_message(Message, A) :-
    message_output_type(simple, SO),
    simple_message_(SO, Message, A).

simple_message_(SO, Message, A) :-
    format(SO, "{", []),
    format(SO, Message, A),
    format(SO, "}\n", []).

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
   message.  These messages are turned @tt{on} by defining a fact of
   @pred{issue_debug_messages/1} with the module name as argument.".

:- if(defined(optim_comp)).
:- '$context'(debug_message/1, module).
debug_message(Message) :- '$module'(Module),
    debug_message_(user_error, Message, [], Module).
:- else.
:- meta_predicate debug_message(addmodule).
debug_message(Message, Module) :-
    debug_message_(user_error, Message, [], Module).
:- endif.

:- pred debug_message(Text, ArgList) : format_control * list
   # "The text provided in @var{Text} is printed as a debugging
   message, using the arguments in @var{ArgList} to interpret any
   variable-related formatting commands embedded in @var{Text}. These
   messages are turned @tt{on} by defining a fact of
   @pred{issue_debug_messages/1} which the module name as argument.".

:- if(defined(optim_comp)).
:- '$context'(debug_message/2, module).
debug_message(Message,A) :- '$module'(Module),
    debug_message_(user_error, Message, A, Module).
:- else.
:- meta_predicate debug_message(?, addmodule).
debug_message(Message, A, Module) :-
    debug_message_(user_error, Message, A, Module).
:- endif.

debug_message_(SO, Message, A, Module) :-
    ( issue_debug_messages(Module) ->
        compose('DEBUG', SO, Module, _, Message, A)
    ; true
    ).

:- trust pred issue_debug_messages(Module) => atm
   # "Printing of debugging messages is enabled for module @var{Module}.".

:- multifile issue_debug_messages/1.
:- data issue_debug_messages/1.

%% ---------------------------------------------------------------------------

:- pred compose(Type, SO, Module, Loc, Mess, Args)
   : atm * stream * atm * location_t * format_control * list
   # "Print a generic message of type @var{Type}, to the stream
   @var{SO}, flagged in module @var{Module} at location @var{Loc}
   (ignored if unbound), with error message @var{Mess} containing
   arguments @var{Args}.".

compose(Type, SO, Module, Loc, Mess, Args) :-
    simplify_module(Module, SimplifiedModule),
    ( nonvar(Loc),
      Loc = loc(File, LB, LE) ->
        append("{In ~w~n~w (~q): (lns ~w-~w) ", Mess, T1),
        append(T1, "~n}~n", CMess),
        AllArgs = [File, Type, SimplifiedModule, LB, LE|Args]
    ; append("{~w (~q): ", Mess, T1),
      append(T1, "}~n", CMess),
      AllArgs = [Type, SimplifiedModule|Args]
    ),
    prettyformat(SO, CMess, AllArgs).

prettyformat(SO, CMess, AllArgs) :-
    ( \+ \+ ( prettyvars(AllArgs),
              prolog_flag(write_strings, Old, on),
              format(SO, CMess, AllArgs),
              set_prolog_flag(write_strings, Old) ) -> true
    ; true % TODO: needed?
    ).

simplify_module(user(Path), SimplifiedModule) :-
    path_basename(Path, SimplifiedModule),
    !.
simplify_module(Path, SimplifiedModule) :-
    path_basename(Path, SimplifiedModule),
    !.
simplify_module(Module, Module).
