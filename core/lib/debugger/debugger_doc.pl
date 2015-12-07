:- use_package([assertions]).

:- doc(filetype, documentation).

:- doc(title,"The interactive debugger").

:- doc(subtitle_extra,"@em{A part of the Ciao interactive program development environment}").
:- doc(subtitle_extra,"@bf{The Ciao System Documentation Series}").
:- doc(subtitle_extra,"Technical Report CLIP X/YY.1").
:- doc(subtitle_extra,"@em{Draft printed on:} @today{}").

:- doc(author, "Daniel Cabeza").
:- doc(author, "Manuel C. Rodriguez").
:- doc(author, "Edison Mera").
:- doc(author, "A. Ciepielewski (first version)").
:- doc(author, "Mats Carlsson (first version)").
:- doc(author, "T. Chikayama (first version)").
:- doc(author, "K. Shen (first version)").

%% :- include(ciao_docsrc(common/'ClipAddress')).
%% :- include(ciao_docsrc(common/'Copyright')).

:- doc(module,"

The Ciao program development environment includes a number of advanced
@concept{debugging tools}, such as a @concept{source-level debugger}, the
@apl{ciaopp} preprocessor, and some @concept{execution visualizers}.
Herein we discuss the interactive debugger available in the standard
@concept{top-level}, which allows tracing the control flow of programs, in
a similar way to other popular Prolog systems. This is a classical Byrd
@em{box-type} @index{debugger} @cindex{box-type debugger}
@cite{Byrd80,BoByPeWa81}, with some enhancements, most notably being able to
track the execution on the source program. 

We also discuss the @concept{embedded debugger}, which is a version of
the debugger which can be embedded into executables. It allows
triggering an interactive debugging session at any time while running
an executable, without any need for the top-level shell.

Byrd's @concept{Procedure Box} model of debugging execution provides a
simple way of visualising control flow, including backtracking.  Control
flow is in principle viewed at the predicate level, rather than at the
level of individual clauses. The Ciao debugger has the ability to mark
selected modules and/or files for debugging (traditional and source
debugging), rather than having to exhaustively trace the program. It also
allows to selectively set @concept{spy-points} and @concept{breakpoints}.
Spy-points allow the programmer to nominate interesting predicates at which
program execution is to pause so that the programmer can interact with the
debugger.  Breakpoints are similar to spy-points, but allow pausing at a
specific line in the code, corresponding to a particular literal. There is
a wide choice of control and information options available during debugging
interaction.

@bf{Note}: While the debugger described herein can be used in a
standalone way (i.e., from an operating system shell or terminal
window) in the same way as other Prolog debuggers, the most convenient
way of debugging Ciao programs is by using the programming environment
(see @ref{Using Ciao inside GNU emacs}). This environment has many
debugging-related facilities, including displaying the source code for
the module(s) corresponding to the procedure being executed, and
higlighting dynamically the code segments corresponding to the
different execution steps.

@section{Marking modules and files for debugging in the top-level debugger}

The Ciao debugger is module-based. This allows skipping during the
debugging process all files (including system library files) except
those in which a bug is suspected. This saves having to explictily and
repetitively skip predicates in unrelated files during the debugging
process.  Also, there is an efficieny advantage: in order to be able
to run the debugger on a module, it must be loaded in @index{debug
(interpreted) mode}, which will execute slower than normal (compiled)
modules.  Thus, it is interesting to compile in debug mode only those
modules that need to be traced.  Instead of doing this (loading of
modules in one mode or another) by hand each time, in Ciao (re)loading
of modules in the appropriate mode is handled automatically by the
Ciao compiler.  However, this requires the user to @em{mark
explicitly} the modules in which debugging is to be performed.  The
simplest way of achieving this is by executing in the Ciao shell
prompt, for each suspicious module @tt{Module} in the program, the
command:

@begin{verbatim}
?- debug_module(Module).
@end{verbatim}

@noindent or, alternatively:

@begin{verbatim}
?- debug_module_source(Module).
@end{verbatim}

@noindent which in addition instructs the debugger to keep track of
the line numbers in the source file and to report them during
debugging. This is most useful when running the top-level inside the
@apl{emacs} editor since in that case the Ciao @concept{emacs mode}
allows performing full @concept{source-level debugging} in each module
marked as above, i.e., the source lines being executed will be
highlighted dynamically during debugging in a window showing the
source code of the module.

@cindex{user modules, debugging} Note that, since all files with no
module declaration belong to the pseudo-module @tt{user}, the command
to be issued for debugging a user file, say @tt{foo.pl}, is
@tt{debug_module(user)} or @tt{debug_module_source(user)}, and not
@tt{debug_module(foo)}.

The two ways of performing @concept{source-level debugging} are fully
compatible between them, i.e., Ciao allows having some modules loaded with
@pred{debug_module/1} and others with @pred{debug_module_source/1}. To
change from one interpreted mode to the other mode it suffices to select
the module with the new interpreted mode (debugger mode), using the
appropiate command, and reload the module.

The commands above perform in fact two related actions: first, they
let the compiler know that if a file containing a module with this
name is loaded, it should be loaded in interpreted mode (source or
traditional). In addition, they instruct the debugger to actually
prepare for debugging the code belonging to that module.  After that,
the modules which are to be debugged have to be (re)loaded so that
they are compiled or loaded for interpretation in the appropriate way.
The nice thing is that, due to the modular behaviour of the
compiler/top-level, if the modules are part of a bigger application,
it suffices to load the main module of the application, since this 
will automatically force the dependent modules which have changed to
be loaded in the appropriate way, including those whose @index{loading
mode} has changed (i.e., changing the loading mode has the effect of
forcing the required re-loading of the module at the appropriate
time). 

Later in the debugging process, as the bug location is isolated,
typically one will want to restrict more and more the modules where
debugging takes place.  To this end, and without the need for 
reloading, one can tell the debugger to not consider a module for
debugging issuing a @pred{nodebug_module/1} command, which counteracts
a @pred{debug_module/1} or @pred{debug_module_source/1} command with
the same module name, and reloading it (or the main file).

There are also two top-level commands @pred{set_debug_mode/1} and
@pred{set_nodebug_mode/1}, which accept as argument a file spec (i.e.,
@tt{library(foo)} or @tt{foo}, even if it is a user file) to be able
to load a file in interpreted mode without changing the set of modules
that the debugger will try to spy.

@section{The debugging process}

Once modules or user files are marked for debugging and reloaded, the
traditional debugging shell commands can be used (the documentation of the
@lib{debugger} library following this chapter contains all the commands and
their description), with the same meaning as in other classical Prolog 
systems. The differences in their behavior are:

@begin{itemize} 

@item Debugging takes place only in the modules in which it was activated,

@item @pred{nospy/1} and @pred{spy/1} accept sequences of predicate specs,
and they will search for those predicates only in the modules marked for
debugging (traditional or source-level debugging).

@item @pred{breakpt/6} and @pred{nobreakpt/6} allow setting breakpoints at
selected clause literals and will search for those literals only in the
modules marked for @concept{source-level debugging} (modules marked with
@pred{debug_module_source/1}).

@end{itemize}

In particular, the system is initially in @concept{nodebug} mode, in which
no tracing is performed. The system can be put in @concept{debug} mode by a
call to @pred{debug/0} in which execution of queries will proceed until the
first @em{spy-point} or @em{breakpoint}. Alternatively, the system can be
put in @concept{trace} mode by a call to @pred{trace/0} in which all
predicates will be trace.

@section{Marking modules and files for debugging with the embedded debugger}

The embedded debugger, as the interpreted debugger, has three
different modes of operation: debug, trace or nodebug. These debugger
modes can be set by adding @em{one} of the following package
declarations to the module:

@begin{verbatim}
:- use_package(debug).    
:- use_package(trace).    
:- use_package(nodebug).  
@end{verbatim}

@noindent and recompiling the application.  These declarations
@em{must} appear the last ones of all @decl{use_package} declarations
used. Also it is possible, as usual, to add the debugging package(s)
in the module declaration using the third argument of the
@pred{module/3} declaration (and they should also be the last ones in
the list), i.e., using one of:

@begin{verbatim}
:- module(..., ..., [..., debug]).
:- module(..., ..., [..., trace]).
:- module(..., ..., [..., nodebug]).
@end{verbatim}

The nodebug mode allows turning off any debugging (and also the
corresponding overhead) but keeping the spy-points and breakpoints in
the code. The trace mode will start the debugger for any predicate in
the file.

The embedded debugger has limitations over the interpreted
debugger. The most important is that the ``retry'' option is not
available. But it is possible to add, and remove, @concept{spy-points}
and @concept{breakpoins} using the predicates @pred{spy/1},
@pred{nospy/1}, @pred{breakpt/6} and @pred{nobreakpt/6}, etc. These
can be used in a clause declaration or as declarations. Also it is
possible to add in the code predicates for issuing the debugger (i.e.,
use debug mode, and in a clause add the predicate @pred{trace/0}).
Finally, if a spy declaration is placed on the entry point of an
executable (@tt{:- spy(main/1)}) the debugger will not start the
first time @pred{main/1} predicate is called, i.e., at the beginning
of program execution (however, it will if there are any subsequent
calls to @pred{main/1}). Starting the embedded debugger at the
beginning of the execution of a program can be done easily however by
simply adding the in trace mode.

Note that there is a particularly interesting way of using the
@concept{embedded debugger}: if an @em{application} is run in a shell
buffer which has been set with Ciao inferior mode (@key{M-x}
@tt{ciao-inferior-mode}) and this application starts emitting output
from the embedded debugger (i.e., which contains the embedded debugger
and is debugging its code) then the Ciao emacs mode will be able to
follow these messages, for example tracking execution in the source
level code. This also works if the application is written in a
combination of languages, provided the parts written in Ciao are
compiled with the embedded debugger package and is thus a covenient
way of debugging multi-language applications. The only thing needed is
to make sure that the output messages appear in a shell buffer that is
in Ciao inferior mode.

See the following as a general example of use of the embedded debugger:
@begin{verbatim}
:- module( foo,[main/1],[assertions, debug]).

:- entry main/1.

main(X) :- 
         display(X),
         spy(foo), 
         foo(X), 
         notrace,
         nl.

foo([]).  
foo([X|T]) :-  
        trace,
        bar(X), 
        foo(T).

bar(X) :- 
        display(X).
@end{verbatim}

@section{The procedure box control flow model}

During debugging the interpreter prints out a sequence of goals in various
states of instantiation in order to show the state that the program has
reached in its execution.  However, in order to understand what is
occurring it is necessary to understand when and why the interpreter prints
out goals.  As in other programming languages, key points of interest are
procedure entry and return, but in Prolog there is the additional
complexity of backtracking.  One of the major confusions that novice Prolog
programmers have to face is the question of what actually happens when a
goal fails and the system suddenly starts backtracking.  The Procedure Box
model of Prolog execution views program control flow in terms of movement
about the program text.  This model provides a basis for the debugging
mechanism in the interpreter, and enables the user to view the behaviour of
the program in a consistent way. It also provides the basis for the
visualization performed on the source level program when source level
program when @concept{source-level debugging} is activated within
@apl{emacs}.


Let us look at an example Prolog procedure:

@image{byrdbox}

The first clause states that @tt{Y} is a descendant of @tt{X} if
@tt{Y} is an offspring of @tt{X}, and the second clause states that
@tt{Y} is a descendant of @tt{X} if @tt{Z} is an offspring of @tt{X}
and @tt{Y} is a descendant of @tt{Z}.  In the diagram a box has been
drawn around the whole procedure and labelled arrows indicate the
control flow in and out of this box.  There are four such arrows which
we shall look at in turn.

@begin{itemize}

@item @bf{Call}

This arrow represents initial invocation of the procedure.  When a goal
of the form @tt{descendant(X,Y)} is required to be satisfied, control
passes through the Call port of the descendant box with the
intention of matching a component clause and then satisfying any
subgoals in the body of that clause.  Note that this is independent of
whether such a match is possible; i.e. first the box is called, and then
the attempt to match takes place.  Textually we can imagine moving to
the code for descendant when meeting a call to descendant in some other
part of the code.

@item @bf{Exit}

This arrow represents a successful return from the procedure.  This
occurs when the initial goal has been unified with one of the component
clauses and any subgoals have been satisfied.  Control now passes out of
the Exit port of the descendant box.  Textually we stop following
the code for descendant and go back to the place we came from.

@item @bf{Redo}

This arrow indicates that a subsequent goal has failed and that the system
is backtracking in an attempt to find alternatives to previous solutions.
Control passes through the Redo port of the descendant box.  An attempt
will now be made to resatisfy one of the component subgoals in the body of
the clause that last succeeded; or, if that fails, to completely rematch
the original goal with an alternative clause and then try to satisfy any
subgoals in the body of this new clause.  Textually we follow the code
backwards up the way we came looking for new ways of succeeding, possibly
dropping down on to another clause and following that if necessary.

@item @bf{Fail}

This arrow represents a failure of the initial goal, which might occur
if no clause is matched, or if subgoals are never satisfied, or if any
solution produced is always rejected by later processing.  Control now
passes out of the Fail port of the descendant box and the system
continues to backtrack.  Textually we move back to the code which called
this procedure and keep moving backwards up the code looking for choice
points.
@end{itemize}

In terms of this model, the information we get about the procedure box is
only the control flow through these four ports.  This means that at this
level we are not concerned with which clause matches, and how any subgoals
are satisfied, but rather we only wish to know the initial goal and the
final outcome.  However, it can be seen that whenever we are trying to
satisfy subgoals, what we are actually doing is passing through the ports
of @em{their} respective boxes.  If we were following this (e.g.,
activating @concept{source-level debugging}), then we would have complete
information about the control flow inside the procedure box.

Note that the box we have drawn around the procedure should really be seen
as an invocation box.  That is, there will be a different box for each
different invocation of the procedure.  Obviously, with something like a
recursive procedure, there will be many different Calls and Exits in the
control flow, but these will be for different invocations.  Since this
might get confusing each invocation box is given a unique integer
identifier in the messages, as described below.

Note that not all procedure calls are traced; there are a few basic
predicates which have been made invisible since it is more convenient
not to trace them. These include debugging directives, basic control
structures, and some builtins. This means that messages will never be 
printed for these predicates during debugging.

@section{Format of debugging messages}

This section explains the two formats of the message output by the debugger
at a port. All trace messages are output to the terminal regardless of
where the current output stream is directed (which allows tracing programs
while they are performing file I/O). The basic format, which will be shown
in traditional debug and in source-level debugging within Ciao @apl{emacs}
mode, is as follows:

@begin{verbatim}
S  13  7  Call: T user:descendant(dani,_123) ?
@end{verbatim}

@tt{S} is a spy-point or breakpoint indicator.  It is printed as '@tt{+}',
indicating that there is a spy-point on @tt{descendant/2} in module
@tt{user}, as @tt{'B'} denoting a breakpoint, or as ' ', denoting no
spy-point or breakpoint. If there is a spy-point and a breakpoint in the
same predicate the spy-point indicator takes preference over breakpoint
indicator.

@tt{T} is a subterm trace.  This is used in conjunction with the
@tt{^} command (set subterm), described below.  If a subterm has been
selected, @tt{T} is printed as the sequence of commands used to select
the subterm.  Normally, however, @tt{T} is printed as ' ',
indicating that no subterm has been selected.

The first number is the unique invocation identifier.  It is always
nondecreasing (provided that the debugger is switched on) regardless of
whether or not the invocations are being actually seen.  This number 
can be used to cross correlate the trace messages for the various ports,
since it is unique for every invocation.  It will also give an
indication of the number of procedure calls made since the start of the
execution.  The invocation counter starts again for every fresh
execution of a command, and it is also reset when retries (see later)
are performed.

The number following this is the @em{current depth}; i.e., the number of
direct @em{ancestors} this goal has.  The next word specifies the
particular port (@tt{Call}, @tt{Exit}, @tt{Redo} or @tt{Fail}).  The goal
is then printed so that its current instantiation state can be inspected.
The final @tt{?} is the prompt indicating that the debugger is waiting for
user interaction. One of the option codes allowed (see below) can be input
at this point.

The second format, quite similar to the format explained above, is shown
when using source-level debugging outside the Ciao @apl{emacs} mode, and it
is as follows:

@begin{verbatim}
	   In /home/mcarlos/ciao/foo.pl (5-9) descendant-1
S  13  7  Call: T user:descendant(dani,_123) ?
@end{verbatim}

This format is identical to the format above except for the first line,
which contains the information for location of the point in the source
program text where execution is currently at. The first line contains the
name of the source file, the start and end lines where the literal can be
found, the substring to search for between those lines and the number of
substrings to locate. This information for locating the point on the source
file is not shown when executing the source-level debugger from the Ciao
@apl{emacs} mode.

Ports can be ``unleashed'' by calling the @pred{leash/1} predicate omiting
that port in the argument. This means that the debugger will stop but user
interaction is not possible for an unleashed port. Obviously, the @tt{?}
prompt will not be shown in such messages, since the user has specified
that no interaction is desired at this point.

@section{Options available during debugging}
@cindex{debug options}

This section describes the particular options that are available when the
debugger prompts after printing out a debugging message. All the options
are one letter mnemonics, some of which can be optionally followed by a
decimal integer. They are read from the terminal with any blanks being
completely ignored up to the next terminator (carriage-return, line-feed,
or escape). Some options only actually require the terminator; e.g., the
creep option, only requires @key{RET}.

The only option which really needs to be remembered is '@tt{h}'
(followed by @key{RET}). This provides help in the form of the following
list of available options.

@begin{verbatim}
<cr>   creep            c      creep
 l     leap             s      skip
 r     retry            r <i>  retry i
 f     fail             f <i>  fail i
 d     display          p      print
 w     write            v <I>  variable(s)
 g     ancestors        g <n>  ancestors n
 n     nodebug          =      debugging
 +     spy this         -      nospy this
 a     abort            
 @@     command          u      unify
 <     reset printdepth < <n>  set printdepth
 ^     reset subterm    ^ <n>  set subterm
 ?     help             h      help
@end{verbatim}

@begin{itemize}

@item @tt{c}
(@index{creep})

causes the debugger to single-step to the very next port and print a
message.  Then if the port is leashed the user is prompted for further
interaction.  Otherwise it continues creeping.  If leashing is off, creep
is the same as leap (see below) except that a complete trace is printed on
the terminal.

@item @tt{l} 
(@index{leap}) 

causes the interpreter to resume running the program, only stopping when a
spy-point or breakpoint is reached (or when the program terminates).
Leaping can thus be used to follow the execution at a higher level than
exhaustive tracing.  All that is needed to do is to set spy-points and
breakpoints on an evenly spread set of pertinent predicates or lines, and
then follow the control flow through these by leaping from one to the
other.

@item @tt{s} 
(@index{skip})

is only valid for Call and Redo ports, if it is issued in Exit or Fail
ports it is equivalent to creep.  It skips over the entire execution of the
predicate. That is, no message will be seen until control comes back to
this predicate (at either the Exit port or the Fail port).  Skip is
particularly useful while creeping since it guarantees that control will be
returned after the (possibly complex) execution within the box.  With skip
then no message at all will appear until control returns to the Exit port
or Fail port corresponding to this Call port or Redo port. This includes
calls to predicates with spy-points and breakpoints set: they will be
masked out during the skip.  There is a way of overriding this: the @tt{t}
option after a @key{^C} interrupt will disable the masking.  Normally,
however, this masking is just what is required!

@item @tt{r}
(@index{retry})

can be used at any of the four ports (although at the Call port it has no
effect).  It transfers control back to the Call port of the box.  This
allows restarting an invocation when, for example, it has left the
programmer with some weird result.  The state of execution is exactly the
same as in the original call (unless the invocation has performed side
effects, which will not be undone).  When a retry is performed the
invocation counter is reset so that counting will continue from the current
invocation number regardless of what happened before the retry.  This is in
accord with the fact that execution has, in operational terms, returned to
the state before anything else was called.

If an integer is supplied after the retry command, then this is taken as
specifying an invocation number and the system tries to get to the Call
port, not of the current box, but of the invocation box specified.  It does
this by continuously failing until it reaches the right place.
Unfortunately this process cannot be guaranteed: it may be the case that
the invocation the programmer is looking for has been cut out of the
search space by cuts in the program.  In this case the system fails to the
latest surviving Call port before the correct one.

@item @tt{f}
(@index{fail})

can be used at any of the four ports (although at the Fail port it has no
effect).  It transfers control to the Fail port of the box, forcing the
invocation to fail prematurely.  If an integer is supplied after the
command, then this is taken as specifying an invocation number and the
system tries to get to the Fail port of the invocation box specified.  It
does this by continuously failing until it reaches the right place.
Unfortunately, as before, this process cannot be guaranteed.

@item @tt{d}
(@index{display})

displays the current goal using @tt{display/1}.  See @tt{w} below. 

@item @tt{p}
(@index{print})

re-prints the current goal using @tt{print/1}.  Nested structures will be
printed to the specified @em{printdepth} (see below).

@item @tt{w}
(@index{write})

writes the current goal on the terminal using @tt{write/1}. 

@item @tt{v}
(@index{variables})

writes the list of the modified variables and their values.  If a
variable name (identifier) @tt{N} is supplied, then the value of
variable @var{N} is shown.

@item @tt{g}
(@index{ancestors})

provides a list of ancestors to the current goal, i.e., all goals that are
hierarchically above the current goal in the calling sequence. It is always
possible to jump to any goal in the ancestor list (by using retry,
etc.). If an integer @tt{n} is supplied, then only @tt{n} ancestors will be
printed.  That is to say, the last @tt{n} ancestors will be printed
counting back from the current goal.  Each entry in the list is preceded by
the invocation number followed by the depth number (as would be given in a
trace message).

@item @tt{n} 
(@index{nodebug})

switches the debugger off.  Note that this is the correct way to switch
debugging off at a trace point. The @tt{@@} option cannot be used because
it always returns to the debugger.

@item @tt{=}
(@index{debugging})

outputs information concerning the status of the current debugging
session.

@item @tt{+}
@index{spy}

sets a spy-point on the current goal.

@item @tt{-}
(@index{nospy})

removes the spy-point from the current goal.

@item @tt{a}
(@index{abort})

causes an abort of the current execution.  All the execution states built
so far are destroyed and the system is put right back at the top-level of
the interpreter.  (This is the same as the built-in predicate
@tt{abort/0}.)

@item @tt{@@}
(@index{command})

allows calling arbitrary goals. The initial message @tt{| ?- } will be
output on the terminal, and a command is then read from the terminal and
executed as if it was at top-level.

@item @tt{u}
(@index{unify})

is available at the Call port and gives the option of providing a solution
to the goal from the terminal rather than executing the goal.  This is
convenient, e.g., for providing a ``stub'' for a predicate that has not yet
been written.  A prompt @tt{|: } will be output on the terminal, and the
solution is then read from the terminal and unified with the goal.

@item @tt{<}
(@index{printdepth})

sets a limit for the subterm nesting level that is printed in messages.
While in the debugger, a printdepth is in effect for limiting the subterm
nesting level when printing the current goal.  When displaying or writing
the current goal, all nesting levels are shown.  The limit is initially 10.
This command, without arguments, resets the limit to 10.  With an argument
of @tt{n} the limit is set to @tt{n}.

@item @tt{^}
(@index{subterm})

sets the subterm to be printed in messages.  While at a particular port, a
current subterm of the current goal is maintained.  It is the current
subterm which is displayed, printed, or written when prompting for a
debugger command.  Used in combination with the printdepth, this provides a
means for navigating in the current goal for focusing on the part which is
of interest.  The current subterm is set to the current goal when arriving
at a new port.  This command, without arguments, resets the current subterm
to the current goal.  With an argument of @tt{n} (greater than 0 and less
or equal to the number of subterms of the current subterm), the current
subterm is replaced by its @tt{n}'th subterm.  With an argument of @tt{0},
the current subterm is replaced by its parent term.

@item @tt{?} or @tt{h}
(@index{help})

displays the table of options given above. 

@end{itemize}

@section{Calling predicates that are not exported by a module}

The Ciao module system does not allow calling predicates which are not
exported during debugging. However, as an aid during debugging, this is
allowed (only from the top-level and for modules which are in debug mode or
source-level debug mode) using the @pred{call_in_module/2} predicate.

Note that this does not affect analysis or optimization issues, since it
only works on modules which are loaded in debug mode or source-level debug
mode, i.e. unoptimized.

@section{Acknowledgements (debugger)}

Originally written by Andrzej Ciepielewski. Minor modifications by Mats
Carlsson. Later modifications (17 Dec 87) by Takashi Chikayama (making
tracer to use @pred{print/1} rather than @pred{write/1}, temporarily
switching debugging flag off while writing trace message and within
``break'' level). Additional modifications by Kish Shen (May 88): subterm
navigation, handle unbound args in @pred{spy/1} and @pred{nospy/1},
trapping arithmetics errors in debug mode. Adapted then to &-Prolog and
Ciao by Daniel Cabeza and included in the Ciao version control system. Extended
for source-level debugging by Manuel C.  Rodr@'{i}guez.  Option that shows 
variable names and values (@tt{v <N>}) implemented by Edison Mera (2009).
(See changelog if included in the document or in the version maintenance 
 system for more detailed documentation on changes.)

").

%% The following changes has been made to the documentation in order to fit the
%% actual debugger options:
%%  1.- From the debugger options remove b break on the a abort line
%%  2.- When explainning the debugger options on the n options replace the 
%%  sentence "The @tt{@@} or @tt{b} options" for "The @tt{@@} option" the move 
%%  the sentence from plural to singular.
%%  3.- Remove the following after the a option
%% @item @tt{b}
%% @index{break}
%% introduces an interpreter break-point: current execution flow is suspended,
%% and a new interpreter top-level started 'on top' of the previous execution,
%% which is 'sitting underneath' it. When the break-point is ended (with
%% @key{^D}) previous execution is resumed, and the user will be reprompted at
%% the port at which execution broke. The new execution is completely separate
%% from the suspended one; the invocation numbers will start again from 1
%% during the break. The debugger is temporarily switched off as the
%% break-point is called and will be re-switched on when the break is
%% finished.  However, any changes to the leashing or to spy-points will
%% remain in effect.  (This option might not be temporarily available.)



%% @section{Format of messages}
%% 
%% invisible predicates
%%  including @code{trace/0}, @code{debug/0}, @code{notrace/0},
%% @code{nodebug/0}, @code{spy/1}, @code{nospy/1}, @code{nospyall/0},
%% @code{leash/1}, @code{debugging}, @code{true/0}, @code{!/0},
%% @code{','/2}, @code{'->'/2}, @code{;/2}, @code{'\+'/1}, and @code{if/3}.

%%  &     blocked goals    & <n>  nth blocked goal
%% @item &
%% Print @dfn{blocked} goals prints a list of the goals which are currently
%% blocked in the current debugging session together with the variable that
%% each such goal is suspended on.  The goals are enumerated from 1 and up.
%% If you supply an integer @var{n}, then only that goal will be printed.
%% The goals are printed using @code{print/1}. and each entry is preceded
%% by the goal number followed by the variable name. 


%% 
%% There are two exceptions to the above debugger message format.  A message
%% 
%% @begin{verbatim}
%% @tt{S} -  -  Block: p(_133)
%% @end{verbatim}
%% 
%% @noindent
%% indicates that the debugger has encountered a @dfn{blocked} goal, i.e.
%% one which is temporarily suspended due to insufficiently instantiated
%% arguments (@ref{Procedural}).  No interaction takes place at this
%% point, and the debugger simply proceeds to the next goal in the
%% execution stream.  The suspended goal will be eligible for execution
%% once the blocking condition ceases to exist, at which time a message
%% 
%% 
%% @begin{verbatim}
%% @tt{S} -  -  Unblock: p(_133)
%% @end{verbatim}
%% 
%% @noindent
%% is printed.

%% @section{Error messages and troubleshooting}
%% 
%% Speed:
%% 
%%       Of course, debugging modules, being interpreted, are executed
%% much slower than compiled modules.  Why did you write flawed code, in
%% the first place?
%% 
%% 
%% Wrong module declaration:
%% 
%%   ?- debug_modules(library(write)).
%%   {Bad module library(write) - must be an atom}
%%   {No module is selected for debugging}
%% 
%%     It is right that the above is wrong: library(write) is not a
%% module name, but a file spec.  The name of the module is simply write,
%% so debug_modules(write) is what is needed.
%% 
%% 
%% ""I do not want to have to reload modules in order to snoop into them"":
%% 
%%    Sorry folks, this is a partially necessary evil: otherwise the
%% whole process (from the startup and execution point of view) would be
%% much slower, since all modules would have to be explicitly marked as
%% unloadable even if no debugging is taken place, which is burden for
%% correct applications.
