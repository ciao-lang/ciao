:- module(toplevel_proc, [
    start/1, kill/1, 
    format/3,
    wait_for_answer/4
   ], [fsyntax, dcg, assertions]).

:- doc(title, "Interactive top-level as an external process").

:- doc(author, "Remy Haemmerle").
:- doc(author, "Jose F. Morales (minor)").

:- doc(module, "This module provides an abstraction for starting new
   toplevels as an external processes (see @lib{process}).

   This modules is currently used for testing automatically
   interactions with the toplevel. Buffering on streams is
   disabled.").

:- doc(bug, "Make input_set_unbuf optional").
:- doc(bug, "Make it more robust (see stream_wait)").
:- doc(bug, "Experimental, use with care").
:- doc(bug, "Missing a polite and clean 'stop/1' predicate").

:- use_module(library(process)).
:- use_module(library(format), [format/2, format/3]).
:- use_module(library(stream_wait)).
:- use_module(engine(stream_basic), [flush_output/1, close/1]).
:- use_module(engine(io_basic)).

:- use_module(ciaobld(config_common), [cmd_path/4]).
cmd_execname(ciaosh) := ~cmd_path(core, plexe, 'ciaosh').

:- pred format(TopLevelProc, String, Args) # "Equivalent to
   @pred{format(IntputStream, String, Args)} from module @lib{format},
   where the @var{InputStream} correspond to the input stream of the
   top-level identified by @var{TopLevelProc}.".

format(TL, _Str, _Arg):-
    var(TL), !,
    throw(error(instantiation_error, 'toplevel_proc:format'/3-1)).
format(TL, Str, Args):-
    Status = ~get_status(TL), !, 
    (
        var(Status) ->
        true
    ;
        throw(error(toplevel_proc_dead, 'toplevel_proc:format'/3-1))
    ),
    format:format(~get_input(TL), Str, Args), 
    flush_output(~get_input(TL)).
format(TL, _Str, _Args):-       
    throw(error(domain_error(toplevel_proc, TL), 'toplevel_proc:format'/3-1)).
    
get_input(toplevel(Stream, _, _, _, _)) :=  Stream.
get_output(toplevel(_, Stream, _, _, _)) :=  Stream.
get_error(toplevel(_, _, Stream, _, _)) :=  Stream.
get_process(toplevel(_, _, _, Process, _)) :=  Process.
get_status(toplevel(_, _, _, _, Status)) :=  Status.

:- pred start(TopLevelProc) # "Starts a Ciao Top-level and unifies
   @var{TopLevelProc} with an implementation defined identifier.".

start(TL):- 
    TL = toplevel(IS, OS, ES, Process, _),
    process_call(~cmd_execname(ciaosh), [],
                 [stdin(stream(IS)), stdout(stream(OS)), stderr(stream(ES)),
                  background(Process)]),
    stream_wait:input_set_unbuf(OS),
    stream_wait:input_set_unbuf(ES), 
    toplevel_proc:format(TL, "true.\n\n", []), 
    (
        wait_for_answer(TL, _, _, 2000000) ->
        true
    ;
        throw(error(unknown_error, 'toplevel_proc:start'/1))
    ).

:- pred kill(TopLevelProc) # "Kills the top-level identified by
   @var{TopLevelProc}, after having close its associated stream.
   Silently suceeds if the top-level have been previously killed".

kill(TL):-
    var(~get_status(TL)), !, 
    close(~get_input(TL)), 
    close(~get_output(TL)), 
    close(~get_error(TL)), 
    process_kill(~get_process(TL)),
    % join to avoid zombies (kill just send signal)
    process_join(~get_process(TL)).
kill(_).

:- pred wait_for_answer(TopLevelProc, Str, EStr, TimeOut) # "Waits for
   an answer form the toplevel identified by @var{TopLevelProc} and
   unifies @var{Str} with the list of characters reads so far.
   Answers are assumed to occur when either the string \"yes\" or the
   string \"no\" are read for the output of the toplevel.  The system
   waits at most @var{TimeOut} microseconds between each character
   read.".

wait_for_answer(TL, Str, EStr, TimeOut) :-
    wait_for_answer_(~get_output(TL), -1, TimeOut, Str, []),
    get_error_string_(~get_error(TL), EStr, []).

wait_for_answer_(S, C0, TimeOut) -->
    {
        stream_wait:input_wait(S, TimeOut), !,
        get_code(S, C)
    },
    [C],
    {
        (C0 = -1,  C = 0'n, C0_ = C;
        C0 = 0'n, C = 0'o, C0_ = 0;
        C0 = -1,  C = 0'y, C0_ = C;
        C0 = 0'y, C = 0'e, C0_ = C;
        C0 = 0'e, C = 0's, C0_ = 0);
        C0_ = -1 
    },!,
    (
        {C0_ = 0} ->
        []
    ;
        {C = -1} ->
        [-1]
    ;
        wait_for_answer_(S, C0_, TimeOut)
    ).

get_error_string_(S) -->
    {
        stream_wait:input_wait(S, 0),!,
        get_code(S, C)
    },
    [C], 
    get_error_string_(S).
get_error_string_(_) --> [].
