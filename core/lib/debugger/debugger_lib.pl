:- module(debugger_lib, [], [assertions, dcg, hiord, datafacts, define_flag]).

:- use_module(engine(io_basic)).
:- use_module(engine(stream_basic)).
:- use_module(engine(messages_basic), [display_list/1]).
:- use_module(library(format)).
:- use_module(library(write)).
:- use_module(library(sort)).
:- use_module(library(read), [read_term/3, read/2]).
%
:- use_module(engine(runtime_control), [module_split/3]).
:- use_module(engine(internals), ['$prompt'/2, '$setarg'/4]).
:- if(defined(optim_comp)).
:- use_module(engine(rt_exp), ['$current_predicate'/2, '$predicate_property'/3]).
:- else.
:- use_module(engine(internals), ['$current_predicate'/2, '$predicate_property'/3]).
:- endif.
:- use_module(engine(debugger_support)).
:- use_module(library(system), [cyg2win_a/3, using_windows/0]).
:- use_module(library(lists), [append/3, union/3]).
:- use_module(library(hiordlib), [maplist/2, filter/3, foldl/4]).
%
:- use_module(library(toplevel/toplevel_io)).

% :- multifile define_flag/3.
% define_flag(debug, [on,debug,trace,off], off).

% ---------------------------------------------------------------------------
%! # Debugger state

% Debugger_state = s(DebugFlag, OptDebugFlag, int, int, AncestorList)
% DebugFlag = trace|debug|off
% OptDebugFlag = trace|debug|off

debugger_setting(Old, New) :-
    get_debugger_state(State),
    arg(1, State, Old),
    '$setarg'(1, State, New, true),
    adjust_debugger_state(State, New).

:- export(get_debugger_state/1).
get_debugger_state(L) :-
    '$debugger_state'(L, L).

:- export(adjust_debugger_state/2).
adjust_debugger_state(State, New) :-
    '$setarg'(2, State, New, true),
    '$setarg'(3, State, 1000000, true),
    '$debugger_mode'.

:- if(defined(optim_comp)).
:- else.
% TODO: move to debugger_support.pl?
:- export(reset_debugger/1).
reset_debugger(State) :-
    '$debugger_state'(State, s(off, off, 1000000, 0, [])),
    '$debugger_mode'.
:- endif.

set_debugger(State) :-
    '$debugger_state'(_, State),
    '$debugger_mode'.

:- export(debug/0).
:- pred debug/0 # "Switches the debugger on. The interpreter will
    stop at all ports of procedure boxes of spied predicates.".

debug :-
    debugger_setting(_, debug),
    what_is_on(debug).

:- export(nodebug/0).
:- pred nodebug/0 # "Switches the debugger off.  If there are any
    spy-points set then they will be kept but disabled.".

nodebug :- notrace.

:- export(trace/0).
:- pred trace/0 # "Start tracing, switching the debugger on if
    needed.  The interpreter will stop at all leashed ports of
    procedure boxes of predicates either belonging to debugged
    modules or called from clauses of debugged modules.  A message
    is printed at each stop point, expecting input from the user
    (write @tt{h} to see the available options).".

trace :-
    debugger_setting(_, trace),
    what_is_on(trace).

:- export(notrace/0).
:- pred notrace/0 # "Equivalent to @pred{nodebug/0}.".

notrace :-
    debugger_setting(_, off),
    what_is_on(off).

% ---------------------------------------------------------------------------
%! ## Flag for debugging RTC

:- export(debug_rtc_db/0).
:- data debug_rtc_db/0.
debug_rtc_db. % (initial state)

:- export(debugrtc/0).
:- pred debugrtc/0 # "Start tracing when a run-time check error be raised".
debugrtc :-
    ( debug_rtc_db -> true ; assertz_fact(debug_rtc_db) ).

:- export(nodebugrtc/0).
:- pred nodebugrtc/0 # "Do not start tracing when a run-time check error be raised".
nodebugrtc :-
    retract_fact(debug_rtc_db).

:- export(tracertc/0).
:- pred tracertc/0 # "Start tracing if the debugger and debug_rtc are activated".
tracertc :-
    ( get_debugger_state(State),
      \+ arg(1, State, off),
      debug_rtc_db ->
        trace
    ; true
    ).

% ---------------------------------------------------------------------------
%! ## Per-module debugging state

:- use_module(library(aggregates), [findall/3]).

:- data debug_mod/1.

:- export(current_debugged/1).
current_debugged(Ms) :- findall(M, current_fact(debug_mod(M)), Ms).

:- export(debug_module/1).
:- pred debug_module(Module) : atm(Module)

   # "To debug the predicates in a module that module needs to be
   marked for debugging and re-loaded to be compiled in debug mode.
   This predicate marks module @var{Module} for debugging. This has
   two effects: The debugger will mark the module so that when
   predicates in this module are called the debugger is entered. In
   addition, the module is marked specially so that when (re)loaded it
   will be (re)compiled in debug mode. It also marks the module as
   'modified' so that (re)loading this file or a main file that uses
   this module will force it to actually be recompiled. Note that in
   addition to marking it for debugging the module @em{needs to be
   reloaded} after marking it, for all this to occur. In other words,
   the proper sequence to debug module @tt{m} is:

   @begin{itemize}
   ?- debug_module(m).  % Mark module m for debugging.
   ?- use_module(m).    % Load module m in debugging mode.
   ?- trace.            % Turn debugger on in, e.g., tracing mode.
   @end{itemize}

   This allows selecting which parts of the program are being debugged
   at the module level. It also allows having some modules loaded
   compiled in debug mode and others in standard mode (faster).".

debug_module(M) :- atom(M), !,
    ( current_fact(debug_mod(M)) ->
        true
    ; assertz_fact(debug_mod(M))
    ).
debug_module(M) :-
    format(user_error, '{Bad module ~q - must be an atom}~n', [M]).

:- export(in_debug_module/1).
in_debug_module(G) :-
    functor(G, F, _),
    current_fact(debug_mod(M)),
    module_split(F, M, _).

:- export(nodebug_module/1).
:- pred nodebug_module(Module) : atm(Module)

   # "Unmark module @var{Module} for debugging.  The debugger will not
   enter debug mode for module @var{Module}.  When issuing this
   command at the toplevel shell, the compiler is instructed also to
   set to @em{compile} the loading mode of the files defining that
   module, i.e., the module will be compiled in standard mode if
   reloaded.".

nodebug_module(M) :- % If M is a var, nodebug for all
    retractall_fact(debug_mod(M)).
%    what_is_debugged.

%% This entry point is only for documentation purposes.
:- export(debug_module_source/1).
:- pred debug_module_source(Module) : atm(Module)
   # "The debugger will take into account module @var{Module} (assuming
   it is loaded in source-level debug mode).  When issuing this
   command at the toplevel shell, the compiler is instructed also to
   set to @em{interpret} the loading mode of files defining that
   module and also to mark it as 'modified' so that (re)loading this
   file or a main file that uses this module will force it to be
   reloaded for source-level debugging.".

debug_module_source(M) :-
    debug_module(M).

% ---------------------------------------------------------------------------
%! ## Port leashing

:- data leashed/1.
%(initial state)
leashed(call).
leashed(exit).
leashed(redo).
leashed(fail).

:- prop port(X) + regtype.
port(call).
port(exit).
port(redo).
port(fail).

:- export(leash/1).
:- pred leash(Ports) : list(port)
   # "Leash on ports @var{Ports}, some of @tt{call}, @tt{exit},
   @tt{redo}, @tt{fail}. By default, all ports are on leash.".

leash(L) :-
    nonvar(L),
    leash1(L),
    !.
leash(L) :-
    format(user_error, '{Bad leash specification ~q}~n', [L]).

leash1(half) :- !, leash1([call, redo]).
leash1(full) :- !, leash1([call, exit, redo, fail]).
leash1(loose) :- !, leash1([call]).
leash1(none) :- !, leash1([]).
leash1(tight) :- !, leash1([call, redo, fail]).
leash1(L) :-
    list(L),
    retractall_fact(leashed(_)), leashlist(L), what_is_leashed.

leashlist([]).
leashlist([Port|L]) :-
    assertz_fact(leashed(Port)),
    leashlist(L).

% ---------------------------------------------------------------------------
%! ## Maximum invocation depth

:- data debugdepth/1.
debugdepth(100000). % (initial value)

:- export(maxdepth/1).
:- pred maxdepth(MaxDepth) : int
   # "Set maximum invocation depth in debugging to
   @var{MaxDepth}. Calls to compiled predicates are not included in
   the computation of the depth.".

maxdepth(D) :-
    integer(D), !,
    retractall_fact(debugdepth(_)),
    assertz_fact(debugdepth(D)),
    what_maxdepth.
maxdepth(D) :-
    format(user_error, '{Bad maxdepth ~q - must be an integer}~n', [D]).

% ---------------------------------------------------------------------------
%! ## Spypoints

:- doc(doinclude, multpredspec/1).
:- prop multpredspec/1 + regtype.
multpredspec(Mod:Spec) :- atm(Mod), multpredspec(Spec).
multpredspec(Name/Low-High) :- atm(Name), int(Low), int(High).
multpredspec(Name/(Low-High)) :- atm(Name), int(Low), int(High).
multpredspec(Name/Arity) :- atm(Name), int(Arity).
multpredspec(Name) :- atm(Name).

:- export(spy/1).
:- pred spy(PredSpec) : sequence(multpredspec)
   # "Set spy-points on predicates belonging to debugged modules and
   which match @var{PredSpec}, switching the debugger on if
   needed. This predicate is defined as a prefix operator by the
   toplevel.".

spy(Preds) :-
    get_debugger_state(State),
    ( arg(1, State, off) -> debug ; true ),
    parse_functor_spec(Preds, X, spy1(X)).

:- export(nospy/1).
:- pred nospy(PredSpec) : sequence(multpredspec)
   # "Remove spy-points on predicates belonging to debugged modules
   which match @var{PredSpec}. This predicate is defined as a prefix
   operator by the toplevel.".

nospy(Preds) :-
    parse_functor_spec(Preds, X, nospy1(X)).

spy1(Pred) :-
    functor(Pred, N, A),
    warn_if_udp(Pred, N, A),
    install_spypoint(Pred, N, A).

nospy1(Pred) :-
    functor(Pred, N, A),
    warn_if_udp(Pred, N, A),
    remove_spypoint(Pred, N, A).

:- if(defined(optim_comp)).
warn_if_udp(_, N, A) :- '$predicate_property'(N/A, _, _), !.
:- else.
warn_if_udp(F, _, _) :- '$predicate_property'(F, _, _), !.
:- endif.
warn_if_udp(_, N, A) :-
    format(user_error, '{Warning: No definition for ~q}~n', [N/A]).

install_spypoint(F, N, A) :-
    '$spypoint'(F, on, on), !,
    format(user, '{There is already a spypoint on ~q}~n', [N/A]).
install_spypoint(F, N, A) :-
    '$spypoint'(F, off, on), !,
    format(user, '{Spypoint placed on ~q}~n', [N/A]).
install_spypoint(_, N, A) :-
    format(user, '{Cannot spy built-in predicate ~q}~n', [N/A]).

remove_spypoint(F, N, A) :-
    '$spypoint'(F, off, off), !,
    format(user, '{There is no spypoint on ~q}~n', [N/A]).
remove_spypoint(F, N, A) :-
    '$spypoint'(F, on, off), !,
    format(user, '{Spypoint removed from ~q}~n', [N/A]).
remove_spypoint(_, N, A) :-
    top_nl,
    format(user, '{Cannot spy built-in predicate ~q}~n', [N/A]).

:- export(nospyall/0).
:- pred nospyall/0 # "Remove all spy-points.".

nospyall :-
    spypoint(F),
    '$spypoint'(F, _, off),
    fail.
nospyall :-
    format(user, '{All spypoints removed}~n', []).

spypoint(X) :-
    '$current_predicate'(_, X),
    '$spypoint'(X, on, on).

% ---------------------------------------------------------------------------
% (helper for spy/1 and nospy/1)

:- if(defined(optim_comp)).
:- use_module(engine(rt_exp), ['$module_concat'/3]).
module_concat(M,F,MF) :- '$module_concat'(F,M,MF). % TODO: reversed args, merge?
:- else.
:- use_module(engine(internals), [module_concat/3]).
:- endif.

:- meta_predicate parse_functor_spec(?, ?, goal).
parse_functor_spec(V, _, _) :-
    var(V), !,
    format(user_error, '{A variable is a bad predicate indicator}~n', []).
parse_functor_spec((S, Ss), GoalArg, Goal) :-
    parse_functor_spec(S, GoalArg, Goal),
    parse_functor_spec(Ss, GoalArg, Goal).
parse_functor_spec(S, GoalArg, Goal) :-
    Flag=f(0),
    ( functor_spec(S, Name, Low, High, M),
        current_fact(debug_mod(M)),
        module_concat(M, Name, PredName),
        '$current_predicate'(PredName, GoalArg), % TODO: optim-comp: (old comment) don't work; why?
        functor(GoalArg, _, N),
        N >= Low, N =< High,
        '$setarg'(1, Flag, 1, true),
        parse_functor_spec_call(Goal),
        fail
    ; Flag=f(0),
        format(user_error,
            "{Bad predicate indicator or predicate undefined "||
            "in modules currently debugged:~n ~w}~n", [S]),
        fail
    ; true
    ).

:- meta_predicate parse_functor_spec_call(goal).
:- if(defined(optim_comp)).
parse_functor_spec_call(Goal) :- call(Goal).
:- else.
parse_functor_spec_call(Goal) :- '$nodebug_call'(Goal).
:- endif.

:- export(functor_spec/5).
functor_spec(Mod:Spec, Name, Low, High, Mod) :-
    functor_spec(Spec, Name, Low, High, _).
functor_spec(Name/Low-High, Name, Low, High, _) :-
    atom(Name),
    integer(Low), integer(High), !.
functor_spec(Name/(Low-High), Name, Low, High, _) :-
    atom(Name),
    integer(Low), integer(High), !.
functor_spec(Name/Arity, Name, Arity, Arity, _) :-
    atom(Name),
    integer(Arity), !.
functor_spec(Name, Name, 0, 255, _) :- % 255 is max. arity
    atom(Name).

% ---------------------------------------------------------------------------
%! ## Breakpoints

:- export(breakpoint/5).
:- pred breakpoint(Pred, Src, Ln0, Ln1, Number) # "Breakpoint storage.".
:- data breakpoint/5.

:- export(breakpt/6).
:- pred breakpt(Pred, Src, Ln0, Ln1, Number, RealLine)
   : atm * sourcename * int * int * int * int
   # "Set a @index{breakpoint} in file @var{Src} between lines
   @var{Ln0} and @var{Ln1} at the literal corresponding to the
   @var{Number}'th occurrence of (predicate) name @var{Pred}.  The pair
   @var{Ln0}-@var{Ln1} uniquely identifies a program clause and must
   correspond to the start and end line numbers for the clause. The
   rest of the arguments provide enough information to be able to
   locate the exact literal that the @var{RealLine} line refers
   to. This is normally not issued by users but rather by the
   @apl{emacs} mode, which automatically computes the different
   argument after selecting a point in the source file.".

breakpt(Pred, Src, Ln0, Ln1, Number, RealLine) :-
    current_fact(breakpoint(Pred, Src, Ln0, Ln1, Number)), !,
    format(user,
        '{There is already a breakpoint on literal ~a in line ~d}~n',
        [Pred, RealLine]).

breakpt(Pred, Src, Ln0, Ln1, Number, RealLine) :-
    get_debugger_state(State),
    ( arg(1, State, off) -> debug ; true ),
    assertz_fact(breakpoint(Pred, Src, Ln0, Ln1, Number)),
    format(user, '{Breakpoint placed on literal ~a in line ~d}~n',
           [Pred, RealLine]).

:- export(nobreakpt/6).
:- pred nobreakpt(Pred, Src, Ln0, Ln1, Number, RealLine)
   : atm * sourcename * int * int * int * int
   # "Remove a breakpoint in file @var{Src} between lines @var{Ln0}
   and @var{Ln1} at the @var{Number}'th occurence of (predicate) name
   @var{Pred} (see @pred{breakpt/6}). Also normally used from de
   @apl{emacs} mode.".

nobreakpt(Pred, Src, Ln0, Ln1, Number, RealLine) :-
    retract_fact(breakpoint(Pred, Src, Ln0, Ln1, Number)), !,
    format(user, '{Breakpoint removed from literal ~a in line ~d}~n',
           [Pred, RealLine]).
nobreakpt(Pred, _, _, _, _, RealLine) :-
    format(user, '{No breakpoint on literal ~a in line ~d}~n',
           [Pred, RealLine]).

:- export(nobreakall/0).
:- pred nobreakall/0 # "Remove all breakpoints.".

nobreakall :-
    retractall_fact(breakpoint(_, _, _, _, _)),
    format(user, '{All breakpoints removed}~n', []).

:- export(list_breakpt/0).
:- pred list_breakpt/0 # "Prints out the location of all
    breakpoints. The location of the breakpoints is shown usually by
    referring to the source file, the lines between which the predicate
    can be found, the predicate name and the number of occurrences of the
    predicate name of the literal.".

list_breakpt:-
    current_fact(breakpoint(Pred, Src, Ln0, Ln1, Number)),
    format(user, 'Breakpoint in file ~a ~d-~d on literal ~a-~d~n',
           [Src, Ln0, Ln1, Pred, Number]),
    fail.
list_breakpt.

break_info(Pred, Src, _Ln0, Ln1, Number, ' B ') :-
    current_fact(breakpoint(Pred, Src, _, Ln1, Number)),
    !.
break_info(_Pred, _Src, _Ln0, _Ln1, _Number, '   ').

% ---------------------------------------------------------------------------
%! ## Show debugging state

:- export(debugging/0).
:- pred debugging/0 # "Display debugger state.".

debugging :-
    get_debugger_state(State),
    arg(1, State, G),
    what_is_on(G),
    what_is_debugged,
    what_is_leashed,
    what_maxdepth,
    all_spypoints,
    top_nl.

what_is_on(Mode) :-
    mode_message(Mode, Msg),
    top_display(Msg),
    top_nl.

mode_message(debug, '{The debugger will first leap -- showing spypoints and breakpoints (debug)}').
mode_message(trace, '{The debugger will first creep -- showing everything (trace)}').
mode_message(off, '{The debugger is switched off}').

what_is_debugged :-
    current_debugged(Ms),
    ( Ms = [] ->
        format(user, '{No module is selected for debugging}~n', [])
    ; format(user, '{Modules selected for debugging: ~w}~n', [Ms])
    ).

what_is_leashed :-
    is_leashed([call, exit, redo, fail], L),
    show_leash_info(L).

is_leashed([], []).
is_leashed([X|Xs], [X|Ys]) :- current_fact(leashed(X)), !, is_leashed(Xs, Ys).
is_leashed([_|Xs], Ys) :- is_leashed(Xs, Ys).

show_leash_info([]) :- !,
    format(user, '{No leashing}~n', []).
show_leash_info(Ps) :-
    format(user, '{Using leashing stopping at ~w ports}~n', [Ps]).

what_maxdepth :-
    current_fact(debugdepth(M)),
    format(user, '{Interpreter maxdepth is ~w}~n', [M]).

all_spypoints :-
    spypoint(_), !,
    top_display('Spypoints:'), list_spypoints.
all_spypoints :-
    top_display('{There are no spypoints}'), top_nl.

list_spypoints :-
    spypoint(X),
    functor(X, N, A),
    top_nl, tab(user, 4), write(user, N/A),
    fail.
list_spypoints :-
    top_nl.

% ---------------------------------------------------------------------------
%! # Internal entry from meta-interpreters

:- export(no_debug_pred/1).
% Internal predicates that cannot be debugged
no_debug_pred('basiccontrol:$metachoice'(_)) :- !. % wrong results otherwise!
no_debug_pred('basiccontrol:$metacut'(_)) :- !. % bad cuts otherwise!
no_debug_pred(G) :-
    % TODO: kludge, use predicate prop bits...
    functor(G, F, _),
    no_debug_mc(Mc),
    atom_concat(Mc, _, F).

:- if(defined(optim_comp)).
no_debug_mc('interpreter:').
no_debug_mc('hiord_rt:').
no_debug_mc('debugger_support:'). % TODO: for $stop_trace
no_debug_mc('debugger:').
:- endif.
no_debug_mc('foreign_js_rt:'). % (ciaowasm)
no_debug_mc('rtchecks_rt:').
no_debug_mc('native_props_rtc:').

:- export(debug_trace2/7).
debug_trace2(X, Pred, Src, Ln0, Ln1, DDict, Number) :-
    get_debugger_state(State),
    debug_dict(X, DDict, Dict),
    retry_hook_(X, B, D, NA, OA, Port, State, Dict),
    %
    '$setarg'(4, State, B, on),
    '$setarg'(5, State, NA, on),
    ( Port = call,
      '$metachoice'(C0),
      call_hook(X, B, D, State, Pred, Src, Ln0, Ln1, Dict, Number),
      '$metachoice'(C1)
    ; '$stop_trace', % TODO: necessary? <- yes, why?
      fail_hook(X, B, D, State, Pred, Src, Ln0, Ln1, Dict, Number), !, fail
    ),
    ( exit_hook(X, B, D, State, Pred, Src, Ln0, Ln1, Dict, Number)
    ; '$stop_trace', % TODO: necessary?
      redo_hook(X, B, D, State, Pred, Src, Ln0, Ln1, Dict, Number), fail
    ),
    % Remove choicepoints in deterministic goals to speed up debugging -- EMM
    ( C0 == C1 -> ! ; true ),
    %
    '$setarg'(5, State, OA, on).

:- if(defined(optim_comp)).
:- else.
'$stop_trace'. % TODO: dummy; backport this simpler mechanism?
:- endif.

retry_hook_(X, B, D, [a(B,X,D,Dict)|A], A, Port, State, Dict) :-
    State = s(_,_,_,B0,A),
    a_length(A, D0),
    B is B0+1,
    D is D0+1,
    ( current_fact(debugdepth(M)), D=<M -> true
    ; adjust_debugger_state(State, trace),
      format(user_error, '{Warning: Interpreter maxdepth exceeded}~n', [])
    ),
    retry_hook(B, call, Port, '$$retry_hook').

retry_hook(_, P, P, _).
retry_hook(Invocation, P0, P, A) :- retry_hook(Invocation, P0, P, A).

a_length([], 0).
a_length([a(_,_,X,_)|_], X).

call_hook(X, B, _, State, _, _, _, _, _, _) :-
    arg(3, State, Level),
    B>Level, !,
    call_hook1(X).
call_hook(X, B, D, State, Pred, Src, Ln0, Ln1, Dict, Number) :-
    debug_port(X, B, D, call, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number),
    call_hook2(Msg, X).

call_hook2(answer(X), X).
call_hook2(no, X) :- call_hook1(X).

:- if(defined(optim_comp)).
call_hook1(X) :-
    functor(X, Name, Ar),
    ( '$predicate_property'(Name/Ar, _, _) ->
        body_trace_call(X)
    ; get_debugger_state(State),
      adjust_debugger_state(State, trace),
      format(user_error, '{Warning: The predicate ~q is undefined}~n',
                         [Name/Ar]),
      fail
    ).
body_trace_call(X) :-
    '$notrace_call'(X),
    '$stop_trace'.
:- else.
:- use_module(engine(internals), [term_to_meta/2]).
call_hook1(X) :-
    ( '$predicate_property'(X, _, _) ->
        term_to_meta(X, G),
        '$nodebug_call'(G)
    ;
        get_debugger_state(State),
        adjust_debugger_state(State, trace),
        functor(X, Name, Ar),
        format(user_error, '{Warning: The predicate ~q is undefined}~n',
            [Name/Ar]),
        fail
    ).
:- endif.

exit_hook(_, B, _, State, _, _, _, _, _, _) :-
    arg(3, State, Level), B>Level, !.
exit_hook(X, B, D, State, Pred, Src, Ln0, Ln1, Dict, Number) :-
    '$setarg'(3, State, 1000000, true),
    debug_port(X, B, D, exit, State, _, Pred, Src, Ln0, Ln1, Dict, Number).

redo_hook(_, B, _, State, _, _, _, _, _, _) :-
    arg(3, State, Level), B>Level, !.
redo_hook(X, B, D, State, Pred, Src, Ln0, Ln1, Dict, Number) :-
    debug_port(X, B, D, redo, State, _, Pred, Src, Ln0, Ln1, Dict, Number).

fail_hook(_, B, _, State, _, _, _, _, _, _) :-
    arg(3, State, Level), B>Level, !.
fail_hook(X, B, D, State, Pred, Src, Ln0, Ln1, Dict, Number) :-
    '$setarg'(3, State, 1000000, true),
    debug_port(X, B, D, fail, State, _, Pred, Src, Ln0, Ln1, Dict, Number).

debug_port(X, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number) :-
    ( '$spypoint'(X, on, on)
    ; % % Ln0 is free because there is no way to determine where the 
      % % clause starts, but the end of the clause can be determined exactly.
      % current_fact(breakpoint(Pred, Src, _Ln0, Ln1, Number))
      %
      % JFMC: The Ciao emacs mode needs a number here. Since
      %   this only affects the output message, it seems that
      %   there is no problem in using the breakpoint Ln0.
      current_fact(breakpoint(Pred, Src, Ln0, Ln1, Number))
    ),
    !,
    defaultopt(Op),
    prompt_command(Op, X, [], B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number).
debug_port(X, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number) :-
    arg(2, State, trace),
    current_fact(leashed(Port)), !,
    defaultopt(Op),
    prompt_command(Op, X, [], B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number).
debug_port(X, B, D, Port, State, no, Pred, Src, Ln0, Ln1, Dict, Number) :-
    arg(2, State, trace), !,
    defaultopt(Op),
    write_goal(Op, X, [], B, D, Port, Pred, Src, Ln0, Ln1, Dict, Number),
    top_nl.
debug_port(_, _, _, _, _, no, _, _, _, _, _, _).

prompt_command(T, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number) :-
    write_goal(T, X, Xs, B, D, Port, Pred, Src, Ln0, Ln1, Dict, Number),
    get_command(C),
    do_trace_command(C, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number).

do_trace_command(0'a, _, _, _, _, _, _, _, _, _, _, _, _, _) :- !,
    % a(bort)
    abort.
do_trace_command(0'c, _, _, _, _, _, State, no, _, _, _, _, _, _) :- !,
    % c(reep)
    '$setarg'(2, State, trace, true),
    '$debugger_mode'.
do_trace_command(0'\n, _, _, _, _, _, State, no, _, _, _, _, _, _) :- !,
    % CR (creep)
    '$setarg'(2, State, trace, true),
    '$debugger_mode'.
do_trace_command(0'd, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number) :- !,
    % d(isplay)
    set_defaultopt(0'd, false, false),
    prompt_command(0'd, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number).
do_trace_command([0'd, AV], X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number) :- proc_extraopts(AV, A, V), !,
    % d(isplay)
    set_defaultopt(0'd, A, V),
    prompt_command(0'd, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number).
do_trace_command(0'g, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number) :- !, % g(ancestors)
    arg(5, State, CA),
    show_ancestors(CA, -1),
    defaultopt(Op),
    prompt_command(Op, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number).
do_trace_command([0'g, Arg], X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number) :- !,
    % g(ancestors) arg
    arg(5, State, CA),
    show_ancestors(CA, Arg),
    defaultopt(Op),
    prompt_command(Op, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number).
do_trace_command(0'l, _, _, _, _, _, State, no, _, _, _, _, _, _) :- !,
    % l(eap)
    '$setarg'(2, State, debug, true),
    '$debugger_mode'.
do_trace_command(0'n, _, _, _, _, _, State, no, _, _, _, _, _, _) :- !,
    % n(odebug)
    % nodebug.
    '$setarg'(3, State, 0, true).
do_trace_command(0'p, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number) :- !,
    % p(rint)
    set_defaultopt(0'p, false, false),
    prompt_command(0'p, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number).
do_trace_command([0'p, AV], X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number) :- proc_extraopts(AV, A, V), !,
    % p(rint)
    set_defaultopt(0'p, A, V),
    prompt_command(0'p, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number).
do_trace_command(0'v, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number) :- !,
    % v(ariables)
    prompt_command(0'v, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number).
do_trace_command([0'v, Name], X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number) :- !,
    % v(ariables)
    prompt_command([0'v, Name], X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number).
do_trace_command(0'r, _, _, _, _, _, State, no, _, _, _, _, _, _) :- !,
    % r(etry)
    arg(5, State, [a(B,_,_,_)|_]),
    do_retry_fail(B, State, call).
do_trace_command([0'r, B], _, _, _, _, _, State, no, _, _, _, _, _, _) :- !,
    % r(etry) arg
    do_retry_fail(B, State, call).
do_trace_command(0'f, _, _, _, _, _, State, no, _, _, _, _, _, _) :- !,
    %f(ail)
    arg(5, State, [a(B,_,_,_)|_]),
    do_retry_fail(B, State, fail).
do_trace_command([0'f, B], _, _, _, _, _, State, no, _, _, _, _, _, _) :- !,
    % f(ail) arg
    do_retry_fail(B, State, fail).
do_trace_command(0's, _, _, B, _, Port, State, no, _, _, _, _, _, _) :- % s(kip)
    set_skip(Port, B, State), !.
do_trace_command(0'w, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number) :- !,
    % w(rite)
    set_defaultopt(0'w, false, false),
    prompt_command(0'w, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number).
do_trace_command([0'w, AV], X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number) :- proc_extraopts(AV, A, V), !,
    % w(rite)
    set_defaultopt(0'w, A, V),
    prompt_command(0'w, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number).
do_trace_command(0'+, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number) :- !,
    % +(spy this)
    lastof(Xs, _-X, _-Goal),
    spy1(Goal),
    defaultopt(Op),
    prompt_command(Op, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number).
do_trace_command(0'-, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number) :- !,
    % -(nospy this)
    lastof(Xs, _-X, _-Goal),
    nospy1(Goal),
    defaultopt(Op),
    prompt_command(Op, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number).
do_trace_command(0'=, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number) :- !,
    % =(debugging)
    reset_debugger(_),
    debugging,
    set_debugger(State),
    defaultopt(Op),
    prompt_command(Op, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number).
% do_trace_command(0'b, X, Xs, B, D, Port, State, Msg) :- !, % b(reak)
%     break,
%     prompt_command(0'p, X, Xs, B, D, Port, State, Msg).
do_trace_command(0'@, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number) :- !,
    %@ (command)
    do_once_command('| ?- ', Dict),
    defaultopt(Op),
    prompt_command(Op, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number).
do_trace_command(0'u, _, _, _, _, call, _, answer(X1), _, _, _, _, _, _) :- !,
    %u (unify)
    '$prompt'(Old, '|: '),
    read(user, X1),
    '$prompt'(_, Old).
do_trace_command(0'<, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number) :- !,
    %< (reset printdepth)
    reset_printdepth,
    defaultopt(Op),
    prompt_command(Op, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number).
do_trace_command([0'<, I], X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number) :- !,
    %< arg (set printdepth)
    set_printdepth(I),
    defaultopt(Op),
    prompt_command(Op, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number).
do_trace_command(0'^, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number) :- !,
    %^ (reset subterm)
    lastof(Xs, _-X, _-Goal),
    defaultopt(Op),
    prompt_command(Op, Goal, [], B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number).
do_trace_command([0'^, 0], _, [_-X|Xs], B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number) :- !,
    %^ 0 (up subterm)
    defaultopt(Op),
    prompt_command(Op, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number).
do_trace_command([0'^, I], X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number) :- arg(I, X, Ith), !,
    %^ arg (set subterm)
    defaultopt(Op),
    prompt_command(Op, Ith, [I-X|Xs], B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number).
do_trace_command(0'?, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number) :- !,
    % ?(help)
    debugging_options,
    defaultopt(Op),
    prompt_command(Op, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number).
do_trace_command(0'h, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number) :- !,
    % h(elp)
    debugging_options,
    defaultopt(Op),
    prompt_command(Op, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number).
do_trace_command(_, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number) :-
    % all others
    format(user, '{Option not applicable at this port}~n', []),
    defaultopt(Op),
    prompt_command(Op, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number).

set_skip(call, To, State) :- '$setarg'(3, State, To, true).
set_skip(redo, To, State) :- '$setarg'(3, State, To, true).
set_skip(_, _To, State) :-
    format(user, '{Skip not applicable at this port, creeping ...}~n', []),
    do_trace_command(0'c, _, _, _, _, _, State, no, _, _, _, _, _, _).

spy_info([], Goal, ' + ') --> {'$spypoint'(Goal, on, on)}, !.
spy_info([], _, '   ') --> [].
spy_info([I-X|Xs], _, Goal) -->
    spy_info(Xs, X, Goal),
    [^, I].

lastof([], X, X).
lastof([X0|Xs], _, X) :- lastof(Xs, X0, X).

do_retry_fail(B, State, Port) :-
    '$retry_cut'(B, Port),
    '$setarg'(2, State, trace, true), % retry implies creep!
    fail.

% Command options
debugging_options :-
    top_display('Debugging options:'), top_nl,
    top_display('   <cr>    creep            c      creep'), top_nl,
    top_display('    l      leap             s      skip'), top_nl,
    top_display('    r      retry            r <i>  retry i'), top_nl,
    top_display('    f      fail             f <i>  fail i'), top_nl,
    top_display('    d <av> display av       p <av> print av'), top_nl,
    top_display('    w <av> write av         a      abort'), top_nl,
    top_display('    v      variables        v <N>  variable N'), top_nl,
    top_display('    g      ancestors        g <n>  ancestors n'), top_nl,
    top_display('    n      nodebug          =      debugging'), top_nl,
    top_display('    +      spy this         -      nospy this'), top_nl,
    top_display('    @      command          u      unify'), top_nl,
    top_display('    <      reset printdepth < <n>  set printdepth'), top_nl,
    top_display('    ^      reset subterm    ^ <n>  set subterm'), top_nl,
    top_display('    ?      help             h      help'), top_nl,
    top_nl,
    top_display('Note: In d, p and w options, you can add'), top_nl,
    top_display('  <a> to show attributes and <v> to show variables.'),
    top_nl,
    top_nl.

% ---------------------------------------------------------------------------
%! ## Input for prompt_command

get_command(Command) :-
    top_display(' ? '),
    top_flush,
    top_get_line(Line),
    parse_cmd(Line, Command).

parse_cmd(end_of_file, Command) :- !, Command = 0'\n.
parse_cmd([], Command) :- !, Command = 0'\n.
parse_cmd([C|Cs], Command) :-
    parse_cmd_(Cs, C, Command).

parse_cmd_([], C, C) :- !.
parse_cmd_([0' |Cs], C1, C) :- !, % skip blank
    parse_cmd_(Cs, C1, C).
parse_cmd_(Cs, C1, [C1, Arg]) :-
    Cs = [C2|_], C2 >= 0'0, C2 =< 0'9, !, % digit
    parse_digits(Cs, 0, Arg).
parse_cmd_(Cs, C1, [C1, Cs]).

parse_digits([], I, I) :- !.
parse_digits([Ch|Cs], SoFar, I) :-
    Ch >= 0'0, Ch =< 0'9, !,
    Next is SoFar*10 + Ch - 0'0,
    parse_digits(Cs, Next, I).
parse_digits([_|Cs], I, J) :- % ignore nondigit
    parse_digits(Cs, I, J).

% ---------------------------------------------------------------------------
%! ## Show ancestors command (g)

show_ancestors([_], _) :- !,
    top_nl, top_display('No ancestors.'), top_nl.
show_ancestors([_|CA], N) :-
    top_nl, top_display('Ancestors:'), top_nl,
    list_ancestors(CA, N).

list_ancestors([], _) :- !.
list_ancestors(_, 0) :- !.
list_ancestors([a(B,X,D,Dict)|As], N0) :-
    N is N0-1,
    list_ancestors(As, N),
    defaultopt(Op),
    write_goal(Op, X, [], B, D, void, _Pred, _Src, nil, nil, Dict, nil),
    top_nl.

% ---------------------------------------------------------------------------
%! ## Do once command (@)

% Note: query called in a toplevel_scope context

:- export(do_once_command/2).
do_once_command(Prompt, d(UDict, CDict, _ADict)) :-
    '$prompt'(OldPrompt, Prompt),
    reset_debugger(State),
    read_term(user, Command, [variable_names(Dict0)]),
    append(UDict, CDict, Dict),
    % Variable Binding between Command and Program:
    union(Dict, Dict0, _),
    '$prompt'(_, '|: '),
    catch((debug_call(Command) -> Y=yes ; Y=no), E, Y=ex),
    '$prompt'(_, OldPrompt),
    ( Y=yes ->
        current_fact(printopts(Op, D, A, _)),
        get_write_options(A, [], D, WriteOpts),
        display_nvs(Dict0, Op, WriteOpts)
    ; Y=no ->
        format(user_error, '{Warning: goal failed}~n', [])
    ; %Y=ex ->
      format(user_error, '{Warning: exception thrown ~w}~n', [E])
    ),
    set_debugger(State).

% (Make sure that toplevel_scope:'$shell_call' exists)
:- use_module(library(toplevel), []).
:- if(defined(optim_comp)).
:- import(user, ['$shell_call'/1]).
:- else.
:- use_module(user, ['$shell_call'/1]). % TODO: use import too?
:- endif.

debug_call(Goal) :- '$shell_call'(Goal).

% ---------------------------------------------------------------------------
%! # Print options

:- pred printopts(DefaultOption, Depth, Attrs, Vars).
% Depth: printing depth
% Attrs: print attributed variables
% Vars: show source variable names
:- data printopts/4.
printopts(0'p, 10, true, false). % (initial options)

% Print depth
:- export(printdepth/1).
printdepth(Depth) :- current_fact(printopts(_, Depth, _, _)).

defaultopt(O) :- current_fact(printopts(O, _, _, _)).

reset_printdepth :-
    set_printdepth(10).

set_printdepth(D) :-
    retract_fact(printopts(O, _, A, V)),
    assertz_fact(printopts(O, D, A, V)).

% Default opt
:- export(set_defaultopt/1).
set_defaultopt(O) :-
    retract_fact(printopts(_, D, A, V)),
    assertz_fact(printopts(O, D, A, V)).

:- export(set_defaultopt/3).
set_defaultopt(O, A, V) :-
    retract_fact(printopts(_, D, _, _)),
    assertz_fact(printopts(O, D, A, V)).

proc_extraopts(Opts, Attrs, Vars) :-
    proc_extraopts_(Opts, Attrs, Vars),
    % set defaults
    ( var(Attrs) -> Attrs = false ; true ),
    ( var(Vars) -> Vars = false ; true ).

proc_extraopts_([], _, _).
proc_extraopts_([C|Cs], Attrs, Vars) :-
    proc_extraopts__(C, Attrs, Vars),
    proc_extraopts_(Cs, Attrs, Vars).

proc_extraopts__(0'a, true, _).
proc_extraopts__(0'v, _, true). % TODO: actual goals, src vars later, is it better?
proc_extraopts__(0'o, _, orig). % TODO: src vars in goals, deprecate?

% ---------------------------------------------------------------------------
%! # Print goal and bindings during debugging

write_goal(T, X, Xs, B, D, Port, Pred, Src, Ln0, Ln1, Dict, Number) :-
    reset_debugger(State),
    port_info(Port, Pport),
    current_output(CO),
    set_output(user),
    do_write_goal(T, X, Xs, B, D, Pport, Pred, Src, Ln0, Ln1, Dict, Number),
    set_output(CO),
    set_debugger(State).

port_info(block,   '  Block: ').
port_info(unblock, '  Unblock: ').
port_info(call,    '  Call: ').
port_info(exit,    '  Exit: ').
port_info(redo,    '  Redo: ').
port_info(fail,    '  Fail: ').
port_info(void,    '  ').

do_write_goal(0'v, X, _, _, _, _, _, _, _, _, Dict, _) :- !,
    write_goal_v(X, Dict).
do_write_goal([0'v, SName], _, _, _, _, _, _, _, _, _, Dict, _) :- !,
    write_goal_v_name(SName, Dict).
do_write_goal(T, X, Xs, B, D, Pport, Pred, Src, Ln0, Ln1, Dict, Number) :-
    print_srcdbg_info(Pport, Pred, Src, Ln0, Ln1, Number),
    spy_info(Xs, X, Mark0, S, []),
    ( Mark0 == '   ' -> break_info(Pred, Src, Ln0, Ln1, Number, Mark)
    ; Mark=Mark0
    ),
    display_list([Mark, B, '  ', D, Pport|S]),
    get_attributed_vars(X, AtVars),
    write_goal2(T, X, Dict, AtVars).

write_goal_v(X0, d(UDict0, CDict0, _)) :-
    current_fact(printopts(Op, D, A, _)),
    append(UDict0, CDict0, Dict0),
    get_attributed_vars(X0-Dict0, UnsortedAtVars),
    sort(UnsortedAtVars, AtVars0),
    apply_dict(
        t(AtVars0, UDict0, CDict0),
        Dict0,
        t(AtVars, UDict, CDict)),
    get_write_options(A, AtVars, D, WriteOpts),
    show_variable_values(UDict0, UDict, user,     Op, WriteOpts),
    show_variable_values(CDict0, CDict, compiler, Op, WriteOpts),
    (A == true -> print_attributes(AtVars, Op, WriteOpts) ; true).

write_goal_v_name(SName, d(UDict, CDict, _)) :-
    current_fact(printopts(Op, D, A, _)),
    append(UDict, CDict, Dict),
    atom_codes(Name, SName),
    ( member(Name=Value0, Dict) ->
        get_attributed_vars(Value0, AtVars0),
        apply_dict(Value0-AtVars0, Dict, Value-AtVars),
        get_write_options(A, AtVars, D, WriteOpts),
        display_var(Name, Value0, Value, Op, WriteOpts),
        ( A == true -> print_attributes(AtVars, Op, WriteOpts) ; true )
    ; format(user, '{~w not defined here}~n', [Name])
    ).

display_var(Name, Value0, Value, Op, WriteOpts) :-
    display(Name),
    ( var(Value0) ->
        display_list([' = ', Value0])
    ; true
    ),
    ( '$VAR'(Name) == Value -> true
    ; display(' = '),
        write_op(Op, Value, WriteOpts)
    ).

write_goal2(Op, Goal0, d(UDict0, CDict0, ADict0), AtVars0) :-
    current_fact(printopts(_, D, A, V)),
    sort(AtVars0, AtVars1),
    ( V == true ->
        Goal = Goal0,
        AtVars = AtVars1
    ; V == orig ->
        apply_dict_with_cycles(t(ADict0,Goal0,AtVars1),ADict0,t(ADict,Goal,AtVars)),              
        foldl(sel_instantiated, ADict, AInst, [])
    ; Goal = Goal0,
      AtVars = AtVars1
    ),
    get_write_options(A, AtVars, D, WriteOpts),
    write_op(Op, Goal, WriteOpts),
    ( V == true ->
        append(UDict0, CDict0, UCDict0),
        % (only uninstantiated variables)
%        filter(uninstantiated, UCDict0, DictUninst),
%        maplist(display_ov(Op, WriteOpts), DictUninst)
        % (all variables)
        maplist(display_ov(Op, WriteOpts), UCDict0)
    ; V == orig ->
        maplist(display_nv(Op, WriteOpts), AInst)
    ; true
    ),
    ( A == true -> print_attributes(AtVars, Op, WriteOpts) ; true ).

sel_instantiated(NameValue) --> {instantiated(NameValue)}, !, [NameValue].
sel_instantiated(_) --> [].

get_write_options(true, [_|_], D, [max_depth(D), numbervars(true)]) :- !.
get_write_options(_, _, D, [max_depth(D), numbervars(true), portrayed(true)]).

write_op(0'p, Goal, WriteOpts) :- write_term(Goal, WriteOpts).
write_op(0'd, Goal, _) :- display(Goal).
write_op(0'w, Goal, _) :- writeq(Goal).

%:- export(instantiated/1).
instantiated(Name = Value) :- '$VAR'(Name) \== Value.

uninstantiated(_ = Value) :- var(Value).

show_variable_values(Dict0, Dict, VarKind, Op, WO) :-
    filter(instantiated, Dict, DictInst),
    filter(uninstantiated, Dict0, DictUninst),
    ( DictInst == [] -> true
    ; format(user,
            '         {Instantiated ~w-defined variables in scope:~n',
            [VarKind]),
        display_nvs(DictInst, Op, WO),
        format(user, '         }~n', [])
    ),
    ( DictUninst == [] -> true
    ;
        ( DictInst == [] -> All = '(all) ' ; All = '' ),
        format(user,
            '         {Uninstantiated ~w~w-defined variables in scope:~n',
            [All, VarKind]),
        display_nvs(DictUninst, Op, WO),
        format(user, '         }~n', [])
    ).

%:- export(display_nvs/3).
display_nvs([],               _,  _).
display_nvs([NameValue|Dict], Op, WO) :-
    display_nv0(NameValue, Op, WO),
    maplist(display_nv(Op, WO), Dict),
    nl.

display_nv0(Name=Value, Op, WO) :-
    display_list(['\t   ', Name, ' = ']),
    write_op(Op, Value, WO).

display_nv(Op, WO, NameValue) :-
    display(','), nl,
    display_nv0(NameValue, Op, WO).

display_ov0(Name=Value, Op, WO) :-
    display_list(['\t   ', Name, ': ']),
    write_op(Op, Value, WO).

display_ov(Op, WO, NameValue) :-
    nl,
    display_ov0(NameValue, Op, WO).

print_attributes(As, Op, WriteOpts) :-
    maplist(print_attribute(Op, WriteOpts), As).

print_attribute(Op, WriteOpts, A) :-
    nl,
    tab(10), % 10 blanks
    display('['),
    write_op(Op, A, WriteOpts),
    display(']').

:- multifile print_srcdbg_info_hook/6.

print_srcdbg_info(_, _, _, nil, nil, nil) :- !.
print_srcdbg_info(Pport, Pred, Src, Ln0, Ln1, Number) :-
    ( using_windows -> % running in a Windows non-cygwin shell
        % Emacs understand slashes instead of backslashes, even on
        % Windows, and this saves problems with escaping
        % backslashes
        cyg2win_a(Src, ActualSrc, noswap)
    ; Src = ActualSrc
    ),
    ( print_srcdbg_info_hook(Pport, Pred, Src, Ln0, Ln1, Number) ->
        true
    ; display_list([
        '         In ', ActualSrc, ' (', Ln0, -, Ln1, ') ',
        Pred, -, Number, '\n'])
    ).

% ---------------------------------------------------------------------------
%! ## Auxiliary for printing bindings (dicts, cycles, and attributed variables)
% TODO: move/merge with prettysols.pl?
% TODO: document

:- use_module(library(cyclic_terms), [uncycle_term/2]). % TODO: slow, see attrdump.pl
:- use_module(library(varnames/apply_dict)).
:- use_module(library(varnames/complete_dict), [set_undefined_names/3]).
:- use_module(engine(attributes), [get_attribute/2]).

debug_dict(X, d(UDict, CDict), Dict) :-
    append(UDict, CDict, Dict0),
    set_undefined_names(Dict0, 1, _),
    select_applicable_with_cycles(X, Dict0, ADict),
    Dict = d(UDict, CDict, ADict).

select_applicable_with_cycles(X,Dict,ADict) :-
    uncycle_term(X,(X1,_)),
    select_applicable(X1, Dict, ADict).

apply_dict_with_cycles(t(ADict0,Goal0,AtVars0),ADict0,t(ADict,Goal,AtVars)) :-
    uncycle_term(Goal0-ADict0,(Goal1-ADict2,Cycles)),
    close_list(Cycles),
    as_dict(Cycles,DictCycles),
    append(ADict2,DictCycles,ADict1),
    set_undefined_names(ADict1,1,_),
    apply_dict(t(ADict1,Goal1,AtVars0), ADict1, t(ADict, Goal, AtVars)).

close_list([]) :- !.
close_list([_|Xs]) :-
    close_list(Xs).

as_dict([],[]).
as_dict([X-Y|L1],[Y=X|L2]) :-
    as_dict(L1,L2).

%:- export(get_attributed_vars/2).
get_attributed_vars(Term, AtVars) :-
    ( cyclic_term(Term) ->
        get_attributed_vars_cy(Term, [], _, [], AtVars)
    ; get_attributed_vars_nc(Term, AtVars, [])
    ).

get_attributed_vars_cy(X, Seen, Seen, At, NewAt) :-
    var(X), !,
    ( get_attribute(X, AtX) ->
        NewAt = [attach_attribute(X, AtX)|At]
    ;
        NewAt = At
    ).
get_attributed_vars_cy(X, Seen, Seen, At, At) :-
    atomic(X), !.
get_attributed_vars_cy(X, Seen, Seen, At, At) :-
    already_seen(Seen, X), !.
get_attributed_vars_cy(X, Seen, NewSeen, At, NewAt) :-
    functor(X, _, Ar),
    get_attributed_vars_cy_args(Ar, X, [X|Seen], NewSeen, At, NewAt).

get_attributed_vars_cy_args(0, _, Seen,  Seen,  At,  At) :- !.
get_attributed_vars_cy_args(N, X, Seen0, Seen2, At0, At2) :-
    N > 0,
    arg(N, X, A),
    get_attributed_vars_cy(A, Seen0, Seen1, At0, At1),
    N1 is N - 1,
    get_attributed_vars_cy_args(N1, X, Seen1, Seen2, At1, At2).

already_seen([T|_Ts], Term) :-
    T == Term,
    !.
already_seen([_T|Ts], Term) :- already_seen(Ts, Term).

get_attributed_vars_nc(X, At, At) :- atomic(X), !.
get_attributed_vars_nc(X, [attach_attribute(X, AtX)|At], At) :-
    var(X),
    get_attribute(X, AtX), !.
get_attributed_vars_nc(X, At,  At) :- var(X), !. % No attributes
get_attributed_vars_nc([X|Xs], At0, At2) :- !,
    get_attributed_vars_nc(X,  At0, At1),
    get_attributed_vars_nc(Xs, At1, At2).
get_attributed_vars_nc(X, At0, At1) :-
    functor(X, _, Ar),
    get_attributed_vars_args(Ar, X, At0, At1).

get_attributed_vars_args(0, _, At, At) :- !.
get_attributed_vars_args(N, X, At0, At2) :-
    N > 0,
    arg(N, X, A),
    get_attributed_vars_nc(A, At0, At1),
    N1 is N - 1,
    get_attributed_vars_args(N1, X, At1, At2).


