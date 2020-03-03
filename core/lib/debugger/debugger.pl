:- module(debugger, [
    call_in_module/2,
    debug_trace/1,
    do_interrupt_command/1,
    switch_off_debugger/0,
    adjust_debugger/0
], [dcg, assertions, hiord]).

:- doc(title, "Predicates controlling the interactive debugger").
:- doc(author, "A. Ciepielewski").
:- doc(author, "Mats Carlsson").
:- doc(author, "T. Chikayama").
:- doc(author, "K. Shen").
:- doc(author, "Daniel Cabeza").
:- doc(author, "Manuel C. Rodriguez").
:- doc(author, "Edison Mera").

:- doc(module, "This library implements predicates which are
   normally used in the interactive top-level shell to debug
   programs. A subset of them are available in the embeddable debugger.").

:- use_module(library(datafacts/datafacts_rt)). % TODO: this one or datafacts package?
:- use_module(engine(runtime_control), [current_prolog_flag/2]).
:- use_module(engine(debugger_support)).
:- use_module(library(debugger/debugger_lib), [
    adjust_debugger_state/2,
    in_debug_module/1,
    debug_trace2/8,
    do_once_command/2,
    get_debugger_state/1]).
:- reexport(library(debugger/debugger_lib), [
    breakpt/6,
    current_debugged/1,
    debug/0,
    debug_module/1,
    debug_module_source/1,
    debugging/0,
    debugrtc/0,
    get_debugger_state/1,
    leash/1,
    list_breakpt/0,
    maxdepth/1,
    nobreakall/0,
    nobreakpt/6,
    nodebug/0,
    nodebug_module/1,
    nodebugrtc/0,
    nospy/1,
    nospyall/0,
    notrace/0,
    reset_debugger/1,
    spy/1,
    trace/0,
    tracertc/0]).
:- use_module(engine(internals), [term_to_meta/2, '$setarg'/4, module_concat/3]).
:- use_module(engine(hiord_rt), ['$nodebug_call'/1, '$meta_call'/1]).
:- use_module(engine(attributes)).
:- use_module(library(format)).

:- use_module(library(toplevel/toplevel_io)).

:- doc(hide, adjust_debugger/0).
:- doc(hide, switch_off_debugger/0).
:- doc(hide, current_debugged/1).
:- doc(hide, reset_debugger/1).
:- doc(hide, set_debugger/1).
:- doc(hide, debug_trace/1).
:- doc(hide, do_interrupt_command/1).

%------------------ Bug Comments ------------------------------

:- doc(bug, "Add an option to the emacs menu to automatically select
    all modules in a project.").
:- doc(bug, "Consider the possibility to show debugging messages 
    directly in the source code emacs buffer.").

%------------------Prolog debugger by AC------------------------------
% Minor hacks by MC.
% Some hacks by Takashi Chikayama (17 Dec 87)
%   - Making tracer to use "print" rather than "write"
%   - Temporarily switching debugging flag off while writing trace
%     message and within "break" level.
% Some hacks by Kish Shen (May 88)
%   - Subterm navigation
%   - Handle unbound arg in spy/1 and nospy/1
%   - Trap arith errors in debug mode
%------------- Built-in predicates for debugging------------------------

% define_flag(debug, [on,debug,trace,off], off).

% Debugger_state = s(DebugFlag, OptDebugFlag, int, int, AncestorList)
% DebugFlag = trace|debug|off
% OptDebugFlag = trace|debug|off

%%:- initialization(initialize_debugger_state).
%%:- on_abort(initialize_debugger_state).

% This has to be done before any choicepoint
% initialize_debugger_state used in internals.pl --EMM
:- entry initialize_debugger_state/0.
initialize_debugger_state :-
    '$debugger_state'(_, s(off, off, 1000000, 0, [])),
    '$debugger_mode'.

switch_off_debugger :-
    '$debugger_state'(State, State),
    '$setarg'(2, State, off, true),
    '$debugger_mode'.

%------------------------ meta-interpreters ------------------------------

% called from interpreter.pl

debug_trace(X) :-
    extract_info(X, Goal, Pred, Src, Ln0, Ln1, Dict, Number),
    ( debuggable(Goal) ->
        get_debugger_state(State),
        debug_trace2(Goal, State, Pred, Src, Ln0, Ln1, Dict, Number)
    ; term_to_meta(X, G),
      '$nodebug_call'(G)
    ).

debuggable(Goal) :-
    in_debug_module(Goal).
debuggable(_) :-
    get_debugger_state(S),
    arg(5, S, [a(_, Ancestor, _, _)|_]),
    in_debug_module(Ancestor).

%-------------------------facilities-------------------------------------

% extract_info('debugger:srcdbg_spy'(Goal,Pred,Src,Ln0,Ln1,Dict,Number),
extract_info('debugger_support:srcdbg_spy'(Goal, Pred, Src, Ln0, Ln1, Dict, Number),
             NewGoal, Pred, Src, Ln0, Ln1, Dict, Number) :-
    !,
    term_to_meta(NewGoal, Goal).
extract_info(Goal, Goal, nil, nil, nil, nil, d([], []), nil).

adjust_debugger :-
    get_debugger_state(State),
    arg(1, State, G),
    adjust_debugger_state(State, G).

do_interrupt_command(0'@) :- !, % @(command)
    top_skipeol, do_once_command('| ?- ', d([], [], [])),
    do_interrupt_command(0'\n).
do_interrupt_command(0'a) :- !, % a(bort)
    top_skipeol, abort.
% do_interrupt_command(0'b) :- !, % b(reak)
%       top_skipeol, break.
do_interrupt_command(0'c) :- !, % c(ontinue)
    top_skipeol.
do_interrupt_command(0'd) :- !, % d(ebug)
    top_skipeol, debug.
do_interrupt_command(0'e) :- !, % e(xit)
    top_skipeol, halt.
do_interrupt_command(0't) :- !, % t(race)
    top_skipeol, trace.
do_interrupt_command(0'\n) :- !, % cr
    format(user, '~nCiao interruption (h for help)? ', []),
    top_flush,
    top_get(C),
    do_interrupt_command(C).
do_interrupt_command(_) :- % h(elp) or other
    top_skipeol,
    interrupt_options,
    do_interrupt_command(0'\n).

interrupt_options :-
    top_nl,
    top_display('Ciao interrupt options:'), top_nl,
    top_display('    a        abort           - cause abort'), top_nl,
%       top_display('    b        break           - cause break'), top_nl,
    top_display('    c        continue        - do nothing'), top_nl,
    top_display('    d        debug           - start debugging'), top_nl,
    top_display('    t        trace           - start tracing'), top_nl,
    top_display('    e        exit            - cause exit'), top_nl,
    top_display('    @        command         - execute a command'), top_nl,
    top_display('    h        help            - get this list'), top_nl.

% :- meta_predicate call_in_module(?, fact).

:- pred call_in_module(Module, Predicate) : atm * callable
# "Calls predicate @var{Predicate} belonging to module
    @var{Module}, even if that module does not export the
    predicate. This only works for modules which are in debug 
    (interpreted) mode (i.e., they are not optimized).".

call_in_module(Module, Goal) :-
    module_concat(Module, Goal, MGoal),
    '$meta_call'(MGoal).
