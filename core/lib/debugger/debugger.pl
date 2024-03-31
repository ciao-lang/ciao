:- module(debugger, [], [assertions]).

:- doc(title, "Predicates controlling the interactive debugger").
:- doc(author, "A. Ciepielewski"). % (original)
:- doc(author, "Mats Carlsson"). % Minor hacks by MC.
:- doc(author, "T. Chikayama").
% Some hacks by Takashi Chikayama (17 Dec 87)
%   - Making tracer to use "print" rather than "write"
%   - Temporarily switching debugging flag off while writing trace
%     message and within "break" level.
:- doc(author, "K. Shen").
% Some hacks by Kish Shen (May 88)
%   - Subterm navigation
%   - Handle unbound arg in spy/1 and nospy/1
%   - Trap arith errors in debug mode
% --
:- doc(author, "Daniel Cabeza").
:- doc(author, "Manuel C. Rodriguez").
:- doc(author, "Edison Mera").
:- doc(author, "Jose F. Morales").
:- doc(author, "Manuel Hermenegildo").
% Minor in documentation.  

:- doc(module, "This library implements predicates which are normally
   used in the interactive top-level shell to debug programs. A subset
   of them are available in the embeddable debugger.").

:- doc(bug, "Add an option to the Emacs menu to automatically
   (de)select all modules in a project.").

:- reexport(library(debugger/debugger_lib), [
    debug_module/1,
    debug_module_source/1,
    nodebug_module/1,
    debug/0,
    nodebug/0,
    trace/0,
    notrace/0,
    maxdepth/1,
    spy/1,
    nospy/1,
    nospyall/0,
    debugging/0,
    leash/1,
    breakpt/6,
    list_breakpt/0,
    nobreakpt/6,
    nobreakall/0,
    current_debugged/1,
    get_debugger_state/1
    ]).
:- reexport(library(debugger/debugger_lib), [
    debugrtc/0,
    nodebugrtc/0,
    tracertc/0]).

:- use_module(engine(debugger_support)).
:- use_module(library(debugger/debugger_lib), [
    adjust_debugger_state/2,
    in_debug_module/1, 
    no_debug_pred/1,
    debug_trace2/7,
    do_once_command/2,
    get_debugger_state/1]).

% ---------------------------------------------------------------------------
:- doc(doinclude,    debug_module/1).
:- doc(doinclude,    debug_module_source/1).
:- doc(doinclude,    nodebug_module/1).
:- doc(doinclude,    debug/0).
:- doc(doinclude,    nodebug/0).
:- doc(doinclude,    trace/0).
:- doc(doinclude,    notrace/0).
:- doc(doinclude,    maxdepth/1).
:- doc(doinclude,    spy/1).
:- doc(doinclude,    nospy/1).
:- doc(doinclude,    nospyall/0).
:- doc(doinclude,    debugging/0).
:- doc(doinclude,    leash/1).
:- doc(doinclude,    breakpt/6).
:- doc(doinclude,    list_breakpt/0).
:- doc(doinclude,    nobreakpt/6).
:- doc(doinclude,    nobreakall/0).
:- doc(doinclude,    current_debugged/1).
:- doc(doinclude,    get_debugger_state/1).

% ---------------------------------------------------------------------------


% ---------------------------------------------------------------------------
%! # Enable/disable debugger context (called from toplevel.pl)

:- use_module(engine(internals), ['$setarg'/4]).

:- if(defined(optim_comp)).
:- else.
% This has to be done before any choicepoint
% initialize_debugger_state used in internals.pl --EMM
:- entry initialize_debugger_state/0.
initialize_debugger_state :-
    % TODO: call reset_debugger(_) instead?
    '$debugger_state'(_, s(off, off, 1000000, 0, [])),
    '$debugger_mode'.
:- endif.

:- doc(hide, adjust_debugger/0).
:- export(adjust_debugger/0).
adjust_debugger :-
    get_debugger_state(State),
    arg(1, State, G),
    adjust_debugger_state(State, G).

:- doc(hide, switch_off_debugger/0).
:- export(switch_off_debugger/0).
switch_off_debugger :-
    '$debugger_state'(State, State),
    '$setarg'(2, State, off, true),
    '$debugger_mode'.

% ---------------------------------------------------------------------------
%! # Debugger entry (called from interpreter.pl)

:- if(defined(optim_comp)).
:- else.
:- use_module(engine(internals), [term_to_meta/2]).
:- use_module(engine(hiord_rt), ['$nodebug_call'/1]).
:- endif.

:- doc(hide, debug_trace/1).
:- export(debug_trace/1).
:- if(defined(optim_comp)).
debug_trace(X) :-
    % note: CInt tracing is disabled at this point
    extract_info(X, Goal, Pred, Src, Ln0, Ln1, Dict, Number),
    ( debuggable(Goal) ->
        debug_trace2(Goal, Pred, Src, Ln0, Ln1, Dict, Number),
        '$start_trace'
    ; % enter the predicate and start trace of its body
      % note: this is equivalent to body_trace_call + start_trace, but
      %   enables tail call recursion
      '$notrace_call'(X)
    ).
:- else.
debug_trace(X) :-
    % note: CInt tracing is disabled at this point
    extract_info(X, Goal, Pred, Src, Ln0, Ln1, Dict, Number),
    ( debuggable(Goal) ->
        debug_trace2(Goal, Pred, Src, Ln0, Ln1, Dict, Number)
    ; term_to_meta(X, G),
      '$nodebug_call'(G)
    ).
:- endif.

debuggable(Goal) :-
    no_debug_pred(Goal), !, fail.
debuggable(Goal) :-
    in_debug_module(Goal).
debuggable(_) :-
    get_debugger_state(S),
    arg(5, S, [a(_,Ancestor,_,_)|_]),
    in_debug_module(Ancestor).

:- if(defined(optim_comp)).
extract_info('debugger_support:srcdbg_spy'(Goal,Pred,Src,Ln0,Ln1,Dict,Number),
             Goal, Pred, Src, Ln0, Ln1, Dict, Number) :- !.
:- else.
extract_info('debugger_support:srcdbg_spy'(Goal,Pred,Src,Ln0,Ln1,Dict,Number),
             NewGoal, Pred, Src, Ln0, Ln1, Dict, Number) :- !,
    term_to_meta(NewGoal, Goal).
:- endif.
extract_info(Goal, Goal, nil, nil, nil, nil, d([], []), nil).

% ---------------------------------------------------------------------------
%! # Handler code for control_c

:- use_module(library(format)).
:- use_module(library(toplevel/toplevel_io)).

:- doc(hide, do_interrupt_command/0).
:- export(do_interrupt_command/0).
do_interrupt_command :-
    format(user, '~nCiao interruption (h for help)? ', []),
    top_flush,
    top_get_line(Line),
    ( Line = end_of_file -> true % continue
    ; Line = [] -> do_interrupt_command
    ; Line = [C|_],
      do_interrupt_command_(C)
    ).

do_interrupt_command_(0'@) :- !, % @(command)
    do_once_command('| ?- ', d([], [], [])),
    do_interrupt_command.
do_interrupt_command_(0'a) :- !, % a(bort)
    abort.
% do_interrupt_command_(0'b) :- !, % b(reak)
%       break.
do_interrupt_command_(0'c) :- !, % c(ontinue)
    true.
do_interrupt_command_(0'd) :- !, % d(ebug)
    debug.
do_interrupt_command_(0'e) :- !, % e(xit)
    halt.
do_interrupt_command_(0't) :- !, % t(race)
    trace.
do_interrupt_command_(_) :- % h(elp) or other
    interrupt_options,
    do_interrupt_command.

interrupt_options :-
    top_nl,
    top_display('Ciao interrupt options:'), top_nl,
    top_display('    a        abort           - cause abort'), top_nl,
    % top_display('    b        break           - cause break'), top_nl,
    top_display('    c        continue        - do nothing'), top_nl,
    top_display('    d        debug           - start debugging'), top_nl,
    top_display('    t        trace           - start tracing'), top_nl,
    top_display('    e        exit            - cause exit'), top_nl,
    top_display('    @        command         - execute a command'), top_nl,
    top_display('    h        help            - get this list'), top_nl.

% ---------------------------------------------------------------------------
% TODO: Hack to call arbitrary non-exported predicates, deprecate?
%       Note that in optim_comp it only works if the module enables
%      '$pragma'(allow_runtime_expansions).

:- if(defined(optim_comp)).
:- use_module(engine(rt_exp), [rt_pgcall/2]). % TODO: put a $ in the name
:- else.
:- use_module(engine(internals), [module_concat/3]).
:- use_module(engine(hiord_rt), ['$meta_call'/1]).
:- endif.

:- export(call_in_module/2).
%:- meta_predicate call_in_module(?, fact).
:- pred call_in_module(Module, Predicate) : atm * cgoal
   # "Calls predicate @var{Predicate} belonging to module
   @var{Module}, even if that module does not export the
   predicate. This only works for modules which are in debug
   (interpreted) mode (i.e., they are not optimized).".

:- if(defined(optim_comp)).
call_in_module(M, X0) :-
    rt_pgcall(X0, M).
:- else.
call_in_module(Module, Goal) :-
    module_concat(Module, Goal, MGoal),
    '$meta_call'(MGoal).
:- endif.
