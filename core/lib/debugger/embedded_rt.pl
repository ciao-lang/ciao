:- module(embedded_rt, [srcdbg_byrd/7],
% 		trace/0,
% 		tracertc/0,
% 		debug/0,
% 		debugrtc/0,
% 		notrace/0,
% 		nodebug/0,
% 		nodebugrtc/0],
	    [dcg, assertions]).

:- use_module(engine(internals), [term_to_meta/2]).
:- use_module(engine(debugger_support), ['$spypoint'/3]).
:- use_module(engine(hiord_rt),         ['$nodebug_call'/1]).
:- use_module(library(debugger/debugger_lib), [
		debug_trace2/10,
		breakpoint/5,
		get_attributed_vars/3,
		get_debugger_state/1
	    ]).

:- reexport(library(debugger/debugger_lib), [
		debug/0,
		debug_module/1,
		debug_module_source/1,
		debugrtc/0,
		nodebug/0,
		nodebug_module/1,
		nodebugrtc/0,
		nospy/1,
		notrace/0,
		spy/1,
		trace/0,
		tracertc/0
	    ]).

% initialize_srcdebugger :-
% 	format(user_error,'Point 1~n',[]),
% 	set_prolog_flag(embedded_debugger,off),
% 	format(user_error,'Point 2~n',[]),
% 	what_is_on(off).

% Predicates to interact with the debugger

% trace :- debugger_lib:trace.
% debug :- debugger_lib:debug.
% notrace :- debugger_lib:notrace.
% nodebug :- debugger_lib:nodebug.
% debugrtc :- debugger_lib:debugrtc.
% nodebugrtc :- debugger_lib:nodebugrtc.
% tracertc :- debugger_lib:tracertc.

% The embedded debugger
:- meta_predicate srcdbg_byrd(goal, _, _, _, _, _, _).
srcdbg_byrd(X, Pred, Src, L0, L1, Dict, Number) :-
	get_debugger_state(State),
	arg(2, State, Debugging),
	( debuggable(Debugging, X, Pred, Src, L0, L1, Number) ->
	    term_to_meta(X1, X),
	    debug_trace2(X1, State, Pred, Src, L0, L1, Dict, Number,
		get_attributed_vars, debug_call)
	; '$nodebug_call'(X)
	).

debuggable(trace, _, _,    _,   _, _,   _).
debuggable(debug, X, Pred, Src, _, Ln1, Number) :-
	(
	    term_to_meta(G, X),
	    '$spypoint'(G, on, on)
	;
% Ln0 is free because there is no way to determine where the 
% clause start, but the end of the clause can be determine exactly.
	    current_fact(breakpoint(Pred, Src, _Ln0, Ln1, Number))
	),
	!.

:- meta_predicate debug_call(goal).

debug_call(Goal) :- catch(Goal, E, error(['Thrown error ', E])).

get_attributed_vars(Term, AtVars) :-
	get_attributed_vars(Term, AtVars, []).
