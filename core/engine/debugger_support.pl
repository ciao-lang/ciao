:- module(debugger_support, [], [assertions, nortchecks]).

:- use_module(engine(hiord_rt), ['$meta_call'/1]).
:- use_module(engine(internals), [term_to_meta/2]).


:- export('$retry_cut'/2).
:- impl_defined('$retry_cut'/2).
:- trust pred '$retry_cut'/2: int * t_port. % pp - check if that's correct

:- export(t_port/1).
:- true prop t_port(_) + regtype.
t_port(fail).
t_port(call).

:- export('$debugger_state'/2).
:- impl_defined('$debugger_state'/2).
:- trust pred '$debugger_state'/2 : t_debugger_state * t_debugger_state.

:- export(t_debugger_state/1).
:- true prop t_debugger_state/1 + regtype.
t_debugger_state(s(A,B,C,D,E)) :-
	t_debug_flag(A),
	t_debug_flag(B),
	int(C),
	int(D),
	list(E,t_anc).

:- export(t_debug_flag/1).
:- true prop t_debug_flag/1 + regtype.
t_debug_flag(trace).
t_debug_flag(debug).
t_debug_flag(off).

:- export(t_anc/1).
:- true prop t_anc/1 + regtype.
t_anc(a(_,_,_)).


:- export('$spypoint'/3).
:- impl_defined('$spypoint'/3).
:- trust pred '$spypoint'/3: callable * t_on_off * t_on_off.

:- export(t_on_off/1).
:- true prop t_on_off/1 + regtype.
t_on_off(on).
t_on_off(off).


:- export('$debugger_mode'/0).
:- impl_defined('$debugger_mode'/0).
:- trust pred '$debugger_mode'/0.

:- export(srcdbg_spy/7).
:- trust pred srcdbg_spy/7 # "Performing source level debugging, all goals
   are expanded to this. This is currenlty done for all interpreted
   code.".
:- doc(hide,srcdbg_spy/7).
:- meta_predicate srcdbg_spy(goal,?,?,?,?,?,?).

srcdbg_spy(Goal, _, _, _, _, _, _) :-
        term_to_meta(G, Goal),
        '$meta_call'(G).

/*
srcdbg_spy(Goal, _, _, _, _, _, _) :-
	'$debugger_state'(State,State),
	arg(1, State, X),
	( X = off ->
	     term_to_meta(G, Goal),
	     '$meta_call'(G)
	;
	    true
	).
*/
% srcdbg_spy(_,_,_,_,_,_):-
%  	'$debugger_state'(State,State),
%  	(  
%  	    arg(1,State,trace)
%  	;
%  	    arg(1,State,debug)
%  	),!.

% srcdbg_spy(Goal,_,_,_,_,_):-
%  	'$debugger_state'(State,State),
%  	arg(1,State,off),!,
%  	term_to_meta(G,Goal),
% 	'$meta_call'(G).
