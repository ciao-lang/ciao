:- module(debugger_lib, [
		adjust_debugger_state/2,
		breakpoint/5,
		breakpt/6,
		current_debugged/1,
		debug/0,
		debug_mod/2,
		debug_module/1,
		debug_module_source/1,
		debug_trace2/10,
		debugging/0,
		debugger_setting/2,
		display_nv0/3,
		display_nvs/3,
		do_once_command/3,
		functor_spec/5,
		get_attributed_vars/3,
		get_debugger_state/1,
		instantiated/1,
		leash/1,
		list_breakpt/0,
		maxdepth/1,
		nobreakall/0,
		nobreakpt/6,
		nodebug/0,
		nodebug_module/1,
		nospyall/0,
		nospy/1,
		notrace/0,
		port/1,
		print_attributes/3,
		printdepth/1,
		reset_debugger/1,
		retry_hook/4,
		set_defaultopt/1,
		set_defaultopt/3,
		spy/1,
		spypoint/1,
		trace/0
	    ],
	    [assertions, dcg, hiord]).

:- use_module(engine(debugger_support)).
:- use_module(library(ttyout)).
:- use_module(engine(internals), ['$prompt'/2, '$predicate_property'/3,
		'$setarg'/4, term_to_meta/2, '$current_predicate'/2]).
:- use_module(library(aggregates)).
:- use_module(library(lists)).
:- use_module(library(format)).
:- use_module(library(write)).
:- use_module(library(hiordlib)).
:- use_module(library(apply)).
:- use_module(library(sort)).
:- use_module(library(read),   [read_term/3, read/2]).
:- use_module(library(system), [cyg2win_a/3, using_windows/0]).
:- use_module(library(varnames/apply_dict)).
:- use_module(library(varnames/complete_dict), [set_undefined_names/3]).
:- use_module(engine(attributes)).

:- doc(hide, get_debugger_state/1).
:- doc(hide, what_is_on/1).
:- doc(hide, debugging_options/1).
:- doc(hide, spy1/1).
:- doc(hide, nospy1/1).
:- doc(hide, functor_spec/5).

% :- multifile define_flag/3.
% 
% define_flag(debug, [on,debug,trace,off], off).

% Debugger_state = s(DebugFlag, OptDebugFlag, int, int, AncestorList)
% DebugFlag = trace|debug|off
% OptDebugFlag = trace|debug|off

/*
reset_debugger(State) :-
	'$debugger_state'(State, s(off,off,1000000,0,[])),
	'$debugger_mode'.
*/

get_command(Command) :-
	ttydisplay(' ? '),
	ttyflush,
	ttyget(C1),
	get_rest_command(C1, Command).

get_rest_command(0'\n, 0'\n) :- !.
get_rest_command(C1,   Command) :-
	ttyget(C2),
	get_arg(C2, C1, Command).

get_arg(0'\n, C,  C) :- !.
get_arg(C2,   C1, [C1, Arg]) :-
	C2 >= 0'0, C2 =< 0'9, !,
	trd_digits(C2, 0, Arg).
get_arg(0' , C1, C) :-
	ttyget(C2),
	get_arg(C2, C1, C).
get_arg(C2, C1, [C1, Arg]) :-
	trd_string(C2, Arg, "").

trd_digits(Ch, SoFar, I) :-
	Ch >= 0'0, Ch =< 0'9, !,
	Next is SoFar*10 + Ch - 0'0,
	ttyget(Ch1),
	trd_digits(Ch1, Next, I).
trd_digits(0'\n, I, I) :- !.
trd_digits(_,    I, J) :-
	ttyget(Ch),
	trd_digits(Ch, I, J).

trd_string(0'\n, I,       I) :- !.
trd_string(Ch,   [Ch|I0], I) :-
	ttyget(Ch1),
	trd_string(Ch1, I0, I).

mode_message(debug,
'{The debugger will first leap -- showing spypoints and breakpoints (debug)}').
mode_message(trace,
	    '{The debugger will first creep -- showing everything (trace)}').
mode_message(off,
	    '{The debugger is switched off}').

what_is_on(Mode) :-
	mode_message(Mode, Msg),
	ttydisplay(Msg),
	ttynl.

:- pred printopts(DefaultOption, Depth, Attribs, Vars).

:- data printopts/4.

printopts(0'p, 10, true, true).

printdepth(Depth) :- current_fact(printopts(_, Depth, _, _)).

defaultopt(O) :- current_fact(printopts(O, _, _, _)).

reset_printdepth :-
	set_printdepth(10).

set_printdepth(D) :-
	retract_fact(printopts(O, _, A, V)),
	assertz_fact(printopts(O, D, A, V)).

set_defaultopt(O) :-
	retract_fact(printopts(_, D, A, V)),
	assertz_fact(printopts(O, D, A, V)).

proc_extraopts("",   false, false).
proc_extraopts("a",  true,  false).
proc_extraopts("av", true,  true).
proc_extraopts("v",  false, true).
proc_extraopts("va", true,  true).

set_defaultopt(O, A, V) :-
	retract_fact(printopts(_, D, _, _)),
	assertz_fact(printopts(O, D, A, V)).

get_attributed_vars(X, At,                            At) :- atomic(X), !.
get_attributed_vars(X, [attach_attribute(X, AtX)|At], At) :-
	var(X),
	get_attribute(X, AtX), !.
get_attributed_vars(X,      At,  At) :- var(X), !. % No attributes
get_attributed_vars([X|Xs], At0, At2) :- !,
	get_attributed_vars(X,  At0, At1),
	get_attributed_vars(Xs, At1, At2).
get_attributed_vars(X, At0, At1) :-
	functor(X, _, Ar),
	get_attributed_vars_args(Ar, X, At0, At1).

get_attributed_vars_args(0, _, At,  At) :- !.
get_attributed_vars_args(N, X, At0, At2) :-
	N > 0,
	arg(N, X, A),
	get_attributed_vars(A, At0, At1),
	N1 is N - 1,
	get_attributed_vars_args(N1, X, At1, At2).

print_attributes(As, Op, WriteOpts) :-
	list(As, print_attribute(Op, WriteOpts)).

print_attribute(A, Op, WriteOpts) :-
	nl,
	tab(10), % 10 blanks
	display('['),
	write_op(Op, A, WriteOpts),
	display(']').

% Command options
debugging_options :-
	ttydisplay('Debugging options:'), ttynl,
	ttydisplay('   <cr>    creep            c      creep'), ttynl,
	ttydisplay('    l      leap             s      skip'), ttynl,
	ttydisplay('    r      retry            r <i>  retry i'), ttynl,
	ttydisplay('    f      fail             f <i>  fail i'), ttynl,
	ttydisplay('    d <av> display av       p <av> print av'), ttynl,
	ttydisplay('    w <av> write av         a      abort'), ttynl,
	ttydisplay('    v      variables        v <N>  variable N'), ttynl,
	ttydisplay('    g      ancestors        g <n>  ancestors n'), ttynl,
	ttydisplay('    n      nodebug          =      debugging'), ttynl,
	ttydisplay('    +      spy this         -      nospy this'), ttynl,
	ttydisplay('    @      command          u      unify'), ttynl,
	ttydisplay('    <      reset printdepth < <n>  set printdepth'), ttynl,
	ttydisplay('    ^      reset subterm    ^ <n>  set subterm'), ttynl,
	ttydisplay('    ?      help             h      help'), ttynl,
	ttynl,
	ttydisplay('Note: In d, p and w options, you can add'), ttynl,
	ttydisplay('  <a> to show attributes and <v> to show variables.'),
	ttynl,
	ttynl.

display_nv0(Name=Value, Op, WO) :-
	display_list(['\t   ', Name, ' = ']),
	write_op(Op, Value, WO).

display_nv(NameValue, Op, WO) :-
	display(','), nl,
	display_nv0(NameValue, Op, WO).

display_nvs([],               _,  _).
display_nvs([NameValue|Dict], Op, WO) :-
	display_nv0(NameValue, Op, WO),
	list(Dict, display_nv(Op, WO)),
	nl.

instantiated(Name = Value) :- '$VAR'(Name) \== Value.

sel_instantiated(NameValue) --> {instantiated(NameValue)}, !, [NameValue].
sel_instantiated(_) --> [].

get_write_options(true, [_|_], D, [max_depth(D), numbervars(true)]) :-
	!.
get_write_options(_, _, D, [max_depth(D), numbervars(true), portrayed(true)]).

write_goal2(Op, Goal0, d(_, _, ADict0), AtVars0) :-
	current_fact(printopts(_, D, A, V)),
	sort(AtVars0, AtVars1),
	( V == true ->
	    apply_dict(t(ADict0, Goal0, AtVars1), ADict0,
		t(ADict, Goal, AtVars)),
	    map(ADict, sel_instantiated, AInst, [])
	;
	    ADict = ADict0,
	    Goal = Goal0,
	    AtVars = AtVars1
	),
	get_write_options(A, AtVars, D, WriteOpts),
	write_op(Op, Goal, WriteOpts),
	(V == true -> list(AInst, display_nv(Op, WriteOpts)) ;  true),
	(A == true -> print_attributes(AtVars, Op, WriteOpts) ; true).

write_op(0'p, Goal, WriteOpts) :- write_term(Goal, WriteOpts).
write_op(0'd, Goal, _) :- display(Goal).
write_op(0'w, Goal, _) :- writeq(Goal).

uninstantiated(_ = Value) :- var(Value).

show_variable_values(Dict0, Dict, VarKind, Op, WO) :-
	include(instantiated,   Dict,  DictInst),
	include(uninstantiated, Dict0, DictUninst),
	( DictInst == [] -> true
	; format(user,
		'         {Instantiated ~w-defined variables in scope:~n',
		[VarKind]),
	    display_nvs(DictInst, Op, WO),
	    format(user, '         }~n', [])
	),
	( DictUninst == [] -> true
	;
	    (DictInst == [] -> All = '(all) ' ; All = ''),
	    format(user,
		'         {Uninstantiated ~w~w-defined variables in scope:~n',
		[All, VarKind]),
	    display_nvs(DictUninst, Op, WO),
	    format(user, '         }~n', [])
	).


:- meta_predicate write_goal(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, pred(2)).
write_goal(T, X, Xs, B, D, Port, Pred, Src, Ln0, Ln1, Dict, Number,
	    GetAttributedVars) :-
	reset_debugger(State),
	port_info(Port, Pport),
	current_output(CO),
	set_output(user),
	do_write_goal(T, X, Xs, B, D, Pport, Pred, Src, Ln0, Ln1, Dict,
	    Number, GetAttributedVars),
	set_output(CO),
	set_debugger(State).

:- meta_predicate do_write_goal(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, pred(2)).
do_write_goal(0'v, X, _, _, _, _, _, _, _, _, Dict, _, GetAttributedVars) :-
	!,
	write_goal_v(X, Dict, GetAttributedVars).
do_write_goal([0'v, SName], _, _, _, _, _, _, _, _, _, Dict, _,
	    GetAttributedVars) :-
	!,
	write_goal_v_name(SName, Dict, GetAttributedVars).
do_write_goal(T, X, Xs, B, D, Pport, Pred, Src, Ln0, Ln1, Dict, Number,
	    GetAttributedVars) :-
	print_srcdbg_info(Pred, Src, Ln0, Ln1, Number),
	spy_info(Xs, X, Mark0, S, []),
	( Mark0 == '   ' -> break_info(Pred, Src, Ln0, Ln1, Number, Mark)
	; Mark=Mark0
	),
	display_list([Mark, B, '  ', D, Pport|S]),
	GetAttributedVars(X, AtVars),
	write_goal2(T, X, Dict, AtVars).

:- meta_predicate write_goal_v(?, ?, pred(2)).
write_goal_v(X0, d(UDict0, CDict0, _), GetAttributedVars) :-
	current_fact(printopts(Op, D, A, _)),
	append(UDict0, CDict0, Dict0),
	GetAttributedVars(X0-Dict0, UnsortedAtVars),
	sort(UnsortedAtVars, AtVars0),
	apply_dict(
	    t(AtVars0, UDict0, CDict0),
	    Dict0,
	    t(AtVars, UDict, CDict)),
	get_write_options(A, AtVars, D, WriteOpts),
	show_variable_values(UDict0, UDict, user,     Op, WriteOpts),
	show_variable_values(CDict0, CDict, compiler, Op, WriteOpts),
	(A == true -> print_attributes(AtVars, Op, WriteOpts) ; true).

:- meta_predicate write_goal_v_name(?, ?, pred(2)).
write_goal_v_name(SName, d(UDict, CDict, _), GetAttributedVars) :-
	current_fact(printopts(Op, D, A, _)),
	append(UDict, CDict, Dict),
	atom_codes(Name, SName),
	( member(Name=Value0, Dict) ->
	    GetAttributedVars(Value0, AtVars0),
	    apply_dict(Value0-AtVars0, Dict, Value-AtVars),
	    get_write_options(A, AtVars, D, WriteOpts),
	    display_var(Name, Value0, Value, Op, WriteOpts),
	    (A == true -> print_attributes(AtVars, Op, WriteOpts) ; true)
	;
	    format(user, '{~w not defined here}~n', [Name])
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

warn_if_udp(F, _, _) :- '$predicate_property'(F, _, _), !.
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
	ttynl,
	format(user, '{Cannot spy built-in predicate ~q}~n', [N/A]).

spy1(Pred) :-
	functor(Pred, N, A),
	warn_if_udp(Pred, N, A),
	install_spypoint(Pred, N, A).

nospy1(Pred) :-
	functor(Pred, N, A),
	warn_if_udp(Pred, N, A),
	remove_spypoint(Pred, N, A).

:- pred nospy(PredSpec) : sequence(multpredspec)
# "Remove spy-points on predicates belonging to debugged modules
          which match @var{PredSpec}. This predicate is defined as a prefix
          operator by the toplevel.".

nospy(Preds) :-
	parse_functor_spec(Preds, X, nospy1(X)).

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

all_spypoints :-
	spypoint(_),
	!,
	ttydisplay('Spypoints:'),
	list_spypoints.
all_spypoints :-
	ttydisplay('{There are no spypoints}'), ttynl.

list_spypoints :-
	spypoint(X),
	functor(X, N, A),
	ttynl, tab(user, 4), write(user, N/A),
	fail.
list_spypoints :-
	ttynl.

:- pred breakpt(Pred, Src, Ln0, Ln1, Number, RealLine)
	: atm * sourcename * int * int * int * int

# "Set a @index{breakpoint} in file @var{Src} between lines
          @var{Ln0} and @var{Ln1} at the literal corresponding to the
          @var{Number}'th occurence of (predicate) name @var{Pred}.  The
          pair @var{Ln0}-@var{Ln1} uniquely identifies a program clause and
          must correspond to the
          start and end line numbers for the clause. The rest of the
          arguments provide enough information to be able to locate the
          exact literal that the @var{RealLine} line refers to. This is
          normally not issued by users but rather by the @apl{emacs} mode,
          which automatically computes the different argument after
          selecting a point in the source file.".

:- pred breakpoint(Pred, Src, Ln0, Ln1, Number) # "Breakpoint storage.".

:- data breakpoint/5.

breakpt(Pred, Src, Ln0, Ln1, Number, RealLine) :-
	current_fact(breakpoint(Pred, Src, Ln0, Ln1, Number)), !,
	format(user,
	    '{There is already a breakpoint on literal ~a in line ~d}~n',
	    [Pred, RealLine]).

breakpt(Pred, Src, Ln0, Ln1, Number, RealLine) :-
	get_debugger_state(State),
	(arg(1, State, off) -> debug ; true),
	assertz_fact(breakpoint(Pred, Src, Ln0, Ln1, Number)),
	format(user, '{Breakpoint placed on literal ~a in line ~d}~n',
	    [Pred, RealLine]).

:- pred nobreakall/0 # "Remove all breakpoints.".

nobreakall :-
	retractall_fact(breakpoint(_, _, _, _, _)),
	format(user, '{All breakpoints removed}~n', []).

:- pred nobreakpt(Pred, Src, Ln0, Ln1, Number, RealLine)
	: atm * sourcename * int * int * int * int
# "Remove a breakpoint in file @var{Src} between lines
          @var{Ln0} and @var{Ln1} at the @var{Number}'th occurence of
          (predicate) name @var{Pred} (see @pred{breakpt/6}). Also 
	  normally used from de @apl{emacs} mode.".

nobreakpt(Pred, Src, Ln0, Ln1, Number, RealLine) :-
	retract_fact(breakpoint(Pred, Src, Ln0, Ln1, Number)), !,
	format(user, '{Breakpoint removed from literal ~a in line ~d}~n',
	    [Pred, RealLine]).
nobreakpt(Pred, _, _, _, _, RealLine) :-
	format(user, '{No breakpoint on literal ~a in line ~d}~n',
	    [Pred, RealLine]).

:- pred list_breakpt/0 # "Prints out the location of all
	breakpoints. The location of the breakpoints is showed usual by
	referring to the source file, the lines between which the predicate
	can be found, the predicate name and the number of ocurrence of the
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

show_leash_info([]) :- !,
	format(user, '{No leashing}~n', []).
show_leash_info(Ps) :-
	format(user, '{Using leashing stopping at ~w ports}~n', [Ps]).

debugger_setting(Old, New) :-
	get_debugger_state(State),
	arg(1, State, Old),
	'$setarg'(1, State, New, true),
	adjust_debugger_state(State, New).

get_debugger_state(L) :-
	'$debugger_state'(L, L).

adjust_debugger_state(State, New) :-
	'$setarg'(2, State, New,     true),
	'$setarg'(3, State, 1000000, true),
	'$debugger_mode'.

:- pred debug/0 # "Switches the debugger on. The interpreter will
        stop at all ports of procedure boxes of spied predicates.".

debug :-
	debugger_setting(_, debug),
	what_is_on(debug).

call_hook2(answer(X), X).
call_hook2(no,        X) :- call_hook1(X).

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

a_length([],                0).
a_length([a(_, _, X, _)|_], X).

:- data debugdepth/1.

debugdepth(100000).

retry_hook(_,          P,  P, _).
retry_hook(Invocation, P0, P, A) :- retry_hook(Invocation, P0, P, A).

retry_hook_(X, B, D, [a(B, X, D, Dict)|A], A, Port, State, Dict) :-
	State = s(_, _, _, B0, A),
	a_length(A, D0),
	B is B0+1,
	D is D0+1,
	( current_fact(debugdepth(M)), D=<M -> true
	; adjust_debugger_state(State, trace),
	    warning('Interpreter maxdepth exceeded')
	),
	retry_hook(B, call, Port, '$$retry_hook').

port_info(block,   '  Block: ').
port_info(unblock, '  Unblock: ').
port_info(call,    '  Call: ').
port_info(exit,    '  Exit: ').
port_info(redo,    '  Redo: ').
port_info(fail,    '  Fail: ').
port_info(void,    '  ').

print_srcdbg_info(_,    _,   nil, nil, nil) :- !.
print_srcdbg_info(Pred, Src, Ln0, Ln1, Number) :-
	( using_windows -> % running in a Windows non-cygwin shell
	    %
	    % Emacs understand slashes instead of backslashes, even on
	    % Windows, and this saves problems with escaping
	    % backslashes
	    cyg2win_a(Src, ActualSrc, noswap)
	; Src = ActualSrc
	),
	display_list(['         In ', ActualSrc, ' (', Ln0, -, Ln1, ') ',
		Pred, -, Number, '\n']).

spy_info([],       Goal, ' + ') --> {'$spypoint'(Goal, on, on)}, !.
spy_info([],       _,    '   ') --> [].
spy_info([I-X|Xs], _,    Goal) -->
	spy_info(Xs, X, Goal),
	[^, I].

lastof([],      X, X).
lastof([X0|Xs], _, X) :- lastof(Xs, X0, X).

do_retry_fail(B, State, Port) :-
	'$retry_cut'(B, Port),
	'$setarg'(2, State, trace, true), % retry implies creep!
	fail.

reset_debugger(State) :-
	'$debugger_state'(State, s(off, off, 1000000, 0, [])),
	'$debugger_mode'.

set_debugger(State) :-
	'$debugger_state'(_, State),
	'$debugger_mode'.

:- data leashed/1.

leashed(call).
leashed(exit).
leashed(redo).
leashed(fail).

:- pred nodebug/0 # "Switches the debugger off.  If there are any
        spy-points set then they will be kept but disabled.".

nodebug :- notrace.

:- pred trace/0 # "Start tracing, switching the debugger on if
        needed.  The interpreter will stop at all leashed ports of
        procedure boxes of predicates either belonging to debugged
        modules or called from clauses of debugged modules.  A message
        is printed at each stop point, expecting input from the user
        (write @tt{h} to see the available options).".

trace :-
	debugger_setting(_, trace),
	what_is_on(trace).

:- pred notrace/0 # "Equivalent to @pred{nodebug/0}.".

notrace :-
	debugger_setting(_, off),
	what_is_on(off).

:- doc(doinclude, multpredspec/1).

:- true prop multpredspec/1 + regtype.

multpredspec(Mod:Spec) :- atm(Mod), multpredspec(Spec).
multpredspec(Name/Low-High) :- atm(Name), int(Low), int(High).
multpredspec(Name/(Low-High)) :- atm(Name), int(Low), int(High).
multpredspec(Name/Arity) :- atm(Name), int(Arity).
multpredspec(Name) :- atm(Name).

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


:- data debug_rtc_db/0.
:- export(debug_rtc_db/0).
debug_rtc_db.

:- pred debugrtc/0 # "Start tracing when a run-time check error be
	raised".

:- export(debugrtc/0).
debugrtc :-
	debug_rtc_db -> true ; assertz_fact(debug_rtc_db).

:- pred debugrtc/0 # "Do not start tracing when a run-time check error
	be raised".

:- export(nodebugrtc/0).
nodebugrtc :-
	retract_fact(debug_rtc_db).

:- pred tracertc/0 # "Start tracing if the debugger and debug_rtc are
	activated".

:- export(tracertc/0).
tracertc :-
	get_debugger_state(State),
	\+ arg(1, State, off),
	debug_rtc_db ->
	trace
    ;
	true.

:- data debug_mod/2.

current_debugged(Ms) :- findall(M, current_fact(debug_mod(M, _)), Ms).

:- pred debug_module(Module) : atm(Module)
# "The debugger will take into acount module @var{Module}
          (assuming it is loaded in interpreted mode).  When issuing this
          command at the toplevel shell, the compiler is instructed also
          to set to @em{interpret} the loading mode of files defining that
          module and also to mark it as 'modified' so that (re)loading 
	  this file or a main file that uses this module will force it 
	  to be reloaded for source-level debugging.".

debug_module(M) :- atom(M), !,
	( current_fact(debug_mod(M, _)) ->
	    true
	; atom_concat(M, ':', Mc),
	    assertz_fact(debug_mod(M, Mc))
	).
debug_module(M) :-
	format(user_error, '{Bad module ~q - must be an atom}~n', [M]).

:- pred nodebug_module(Module) : atm(Module)
# "The debugger will not take into acount module @var{Module}.
          When issuing this command at the toplevel shell, the compiler is
          instructed also to set to @em{compile} the loading mode of files
          defining that module.".

nodebug_module(M) :- % If M is a var, nodebug for all
	retractall_fact(debug_mod(M, _)).
%        what_is_debugged.

:- meta_predicate parse_functor_spec(?, ?, goal).

parse_functor_spec(V, _, _) :-
	var(V), !,
	format(user_error, '{A variable is a bad predicate indicator}~n', []).
parse_functor_spec((S, Ss), GoalArg, Goal) :-
	parse_functor_spec(S,  GoalArg, Goal),
	parse_functor_spec(Ss, GoalArg, Goal).
parse_functor_spec(S, GoalArg, Goal) :-
	Flag=f(0),
	( functor_spec(S, Name, Low, High, M),
	    current_fact(debug_mod(M, Mc)),
	    atom_concat(Mc, Name, PredName),
	    '$current_predicate'(PredName, GoalArg),
	    functor(GoalArg, _, N),
	    N >= Low, N =< High,
	    '$setarg'(1, Flag, 1, true),
	    '$nodebug_call'(Goal),
	    fail
	; Flag=f(0),
	    format(user_error,
		"{Bad predicate indicator or predicate undefined "||
		"in modules currently debugged:~n ~w}~n", [S]),
	    fail
	; true
	).

%% This entry point is only for documentation purposes.
:- pred debug_module_source(Module) : atm(Module)
# "The debugger will take into acount module @var{Module}
	  (assuming it is is loaded in source-level debug mode).  When 
	  issuing this command at the toplevel shell, the compiler is 
	  instructed also to set to @em{interpret} the loading mode of 
	  files defining that module and also to mark it as 'modified'
	  so that (re)loading this file or a main file that uses this
	  module will force it to be reloaded for source-level debugging.".

debug_module_source(M) :-
	debug_module(M).

what_is_debugged :-
	current_debugged(Ms),
	( Ms = [] ->
	    format(user, '{No module is selected for debugging}~n', [])
	; format(user, '{Modules selected for debugging: ~w}~n', [Ms])
	).

:- pred spy(PredSpec) : sequence(multpredspec)
# "Set spy-points on predicates belonging to debugged modules and
	  which match @var{PredSpec}, switching the debugger on if
          needed. This predicate is defined as a prefix operator by the
          toplevel.".

spy(Preds) :-
	get_debugger_state(State),
	(arg(1, State, off) -> debug ; true),
	parse_functor_spec(Preds, X, spy1(X)).

:- pred debugging/0 # "Display debugger state.".

debugging :-
	get_debugger_state(State),
	arg(1, State, G),
	what_is_on(G),
	what_is_debugged,
	what_is_leashed,
	what_maxdepth,
	all_spypoints,
	ttynl.

:- prop port(X) + regtype.

port(call).
port(exit).
port(redo).
port(fail).

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

what_is_leashed :-
	is_leashed([call, exit, redo, fail], L),
	show_leash_info(L).

is_leashed([],     []).
is_leashed([X|Xs], [X|Ys]) :- current_fact(leashed(X)), !, is_leashed(Xs, Ys).
is_leashed([_|Xs], Ys) :- is_leashed(Xs, Ys).

:- pred maxdepth(MaxDepth) : int
# "Set maximum invocation depth in debugging to
           @var{MaxDepth}. Calls to compiled predicates are not included
           in the computation of the depth.".

maxdepth(D) :-
	integer(D), !,
	retractall_fact(debugdepth(_)),
	assertz_fact(debugdepth(D)),
	what_maxdepth.
maxdepth(D) :-
	format(user_error, '{Bad maxdepth ~q - must be an integer}~n', [D]).

what_maxdepth :-
	current_fact(debugdepth(M)),
	format(user, '{Interpreter maxdepth is ~w}~n', [M]).

:- meta_predicate do_once_command(?, pred(1), ?).
do_once_command(Prompt, DebugCall, d(UDict, CDict, _ADict)) :-
	'$prompt'(OldPrompt, Prompt),
	reset_debugger(State),
	read_term(user, Command, [variable_names(Dict0)]),
	append(UDict, CDict, Dict),
	% Variable Binding between Command and Program:
	union(Dict, Dict0, _),
	'$prompt'(_, '|: '),
	catch((DebugCall(Command) -> Y=yes ; Y=no), E, Y=ex),
	'$prompt'(_, OldPrompt),
	(
	    Y=yes ->
	    current_fact(printopts(Op, D, A, _)),
	    get_write_options(A, [], D, WriteOpts),
	    display_nvs(Dict0, Op, WriteOpts)
	;
	    Y=no ->
	    format(user_error, '{Warning: goal failed}~n', [])
	;
	    %Y=ex ->
	    format(user_error, '{Warning: exception thrown ~w}~n', [E])
	),
	set_debugger(State).

:- meta_predicate show_ancestors(?, ?, pred(2)).
show_ancestors([_], _, _) :- !,
	ttynl, ttydisplay('No ancestors.'), ttynl.
show_ancestors([_|CA], N, GetAttributedVars) :-
	ttynl, ttydisplay('Ancestors:'), ttynl,
	list_ancestors(CA, N, GetAttributedVars).

:- meta_predicate list_ancestors(?, ?, pred(2)).
list_ancestors([],                    _,  _) :- !.
list_ancestors(_,                     0,  _) :- !.
list_ancestors([a(B, X, D, Dict)|As], N0, GetAttributedVars) :-
	N is N0-1,
	list_ancestors(As, N, GetAttributedVars),
	defaultopt(Op),
	write_goal(Op, X, [], B, D, void, _Pred, _Src, nil, nil, Dict, nil,
	    GetAttributedVars),
	ttynl.

:- meta_predicate debug_trace2(?, ?, ?, ?, ?, ?, ?, ?, pred(2), pred(1)).
debug_trace2(X, State, Pred, Src, L0, L1, d(UDict, CDict), Number,
	    GetAttributedVars, DebugCall) :-
	append(UDict, CDict, Dict0),
	set_undefined_names(Dict0, 1, _),
	select_applicable(X, Dict0, ADict),
	Dict = d(UDict, CDict, ADict),
	retry_hook_(X, B, D, NA, OA, Port, State, Dict),
	'$setarg'(4, State, B,  on),
	'$setarg'(5, State, NA, on),
	(
	    Port = call,
	    '$metachoice'(C0),
	    call_hook(X, B, D, State, Pred, Src, L0, L1, Dict, Number,
		GetAttributedVars, DebugCall),
	    '$metachoice'(C1)
	; fail_hook(X, B, D, State, Pred, Src, L0, L1, Dict, Number,
		GetAttributedVars, DebugCall), !, fail
	),
	( exit_hook(X, B, D, State, Pred, Src, L0, L1, Dict, Number,
		GetAttributedVars, DebugCall)
	; redo_hook(X, B, D, State, Pred, Src, L0, L1, Dict, Number,
		GetAttributedVars, DebugCall), fail
	),
	% Remove choicepoints in deterministic goals to speed up debugging -- EMM
	(C0 == C1 -> ! ; true),
	'$setarg'(5, State, OA, on).

:- meta_predicate call_hook(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, pred(2), pred(1)).

call_hook(X, B, _, State, _, _, _, _, _, _, _, _) :-
	arg(3, State, Level),
	B>Level, !,
	call_hook1(X).
call_hook(X, B, D, State, Pred, Src, Ln0, Ln1, Dict, Number,
	    GetAttributedVars, DebugCall) :-
	debug_port(X, B, D, call, State, Msg, Pred, Src, Ln0, Ln1, Dict,
	    Number, GetAttributedVars, DebugCall),
	call_hook2(Msg, X).

:- meta_predicate exit_hook(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, pred(2), pred(1)).

exit_hook(_, B, _, State, _, _, _, _, _, _, _, _) :-
	arg(3, State, Level), B>Level, !.
exit_hook(X, B, D, State, Pred, Src, Ln0, Ln1, Dict, Number,
	    GetAttributedVars, DebugCall) :-
	'$setarg'(3, State, 1000000, true),
	debug_port(X, B, D, exit, State, _, Pred, Src, Ln0, Ln1, Dict, Number,
	    GetAttributedVars, DebugCall).

:- meta_predicate redo_hook(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, pred(2), pred(1)).
redo_hook(_, B, _, State, _, _, _, _, _, _, _, _) :-
	arg(3, State, Level), B>Level, !.
redo_hook(X, B, D, State, Pred, Src, Ln0, Ln1, Dict, Number,
	    GetAttributedVars, DebugCall) :-
	debug_port(X, B, D, redo, State, _, Pred, Src, Ln0, Ln1, Dict, Number,
	    GetAttributedVars, DebugCall).

:- meta_predicate fail_hook(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, pred(2), pred(1)).
fail_hook(_, B, _, State, _, _, _, _, _, _, _, _) :-
	arg(3, State, Level), B>Level, !.
fail_hook(X, B, D, State, Pred, Src, Ln0, Ln1, Dict, Number,
	    GetAttributedVars, DebugCall) :-
	'$setarg'(3, State, 1000000, true),
	debug_port(X, B, D, fail, State, _, Pred, Src, Ln0, Ln1, Dict, Number,
	    GetAttributedVars, DebugCall).

:- meta_predicate debug_port(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, pred(2),
	    pred(1)).
debug_port(X, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number,
	    GetAttributedVars, DebugCall) :-
	(
	    '$spypoint'(X, on, on)
	;
%	    % Ln0 is free because there is no way to determine where the 
%	    % clause starts, but the end of the clause can be determined exactly.
%	    current_fact(breakpoint(Pred, Src, _Ln0, Ln1, Number))
	    %
	    % JFMC: The Ciao emacs mode needs a number here. Since
	    %   this only affects the output message, it seems that
	    %   there is no problem in using the breakpoint Ln0.
	    current_fact(breakpoint(Pred, Src, Ln0, Ln1, Number))
	),
	!,
	defaultopt(Op),
	prompt_command(Op, X, [], B, D, Port, State, Msg, Pred, Src, Ln0, Ln1,
	    Dict, Number, GetAttributedVars, DebugCall).
debug_port(X, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number,
	    GetAttributedVars, DebugCall) :-
	arg(2, State, trace),
	current_fact(leashed(Port)), !,
	defaultopt(Op),
	prompt_command(Op, X, [], B, D, Port, State, Msg, Pred, Src, Ln0, Ln1,
	    Dict, Number, GetAttributedVars, DebugCall).
debug_port(X, B, D, Port, State, no, Pred, Src, Ln0, Ln1, Dict, Number,
	    GetAttributedVars, _) :-
	arg(2, State, trace), !,
	defaultopt(Op),
	write_goal(Op, X, [], B, D, Port, Pred, Src, Ln0, Ln1, Dict, Number,
	    GetAttributedVars),
	ttynl.
debug_port(_, _, _, _, _, no, _, _, _, _, _, _, _, _).

:- meta_predicate prompt_command(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?,
	    pred(2), pred(1)).

prompt_command(T, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict,
	    Number, GetAttributedVars, DebugCall) :-
	write_goal(T, X, Xs, B, D, Port, Pred, Src, Ln0, Ln1, Dict, Number,
	    GetAttributedVars),
	get_command(C),
	do_trace_command(C, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1,
	    Dict, Number, GetAttributedVars, DebugCall).

:- meta_predicate do_trace_command(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?,
	    pred(2), pred(1)).
do_trace_command(0'a, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) :- !,
	% a(bort)
	abort.
do_trace_command(0'c, _, _, _, _, _, State, no, _, _, _, _, _, _, _, _) :- !,
	% c(reep)
	'$setarg'(2, State, trace, true),
	'$debugger_mode'.
do_trace_command(0'\n, _, _, _, _, _, State, no, _, _, _, _, _, _, _, _) :-
	!, % CR (creep)
	'$setarg'(2, State, trace, true),
	'$debugger_mode'.
do_trace_command(0'd, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict,
	    Number, GetAttributedVars, DebugCall) :- !, % d(isplay)
	set_defaultopt(0'd, false, false),
	prompt_command(0'd, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1,
	    Dict, Number, GetAttributedVars, DebugCall).
do_trace_command([0'd, AV], X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1,
	    Dict, Number, GetAttributedVars, DebugCall) :- % d(isplay)
	proc_extraopts(AV, A, V), !,
	set_defaultopt(0'd, A, V),
	prompt_command(0'd, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1,
	    Dict, Number, GetAttributedVars, DebugCall).
do_trace_command(0'g, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict,
	    Number, GetAttributedVars, DebugCall) :- !, % g(ancestors)
	arg(5, State, CA),
	show_ancestors(CA, -1, GetAttributedVars),
	defaultopt(Op),
	prompt_command(Op, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1,
	    Dict, Number, GetAttributedVars, DebugCall).
do_trace_command([0'g, Arg], X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0,
	    Ln1, Dict, Number, GetAttributedVars, DebugCall) :- !,
	% g(ancestors) arg
	arg(5, State, CA),
	show_ancestors(CA, Arg, GetAttributedVars),
	defaultopt(Op),
	prompt_command(Op, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1,
	    Dict, Number, GetAttributedVars, DebugCall).
do_trace_command(0'l, _, _, _, _, _, State, no, _, _, _, _, _, _, _, _) :- !,
	% l(eap)
	'$setarg'(2, State, debug, true),
	'$debugger_mode'.
do_trace_command(0'n, _, _, _, _, _, State, no, _, _, _, _, _, _, _, _) :-
	!, % n(odebug)
	% nodebug.
	'$setarg'(3, State, 0, true).
do_trace_command(0'p, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict,
	    Number, GetAttributedVars, DebugCall) :- !, % p(rint)
	set_defaultopt(0'p, false, false),
	prompt_command(0'p, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1,
	    Dict, Number, GetAttributedVars, DebugCall).
do_trace_command([0'p, AV], X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1,
	    Dict, Number, GetAttributedVars, DebugCall) :- % p(rint)
	proc_extraopts(AV, A, V), !,
	set_defaultopt(0'p, A, V),
	prompt_command(0'p, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1,
	    Dict, Number, GetAttributedVars, DebugCall).
do_trace_command(0'v, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict,
	    Number, GetAttributedVars, DebugCall) :- !, % v(ariables)
	prompt_command(0'v, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1,
	    Dict, Number, GetAttributedVars, DebugCall).
do_trace_command([0'v, Name], X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0,
	    Ln1, Dict, Number, GetAttributedVars, DebugCall) :- !, % v(ariables)
	prompt_command([0'v, Name], X, Xs, B, D, Port, State, Msg, Pred, Src,
	    Ln0, Ln1, Dict, Number, GetAttributedVars, DebugCall).
do_trace_command(0'r, _, _, _, _, _, State, no, _, _, _, _, _, _, _, _) :-
	!, % r(etry)
	arg(5, State, [a(B, _, _, _)|_]),
	do_retry_fail(B, State, call).
do_trace_command([0'r, B], _, _, _, _, _, State, no, _, _, _, _, _, _, _, _) :-
	!, % r(etry) arg
	do_retry_fail(B, State, call).
do_trace_command(0'f, _, _, _, _, _, State, no, _, _, _, _, _, _, _, _) :-
	!, %f(ail)
	arg(5, State, [a(B, _, _, _)|_]),
	do_retry_fail(B, State, fail).
do_trace_command([0'f, B], _, _, _, _, _, State, no, _, _, _, _, _, _, _, _) :-
	!, % f(ail) arg
	do_retry_fail(B, State, fail).
do_trace_command(0's, _, _, B, _, Port, State, no, _, _, _, _, _, _,
	    GetAttributedVars, DebugCall) :- % s(kip)
	set_skip(Port, B, State, GetAttributedVars, DebugCall), !.
do_trace_command(0'w, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict,
	    Number, GetAttributedVars, DebugCall) :-
	!, % w(rite)
	set_defaultopt(0'w, false, false),
	prompt_command(0'w, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1,
	    Dict, Number, GetAttributedVars, DebugCall).
do_trace_command([0'w, AV], X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1,
	    Dict, Number, GetAttributedVars, DebugCall) :-
	proc_extraopts(AV, A, V), !,
	set_defaultopt(0'w, A, V),
	prompt_command(0'w, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1,
	    Dict, Number, GetAttributedVars, DebugCall).
do_trace_command(0'+, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict,
	    Number, GetAttributedVars, DebugCall) :-
	!, % +(spy this)
	lastof(Xs, _-X, _-Goal),
	spy1(Goal),
	defaultopt(Op),
	prompt_command(Op, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1,
	    Dict, Number, GetAttributedVars, DebugCall).
do_trace_command(0'-, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict,
	    Number, GetAttributedVars, DebugCall) :-
	!, % -(nospy this)
	lastof(Xs, _-X, _-Goal),
	nospy1(Goal),
	defaultopt(Op),
	prompt_command(Op, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1,
	    Dict, Number, GetAttributedVars, DebugCall).
do_trace_command(0'=, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict,
	    Number, GetAttributedVars, DebugCall) :-
	!, % =(debugging)
	reset_debugger(_),
	debugging,
	set_debugger(State),
	defaultopt(Op),
	prompt_command(Op, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1,
	    Dict, Number, GetAttributedVars, DebugCall).
% do_trace_command(0'b, X, Xs, B, D, Port, State, Msg) :- !, % b(reak)
% 	break,
% 	prompt_command(0'p, X, Xs, B, D, Port, State, Msg).
do_trace_command(0'@, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict,
	    Number, GetAttributedVars, DebugCall) :-
	!, %@ (command)
	do_once_command('| ?- ', DebugCall, Dict),
	defaultopt(Op),
	prompt_command(Op, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1,
	    Dict, Number, GetAttributedVars, DebugCall).
do_trace_command(0'u, _, _, _, _, call, _, answer(X1), _, _, _, _, _, _, _, _)
:- !, %u (unify)
	'$prompt'(Old, '|: '),
	read(user, X1),
	'$prompt'(_, Old).
do_trace_command(0'<, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict,
	    Number, GetAttributedVars, DebugCall) :-
	!, %< (reset printdepth)
	reset_printdepth,
	defaultopt(Op),
	prompt_command(Op, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1,
	    Dict, Number, GetAttributedVars, DebugCall).
do_trace_command([0'<, I], X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1,
	    Dict, Number, GetAttributedVars, DebugCall) :-
	!, %< arg (set printdepth)
	set_printdepth(I),
	defaultopt(Op),
	prompt_command(Op, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1,
	    Dict, Number, GetAttributedVars, DebugCall).
do_trace_command(0'^, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict,
	    Number, GetAttributedVars, DebugCall) :-
	!, %^ (reset subterm)
	lastof(Xs, _-X, _-Goal),
	defaultopt(Op),
	prompt_command(Op, Goal, [], B, D, Port, State, Msg, Pred, Src, Ln0,
	    Ln1, Dict, Number, GetAttributedVars, DebugCall).
do_trace_command([0'^, 0], _, [_-X|Xs], B, D, Port, State, Msg, Pred, Src, Ln0,
	    Ln1, Dict, Number, GetAttributedVars, DebugCall) :-
	!, %^ 0 (up subterm)
	defaultopt(Op),
	prompt_command(Op, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1,
	    Dict, Number, GetAttributedVars, DebugCall).
do_trace_command([0'^, I], X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1,
	    Dict, Number, GetAttributedVars, DebugCall) :- %^ arg (set subterm)
	arg(I, X, Ith), !,
	defaultopt(Op),
	prompt_command(Op, Ith, [I-X|Xs], B, D, Port, State, Msg, Pred, Src,
	    Ln0, Ln1, Dict, Number, GetAttributedVars, DebugCall).
do_trace_command(0'?, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict,
	    Number, GetAttributedVars, DebugCall) :-
	!, % ?(help)
	debugging_options,
	defaultopt(Op),
	prompt_command(Op, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1,
	    Dict, Number, GetAttributedVars, DebugCall).
do_trace_command(0'h, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict,
	    Number, GetAttributedVars, DebugCall) :-
	!, % h(elp)
	debugging_options,
	defaultopt(Op),
	prompt_command(Op, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1,
	    Dict, Number, GetAttributedVars, DebugCall).
do_trace_command(_, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict,
	    Number, GetAttributedVars, DebugCall) :- % all others
	format(user, '{Option not applicable at this port}~n', []),
	defaultopt(Op),
	prompt_command(Op, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1,
	    Dict, Number, GetAttributedVars, DebugCall).

:- meta_predicate set_skip(?, ?, ?, pred(2), pred(1)).
set_skip(call, To, State, _, _) :-
	'$setarg'(3, State, To, true).
set_skip(redo, To, State, _, _) :-
	'$setarg'(3, State, To, true).
set_skip(_, _, State, GetAttributedVars, DebugCall) :-
	format(user, '{Skip not applicable at this port, creeping ...}~n', []),
	do_trace_command(0'c, _, _, _, _, _, State, no, _, _, _, _, _, _,
	    GetAttributedVars, DebugCall).
