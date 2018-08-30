:- module(fibers_rt, [], [assertions, regtypes, fsyntax, hiord]).

:- doc(title, "Runtime support for fibers").

:- doc(module, "This module implements the internal fiber state and a
   basic scheduler. See @tt{fibers_hooks.pl} for details on hooks.

   @begin{alert}
   Very low-level, use with care.
   @end{alert}

   @begin{alert}
   Hooks should not be used directly. It should rely on
   @tt{suspendable} declaration instead.
   @end{alert}
").

:- use_module(engine(data_facts)).

:- use_module(library(fibers/stream_watchdog)).

:- include(library(fibers/fibers_hooks)).

% ---------------------------------------------------------------------------
:- doc(section, "Current active fiber ID").

:- data curr_fid/1.

:- export(get_fid/1). % (internal)
get_fid(FID) :-
	( current_fact(curr_fid(FID0)) -> FID = FID0
	; throw(error(uninitialized_fiber_rt, get_fid/1))
	).

:- export(set_fid/1). % (internal)
set_fid(FID) :-
	retractall_fact(curr_fid(_)),
	asserta_fact(curr_fid(FID)).

:- export(get_fid_nothrow/1). % (internal) % (only for dist_log/1)
get_fid_nothrow(FID) :-
	( current_fact(curr_fid(FID0)) -> FID = FID0
	; fail
	).

% ---------------------------------------------------------------------------
:- doc(section, "(Conditionally) suspended fibers").

% fiber_condsusp(FID,Condsusp): Conditionally suspended fiber
:- data fiber_condsusp/2.

:- export(fiber_set_condsusp/2). % (internal)
fiber_set_condsusp(FID, Condsusp) :-
	retractall_fact(fiber_condsusp(FID, _)),
	assertz_fact(fiber_condsusp(FID, Condsusp)).

:- export(fiber_get_condsusp/2). % (internal)
fiber_get_condsusp(FID, Condsusp) :-
	( fiber_condsusp(FID, Condsusp0) -> Condsusp = Condsusp0
	; throw(error(no_condsusp(FID), fiber_get_condsusp/2))
	).

% ---------------------------------------------------------------------------
:- doc(section, "Fiber and IO scheduler").

% This is a task scheduler. Tasks can be fibers waiting for messages
% or stream handlers (including sockets).
%
% Tasks as picked by @pred{find_runnable/1} from:
%  - the ready stream queue (see @pred{wait_streams/1})
%  - the '$current_msg'/2 hook
%
% On idle state, the scheduler will block (see @tt{wait_streams(off)}).

:- data nested_sched/1. % stack for reentrant io_sched/1

% TODO: Support timeouts (see wait_streams/1), errors, etc.
% TODO: Add a version with concurrent/1 pred instead of streams

:- export(io_sched_init/0).
io_sched_init :-
	retractall_fact(nested_sched(_)),
	init_stream_watch.

:- export(io_sched/1).
io_sched(ExitArg) :-
	% TODO: for efficiency we could skip some polls
	wait_streams(0), % poll streams (non-blocking)
	io_sched1(ExitArg).

io_sched1(ExitArg) :-
	find_runnable(Runnable),
	( Runnable = idle -> io_sched_idle(ExitArg)
	; dispatch(Runnable, ExitArg)
	).

dispatch(handle_stream(Stream, StreamAttr), ExitArg) :- !,
	'$handle_stream'(StreamAttr, Stream),
	io_sched(ExitArg).
dispatch(wake_fiber(FID, Condsusp, Msg, Mode), ExitArg) :- !,
	set_fid(FID),
	( Mode = u ->
	    % Code at the io_sched_nested/2 continuation
	    ExitArg = Msg
	; % Mode = n
	  (Condsusp as gsusp).run(Msg),
	  io_sched(ExitArg)
	).

io_sched_idle(ExitArg) :-
	% We have no tasks to run. Do a blocking wait on streams.
	wait_streams(off), % wait for ready_stream items (blocking)
	( dried_streams ->
	    % 'deadlock' when no pending queries and no new messages can arrive
	    % TODO: signal and other events may prevent this
	    throw(deadlock)
	; io_sched1(ExitArg)
	).

% Find next runnable task (in priority order)
find_runnable(Runnable) :-
	% Task waiting on a stream
	current_ready_stream(Stream, StreamAttr, Ref),
	!,
	erase(Ref),
	Runnable = handle_stream(Stream, StreamAttr).
find_runnable(Runnable) :-
	% Task waiting on a message
	% TODO: optimize indexing
	'$current_msg'(FID, Msg, Ref),
	fiber_get_condsusp(FID, Condsusp),
	( is_top_sched(FID) -> TopSched = yes ; TopSched = no ),
	(Condsusp as gsusp).guard(TopSched, Msg, Mode),
	!,
	erase(Ref), % consume	
	Runnable = wake_fiber(FID, Condsusp, Msg, Mode).
find_runnable(Runnable) :- % No pending task... Idle
	Runnable = idle.

% the top running io_sched_nested/2 corresponds to FID
is_top_sched(FID) :-
	( current_fact(nested_sched(FID0)) -> true ; fail ),
	FID = FID0.

% ---------------------------------------------------------------------------

% :- export(io_sched_nested/2).
% Like io_sched/1 but considers success continuation as a blocked task determined by UpCond
% (this is reentrant)
io_sched_nested(FID, ExitArg) :-
	asserta_fact(nested_sched(FID)), % TODO: counter or FID?
	io_sched(ExitArg),
	( retract_fact(nested_sched(_)) -> true ; true ). % TODO: right?

:- export(fiber_wait/2).
:- meta_predicate fiber_wait(?, pred(1)).
fiber_wait(NewSusp, OnWake) :-
	get_fid(FID),
	fiber_get_condsusp(FID, PrevSusp),
	fiber_set_condsusp(FID, NewSusp),
	io_sched_nested(FID, ExitArg),
	Msg = ExitArg,
	fiber_set_condsusp(FID, PrevSusp),
	%
	OnWake(Msg).

% ---------------------------------------------------------------------------
:- doc(section, "Extended runtime meta-information").

:- include(library(fibers/fnct_rt)).

:- export(get_async_ftypes/2).
% Argument types for suspendable
get_async_ftypes(G, FTypes) :-
	fnct_stub(G, G2),
	( (G2 as async).ftypes(FTypes0) -> FTypes = FTypes0
	; functor(G, F, A),
	  throw(no_ftypes_for_goal(F, A))
	).

% ---------------------------------------------------------------------------
:- doc(section, "Fiber manipulation").

% Obtain suspensions (including a continuation goal and transient state)
% for the current actI

% :- use_module(engine(messages_basic), [message/2]).
:- use_module(library(fibers/fibers_data)).

% TODO: support incremental suspensions (save diff of states; see persistent preds)

:- export('$fiber_susp'/2).
% Obtain a suspension of the current fiber with continuation G
'$fiber_susp'(X) := _ :- var(X), !, throw(error(instantiation, '$fiber_susp'/2)).
'$fiber_susp'(G) := PA :- '$fiber_susp_hook'(G, PA0), !, PA = PA0.
'$fiber_susp'(G) := PA :-
%	actmod_get_self_mod(DMod),
%	( DMod2 = ~fnct_mod(G), fnct_property(G, async),
%	  '$local_actmod'(DMod2),
%	  \+ DMod = DMod2 -> % TODO: why?
%	    message(warning, ['suspending non-current active module instance ', DMod2, ' (from ', DMod, ')'])
%	; true
%	),
	DMod = ~fnct_mod(G), fnct_property(G, async), % TODO:T253 see commented code above
	t_data_dump(State), % TODO:T253 use ActRef/FID here!!!
        FiberData = fiberData(DMod,State),
	PA = '$fiberSusp'(FiberData, G).

:- export('$fiber_susp_spawn'/2).
% Obtain a suspension for a new fiber spawning a new active module
% instance (given by G) that continues execution with G
'$fiber_susp_spawn'(X) := _ :- var(X), !, throw(error(instantiation, '$fiber_susp_spawn'/2)).
'$fiber_susp_spawn'(G) := PA :-
	DMod = ~fnct_mod(G), fnct_property(G, async),
	% ( \+ '$local_actmod'(DMod) -> % NOTE: Not a problem, props in stub
	%     message(warning, ['cannot spawn a non-active module ', DMod])
	% ; true
	% ),
	t_fresh_data(State),
	FiberData = fiberData(DMod,State),
	PA = '$fiberSusp'(FiberData, G).

% (Fresh transient data)
t_fresh_data([]).

