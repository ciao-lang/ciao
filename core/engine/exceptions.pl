:- module(exceptions, [catch/3, intercept/3, throw/1, send_signal/1,
	    send_silent_signal/1, halt/0, halt/1, abort/0],
	    [assertions, nortchecks, isomodes, datafacts]).

:- doc(title, "Exception and signal handling").

:- doc(author, "The Ciao Development Team").
:- doc(author, "Jose F. Morales (global vars version)").

:- doc(usage, "@include{InPrelude.lpdoc}").

:- doc(module, "This module includes predicates related to
   exceptions and signals, which alter the normal flow of Prolog.").

:- use_module(engine(basiccontrol), ['$metachoice'/1, '$metacut'/1]).
:- use_module(engine(hiord_rt), ['$meta_call'/1]).
:- use_module(engine(internals), ['$global_vars_get'/2, '$global_vars_set'/2]).

% ---------------------------------------------------------------------------

:- use_module(engine(internals), ['$exit'/1]).

:- pred halt => true + (iso, native).

:- doc(halt, "Halt the system, exiting to the invoking shell.").

halt :- '$exit'(0).

:- pred halt(+int) => int + (iso, equiv(fail)).
% TODO: equiv(fail) temporary until we implement more accurate builtin

:- doc(halt(Code), "Halt the system, exiting to the invoking shell,
   returning exit code @var{Code}.").

halt(E) :- integer(E), !, '$exit'(E).
halt(V) :- var(V), !, throw(error(instantiation_error, halt/1-1)).
halt(N) :- throw(error(type_error(integer, N), halt/1-1)).

:- doc(abort, "Abort the current execution.").

abort :-
	reset_error,
	'$exit'(-32768).

% ---------------------------------------------------------------------------

% TODO: thread-local predicate or non-backtrackable copy would
% be faster (do not require any locks)
:- concurrent thrown/2.

reset_error :-
	% (just in case thrown/2 contains garbage)
	eng_id(EngId),
	retractall_fact(thrown(EngId,_)).

send_error(Error) :-
	eng_id(EngId),
	asserta_fact(thrown(EngId,Error)).

recv_error(Error) :-
	eng_id(EngId),
	retract_fact_nb(thrown(EngId,Error0)), !,
	Error = Error0.

% (weak import, use predicate only if concurrency.pl loaded)
:- import(concurrency, ['$eng_self'/2]).
:- use_module(engine(internals), ['$predicate_property'/3]).

eng_id(EngId) :-
	( '$predicate_property'('concurrency:$eng_self'(_,_),_,_) ->
	    % Use GoalDesc (integer encoding an internal pointer) as
	    % thread id.
	    '$eng_self'(EngId, _)
	; EngId = 0 % (this is safe here due to lifetime of thrown/2 facts)
	).

% ---------------------------------------------------------------------------

:- primitive_meta_predicate(catch(goal, ?, goal)).

:- trust pred catch(+callable, ?term, ?callable) + (iso, native).

:- doc(catch(Goal, Error, Handler), "Executes @var{Goal}.  If an
   exception is raised during its execution, @var{Error} is unified with
   the exception, and if the unification succeeds, the entire execution
   derived from @var{Goal} is aborted, and @var{Handler} is executed.
   The execution resumes with the continuation of the @tt{catch/3} call.  For
   example, given the code
@begin{verbatim}
p(X) :- throw(error), display('---').
p(X) :- display(X).
@end{verbatim}
   the execution of ""@tt{catch(p(0), E, display(E)), display(.), fail.}""
   results in the output ""@tt{error.}"".").

catch(Goal, Error, _) :-
	'$metachoice'(Choice),
	'$global_vars_get'(7,PrevStack),
	'$global_vars_set'(7,catching_frame(Choice,Error,PrevStack)),
	'$meta_call'(Goal),
	'$metachoice'(AfterChoice),
	'$global_vars_set'(7,PrevStack),
	( Choice = AfterChoice -> % no more solutions
	    ! % remove the unnecessary exception choice point
	; true
	).
catch(_, Error, Handler) :-
	% receive error term (see throw/1)
	recv_error(Error),
	'$meta_call'(Handler).

% ---------------------------------------------------------------------------

:- trust pred throw(Term) : nonvar(Term) + (iso, equiv(fail)).
% TODO: equiv(fail) temporary until we implement more accurate builtin

:- doc(throw(Ball), "Raises an error, throwing the exception
   @var{Ball}, to be caught by an ancestor @pred{catch/3}.  The
   closest matching ancestor is chosen.  In addition to calls to
   @pred{throw/2} in user code, exceptions are also thrown by many
   library predicates in cases of error.").

throw(Error) :-
	var(Error), !,
	throw(error(instantiation_error, throw/1 -1)).
throw(Error) :-
	'$global_vars_get'(7,Stack),
	match_catching_frame(Stack, Error, Chpt, Prev),
	!,
	'$global_vars_set'(7,Prev), % unwind catching frames
	% send error term (see catch/1)
	send_error(Error),
	% cut to Chpt and fail (call the handler)
	'$metacut'(Chpt),
	fail.
throw(Error) :-
	no_handler(Error).

match_catching_frame(catching_frame(Chpt0,E0,Prev0), E, Chpt, Prev) :-
	( E = E0 -> % TODO: unify? instance? \+ \+?
	    Chpt = Chpt0,
	    Prev = Prev0
	; match_catching_frame(Prev0, E, Chpt, Prev)
	).

% ---------------------------------------------------------------------------

:- use_module(engine(messages_basic), [message/2]).
:- use_module(engine(io_basic), [display/2]).

no_handler(Error) :-
	display(user_error, '{'),
	message(error, ['No handle found for thrown error ', ~~(Error), '}']),
	abort.

% ===========================================================================

:- use_module(engine(internals), ['$setarg'/4]).

:- primitive_meta_predicate(intercept(goal, ?, goal)).

:- pred intercept(+callable, ?term, +callable).

:- doc(intercept(Goal, Signal, Handler), "Executes @var{Goal}.  If
   a signal is sent during its execution, @var{Signal} is unified with
   the exception, and if the unification succeeds, @var{Handler} is
   executed and then the execution resumes after the point where the
   exception was thrown.  To avoid infinite loops if @var{Handler}
   raises an exception which unifies with @var{Error}, the exception
   handler is deactivated before executing @var{Handler}.  Note the
   difference with builtin @pred{catch/3}, given the code 
@begin{verbatim}
p(X) :- send_signal(error), display('---').
p(X) :- display(X).
@end{verbatim}
   the execution of ""@tt{intercept(p(0), E, display(E)),
   display(.), fail.}"" results in the output ""@tt{error---.0.}"".").

intercept(Goal, Signal, Handler) :-
	'$global_vars_get'(8,PrevStack),
	'$global_vars_set'(8,signal_frame(Signal,Handler,inactive,PrevStack)),
	'$meta_call'(Goal),
	'$global_vars_set'(8,PrevStack).

% ---------------------------------------------------------------------------

:- trust pred send_signal(Term) : nonvar(Term).

:- doc(send_signal(Signal), "Emits a signal, to be intercepted by an
   ancestor @pred{intercept/3}. The closest matching ancestor is
   chosen. If the signal is not intercepted, the following error is
   thrown: @tt{error(unintercepted_signal(Signal),
   send_signal/1-1)}.").

send_signal(Signal):-
	( send_signal_(Signal, true) ->
	    true
	; throw(error(unintercepted_signal(Signal), 'exceptions:send_signal'/1 -1))
	).

:- trust pred send_silent_signal(Term) : nonvar(Term).

:- doc(send_silent_signal(Signal), "Emits a signal as
   @pred{send_signal/1}, but does not throw an error if the signal is
   not intercepted (i.e. just suceeds silently)").

send_silent_signal(Signal) :-
	send_signal_(Signal, _).

send_signal_(Signal, _) :-
	var(Signal), !,
	throw(error(instantiation_error, 'exceptions:send_signal'/1 -1)).
send_signal_(Signal, _) :-
	'$global_vars_get'(8,Stack),
	match_signal_frame(Stack, Signal, SignalFrame),
	SignalFrame = signal_frame(S,H,_,_),
	'$setarg'(3, SignalFrame, active, on), % Mark as active
	!,
	copy_term((S,H), (Signal,Handler)),
	'$meta_call'(Handler),
	'$setarg'(3, SignalFrame, inactive, on). % Mark again as inactive
send_signal_(_Signal, false).

% TODO: linear search; replace with named global variables?
match_signal_frame(Stack, Signal, SignalFrame) :-
	Stack = signal_frame(Signal0,_,Running,Prev),
	( Running = inactive, \+ \+ Signal = Signal0 ->
	    SignalFrame = Stack
	; match_signal_frame(Prev, Signal, SignalFrame)
	).

% ---------------------------------------------------------------------------
% TODO: move somewhere else

% :- test intercept(G, S, H) : ( G=((A=a;A=b), send_signal(c(A))),
% 	    S=c(A), H=display(A) ) + (not_fails, non_det)
% # "intercept/3 preserves determinism properties of Goal (even when
%    @var{H} and @var{G} share variables)".

