:- module(exceptions, [catch/3, intercept/3, throw/1, send_signal/1,
	    send_silent_signal/1, halt/0, halt/1, abort/0],
	    [assertions, nortchecks, isomodes]).

:- use_module(engine(internals),    ['$exit'/1]).
:- use_module(engine(basiccontrol), ['$metachoice'/1, '$metacut'/1]).
:- use_module(engine(hiord_rt),     ['$meta_call'/1]).
:- use_module(engine(exceptions_db)).

:- doc(title, "Exception and Signal handling").

:- doc(author, "The CLIP Group").

:- doc(usage, "These predicates are builtin in Ciao, so nothing special
   has to be done to use them.").

:- doc(module, "This module includes predicates related to
   exceptions and signals, which alter the normal flow of Prolog.").

:- primitive_meta_predicate(catch(goal,     ?, goal)).
:- primitive_meta_predicate(intercept(goal, ?, goal)).

:- pred halt => true + (iso, native).

:- doc(halt, "Halt the system, exiting to the invoking shell.").

halt :- '$exit'(0).


:- pred halt(+int) => int + iso.

:- doc(halt(Code), "Halt the system, exiting to the invoking shell,
   returning exit code @var{Code}.").

halt(E) :- integer(E), !, '$exit'(E).
halt(V) :- var(V), !, throw(error(instantiation_error, halt/1-1)).
halt(N) :- throw(error(type_error(integer, N), halt/1-1)).

:- doc(abort, "Abort the current execution.").

abort :- '$exit'(-32768).

%------ errors ------%

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
	asserta_catching(Choice, Error, '$catching'),
	'$metachoice'(BeforeChoice),
	'$meta_call'(Goal),
	'$metachoice'(AfterChoice),
	retract_catching(Choice, Error, '$catching'),
	( BeforeChoice = AfterChoice -> % no more solutions
	    ! % remove the unnecessary exception choice point
	; true
	).
catch(_, Error, Handler) :-
	retract_fact_nb(thrown(Error)), !,
	'$meta_call'(Handler).


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

:- test intercept(G, S, H) : ( G=((A=a;A=b), send_signal(c(A))),
	    S=c(A), H=display(A) ) + (not_fails, non_det)
# "intercept/3 preserves determinism properties of Goal (even when
   @var{H} and @var{G} share variables)".

intercept(Goal, Signal, Handler) :-
	'$metachoice'(Choice),
	asserta_catching(Choice, Signal, Handler),
	current_fact(catching(Choice, S, H)), % avoid unifs with Goal
	'$metachoice'(BeforeChoice),
	'$meta_call'(Goal),
	'$metachoice'(AfterChoice),
	retract_catching(Choice, S, H),
	( BeforeChoice = AfterChoice -> % no more solutions
	    ! % remove the unnecessary exception choice point
	; true
	).

:- trust pred throw(Term) : nonvar(Term) + iso.

:- doc(throw(Ball), "Raises an error, throwing the exception
   @var{Ball}, to be caught by an ancestor @pred{catch/3}.  The
   closest matching ancestor is chosen.  In addition to calls to
   @pred{throw/2} in user code, exceptions are also thrown by many
   library predicates in cases of error.").

throw(Error) :-
	var(Error), !,
	throw(error(instantiation_error, throw/1 -1)).
throw(Error) :-
	current_fact(catching(C, E, '$catching'), Ref),
	\+ current_fact_nb(disabled(Ref)),
	E = Error, !,
	throw_action('$catching', E, C, Ref).
throw(Error) :-
	display(user_error, '{'),
	message(error, ['No handle found for thrown error ', ~~(Error), '}']),
	retractall_fact(catching(_, _, _)),
	retractall_fact(disabled(_)),
	abort.


send_signal2(Signal, _) :-
	var(Signal), !,
	throw(error(instantiation_error, signal/1 -1)).
send_signal2(Signal, _) :-
	current_fact(catching(C, E, H), Ref),
	H \== '$catching',
	\+ current_fact_nb(disabled(Ref)),
	E = Signal, !,
	throw_action(H, E, C, Ref).
send_signal2(_Signal, false).

:- trust pred send_signal(Term) : nonvar(Term).

:- doc(send_signal(Signal), "Emits a signal, to be intercepted by an
   ancestor @pred{intercept/3}. The closest matching ancestor is
   chosen. If the signal is not intercepted, the following error is
   thrown: @tt{error(unintercepted_signal(Signal),
   send_signal/1-1)}.").


send_signal(Signal):-
	(
	    send_signal2(Signal, true) ->
	    true
	;
	    throw(error(unintercepted_signal(instantiation_error), signal/1 -1))
	).


:- trust pred send_silent_signal(Term) : nonvar(Term).

:- doc(send_silent_signal(Signal), "Emits a signal as
   @pred{send_signal/1}, but does not throw an error if the signal is
   not intercepted (i.e. just suceeds silently)").

send_silent_signal(Signal) :-
	send_signal2(Signal, _).

throw_action('$catching', Error, Choice, _) :-
	asserta_fact(thrown(Error)),
	cut_to(Choice), % This cuts also next clause
	fail.
throw_action(Handler, _, _, Ref) :-
	asserta_disabled(Ref),
	'$metachoice'(BeforeChoice),
	'$meta_call'(Handler),
	'$metachoice'(AfterChoice),
	retract_disabled(Ref),
	(BeforeChoice = AfterChoice -> ! ; true).

cut_to(Choice) :-
	current_fact(catching(C, _, _), Ref),
	erase(Ref),
	retractall_fact(disabled(Ref)),
	C = Choice,
	'$metacut'(Choice).
