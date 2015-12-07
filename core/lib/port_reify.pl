:- module(_, [], [assertions, regtypes, isomodes, hiord]).

:- doc(title, "Call goals with reified (exit) ports.").
:- doc(author, "Jose F. Morales").

:- doc(module, "Predicates to delay the execution of the (exit) port
   of a goal. This is useful to introduce side-effects at selected
   points (e.g., clean-ups before goal exit in failure or exception
   conditions).").

:- doc(bug, "Improve name, move elsewhere?").

% ---------------------------------------------------------------------------

:- export(exit_port/1).
:- regtype exit_port(Result)
   # "Exit port of a goal execution, defined as:
   @begin{itemize}
   @item @tt{success}: goal succeeded
   @item @tt{failure}: goal failed
   @item @tt{exception(E)}: goal threw an exception @var{E}
   @end{itemize}".

exit_port(success).
exit_port(failure).
exit_port(exception(_)).

% ---------------------------------------------------------------------------

:- export(once_port_reify/2).
:- meta_predicate once_port_reify(goal, ?).
:- pred once_port_reify(Goal, Result) => exit_port(Result)
   # "Execute @term{once(Goal)} (alternatives are cut) and obtain its
      execution port @var{Port}, which can be continued with
      @pred{port_call/1}.

      The sequence
      @term{(once_port_reify(Goal,P),Cleanup,port_call(P))} is
      semantically equivalent to @term{once(Goal)}, but executes
      @var{Cleanup) goals in case of success, failure, and
      exception.".

once_port_reify(Goal, Result) :-
	catch(once_port_reify_(Goal, Result), E,
	      Result = exception(E)).

once_port_reify_(Goal, Result) :-
	( call(Goal) ->
	    Result = success
	; Result = failure
	).

% ---------------------------------------------------------------------------

:- export(port_call/1).
:- pred port_call(Result) : exit_port(Result)
   # "Succeed, fail, or throw the exception from @var{Result} (see
     @pred{once_port_reify/2}).".

port_call(success).
port_call(failure) :- fail.
port_call(exception(E)) :- throw(E).

