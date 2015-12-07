:- module(jtopl,
	[prolog_server/0,
	 prolog_server/1,
	 prolog_server/2,
	 shell_s/0
	],
	[assertions,regtypes,isomodes]).

:- doc(title,"Java to Prolog interface").

:- doc(author,"Jes@'{u}s Correas").

:- doc(module, "
@cindex{Java to Prolog interface}
This module defines the Prolog side of the Java to Prolog interface. This side
of the interface only has one public predicate: a server
that listens at the socket connection with Java, and executes the commands
received from the Java side.

In order to evaluate the goals received from the Java side, this module can
work in two ways: executing them in the same engine, or starting a thread
for each goal. The easiest way is to launch them in the same engine, but
the goals must be evaluated sequentially: once a goal provides the first
solution, all the subsequent goals must be finished before this goal can
backtrack to provide another solution. The Prolog side of this interface
works as a top-level, and the goals partially evaluated are not
independent.

The solution of this goal dependence is to evaluate the goals in a
different prolog engine. Although Ciao includes a mechanism to evaluate
goals in different engines, the approach used in this interface is to
launch each goal in a different thread.

The decision of what kind of goal evaluation is selected is done by the
Java side. Each evaluation type has its own command terms, so the Java side
can choose the type it needs.

A Prolog server starts by calling the @tt{prolog_server/0} predicate,
or by calling @tt{prolog_server/1} predicate and providing the port
number as argument. The user predicates and libraries to be called
from Java must be included in the executable file, or be accesible
using the built-in predicates dealing with code loading.

").

:- use_module(library(concurrency)).
:- use_module(library(system)).
:- use_module(library(read), [read/2]).
:- use_module(library(compiler), [use_module/1]).
:- use_module(library(javall/javasock), 
	[
	    start_socket_interface/2,
	    bind_socket_interface/1,
	    join_socket_interface/0,
	    prolog_query/2,
	    prolog_response/2,
	    java_debug/1,
	    java_debug_redo/1,
	    start_threads/0
	]).

%%------------------------------------------------------------------
%% Data predicates.
%%------------------------------------------------------------------
% Contains the exception thrown when launching a goal
:- concurrent exception_flag/2.

% Contains the solutions generated from the Prolog goals that
% are waiting to be requested from the Java side. Once the solution
% is obtained by the corresponding thread, it is not automatically
% returned to the other side of the interface. The solution is stored
% in this fact, and is sent to the Java side when a next_solution
% request is received.
:- concurrent query_solutions/2.

% Contains the requests for next solution/termination.
% When the Java side requests next solution or the termination of
% a given goal, the interface asserts a fact with its query Id, in
% order to allow the thread that is processing the goal can continue
% executing it to get the next solution, or just terminate
% its execution.
% Second argument must be the atoms 'next_solution' or 'terminate'.
:- concurrent query_requests/2.

% Contains the list of queries launched from Java and no terminated
:- concurrent running_queries/2.

:- export(query_solutions/2).
:- export(query_requests/2).
:- export(running_queries/2).

%%------------------------------------------------------------------
%% Documentation.
%%------------------------------------------------------------------
% Currently there is no access to internal predicates documentation. 

%%------------------------------------------------------------------
%% REGTYPES
%%------------------------------------------------------------------
:- regtype command(X) # "@var{X} is a command received from the java
	client, to be executed by the Prolog process. The command is
	represented as an atom or a functor with arity 1. The command to be
	executed must be one of the following types:
@cindex{Java commands}
@begin{itemize}

@item @tt{prolog_launch_query(Q)} Compound term to create a new query,
	received as single argument of this structure. A reference to the
	new query is returned to Java.

@item @tt{prolog_next_solution} Atom to get the next solution
	of a goal. A term representing the goal instantiated with the 
        next solution is returned to Java.

@item @tt{prolog_execute} Atom to indicate that next solution of a
	goal must be got, without blocking the requester (it has to check 
        if this goal is still running using prolog_is_running command).

@item @tt{prolog_terminate_query} Atom to indicate that a goal must be 
        terminated.

@item @tt{prolog_use_module(M)} Compound term to load dynamically a 
        module given as argument.

@item @tt{prolog_is_running} Atom to check if a goal is yet running a
        prolog_execute command.

@item @tt{prolog_halt} Atom to terminate the current Prolog process.

@end{itemize}
".

command(prolog_launch_query(Query)) :-
	callable(Query).

command(prolog_launch_query_on_thread(Query)) :-
	callable(Query).
command(prolog_next_solution(_)).
command(prolog_terminate_query(_)).
command(prolog_halt).

:- regtype answer(X) # "@var{X} is a response sent from the prolog
	server. Is represented as an atom or a functor with arity 1 or 2,
	depending on the functor name.
@cindex{Prolog answers}

".

answer(prolog_success).  
answer(prolog_fail).
answer(prolog_still_running).
answer(prolog_solution(X)) :- nonvar(X).  
answer(prolog_query_id(X)) :- nonvar(X).
answer(prolog_exception(X)) :- nonvar(X).
answer(prolog_exception(X,Y)) :- int(X), nonvar(Y).

:- regtype prolog_query_id(X) # "@var{X} is a prolog query identifier.".
prolog_query_id(prolog_query_id(X)) :- nonvar(X).

%----------------------------------------------------------------------------
:- pred prolog_server/0
	# "Prolog server entry point. Reads from the standard input
	  the node name and port number where the java client resides,
	  and starts the prolog server listening at the jp
	  socket. This predicate acts as a server: it includes an
	  endless read-process loop until the @tt{prolog_halt} command
	  is received.  

          However, from the low-level communication point of view,
	  this Prolog server actually works as a client of the Java
	  side. This means that Java side waits at the given port to a
	  Prolog server trying to create a socket; Prolog side
	  connects to that port, and then waits for Java requests
	  (acting as a 'logical' server). To use this Prolog server as
	  a real server waiting for connections at a given port, use
	  @tt{prolog_server/1}.

@cindex{Prolog server}
".
%----------------------------------------------------------------------------
prolog_server :-
	current_host(Node),
	get_port(user_input,Port),
	start_socket_interface(Node:Port,_),
	join_socket_interface,
	eng_killothers.

%----------------------------------------------------------------------------
:- pred prolog_server/1
	:: atm
	# "Waits for incoming Java connections to act as a Prolog goal
          server for Java requests.This is the only
          @tt{prolog_server/*} predicate that works as a true server:
          given a port number, waits for a connection from Java and
          then serves Java requests. When a termination request is
          received, finishes the connection to Java and waits next
          Java connection request. This behaviour is different with
          respect to previous versions of this library. To work as
          before, use @tt{prolog_server/2}.

          Although it currently does not support simultaneous Java
          connections, some work is being done in that direction.
@cindex{Prolog server}
".
%----------------------------------------------------------------------------
prolog_server(Port) :-
	int(Port),
	bind_socket_interface(Port),
	join_socket_interface,
%	eng_killothers,
	!,
	prolog_server_.

prolog_server_:-
	start_threads,
	join_socket_interface,
	!,
	prolog_server_.

%----------------------------------------------------------------------------
:- pred prolog_server/2
	:: atm * atm
	# "Prolog server entry point. Given a network @tt{node} and a
	  @tt{port} number, starts the prolog server trying to connect
	  to Java side at that @tt{node:port} address, and then
	  waits for Java requests. This predicate acts as a server:
	  it includes an endless read-process loop until the
	  @tt{prolog_halt} command is received. 

          However, from the low-level communication point of view,
	  this Prolog server actually works as a client of the Java
	  side. This means that Java side waits at the given port to a
	  Prolog server trying to create a socket; Prolog side
	  connects to that port, and then waits for Java requests
	  (acting as a 'logical' server). To use this Prolog server as
	  a real server waiting for connections at a given port, use
	  @tt{prolog_server/1}.
@cindex{Prolog server}
".
%----------------------------------------------------------------------------
prolog_server(Node,Port) :-
	int(Port),
	atom(Node),
	start_socket_interface(Node:Port,_),
	join_socket_interface,
	eng_killothers.

%% -----------------------------------------------------------------------
:- pred get_port(+Stream,-Port)
	:: atom * atom # "Gets the port number to connect to Java
	server, reading it from the stream received as argument.".
%% -----------------------------------------------------------------------

get_port(Stream,Port):-
        current_input(CU),
        set_input(Stream),
        read(Stream, Port),
        set_input(CU).

%----------------------------------------------------------------------------
:- pred shell_s/0 # "Command execution loop. This predicate is called
	when the connection to Java is established, and performs an endless 
        loop processing the commands received. This predicate is only intended
        to be used by the Prolog to Java interface and it should not be used
        by a user program.".
%----------------------------------------------------------------------------
shell_s :- 
	read_command(Id,Command), % Gives commands on backtracking.
	(termination_check(Command) -> 
	 true 
	; 
	    process_command(Id,Command), !,
	    %% Avoid choicepoints (none should have been pushed--just in case)
	shell_s ), !.
shell_s :-
	%% The previous command has failed, a general exception is
        %% thrown and main loop is restarted.
	write_answer(0,prolog_exception(jtopl('Read or process failure.'))),
	!,  %% Avoid choicepoints (none should have been pushed--just in case)
	shell_s.

%---------------------------------------------------------------------------
:- pred process_command(+Id,+Command) 
	:: prolog_query_id * command 
        # "Processes the first command of a query. Using the threads
        option, it processes all the commands received from the prolog
        server.".
%---------------------------------------------------------------------------
process_command(Id,prolog_is_running) :-
	%% Checks if the query received as argument is still running,
        %% or there are solutions not requested from Java.
        %% case a: Query is still running
        current_fact_nb(running_queries(Id, _Q)),
	write_answer(Id,prolog_success),
	!.
	
process_command(Id,prolog_is_running) :-
	%% Checks if the query received as argument is still running,
        %% or there are solutions not requested from Java.
        %% case b: Query is not running, but there are solutions
        %% to send to Java
	current_fact_nb(query_solutions(Id,_S)),
	write_answer(Id,prolog_success),
	!.
	
process_command(Id,prolog_is_running) :-
	%% Checks if the query received as argument is still running,
        %% or there are solutions not requested from Java.
        %% case c: Query is not running nor solutions pending.
	write_answer(Id,prolog_fail),
	!.

process_command(JId,prolog_launch_query(Query)) :-
	%% launches a goal on a separate thread.
        %% Further commands are processed by this predicate
        %% JId is the Java Id needed to work with unique Ids
        %% before Prolog assigns its own Id.
	eng_call(solve(Query,JId), create, create),
	!.

process_command(Id,prolog_execute) :-
	%% Requests the execution of a goal on a separate 
        %% thread, and returns immediately (it does not wait until
        %% the execution is finished.
        %% NOTE: The goal must be already launched.
        %% case a: The query is running.
        current_fact_nb(running_queries(Id, _)),
        assertz_fact(query_requests(Id,next_solution)),
	assertz_fact(query_requests(Id,terminate)),
	write_answer(Id,prolog_success),
	retractall_fact(query_solutions(Id, _)),
	!.

process_command(Id,prolog_execute) :-
	%% Requests the execution of a goal on a separate
        %% thread, and returns immediately.
	%% Case b: The query is terminated.
        retractall_fact(query_requests(Id,_)),
	write_answer(Id,prolog_fail),
	retractall_fact(query_solutions(Id,_)),
	!.

process_command(Id,prolog_next_solution) :-
	%% Requests the next solution of the query to the
        %% thread, and returns the solution.
        %% case a: The query is running.
        current_fact_nb(running_queries(Id, _)),
        assertz_fact(query_requests(Id,next_solution)),
	retract_fact(query_solutions(Id,Solution)),
	(Solution = prolog_fail ->
	 write_answer(Id,prolog_fail) % There are no more solutions
	;
	 write_answer(Id,prolog_solution(Solution)) % Next solution
	),
	!.

process_command(Id,prolog_next_solution) :-
	%% Gets and returns the next solution of the query
	%% Case b: The query is terminated.
        retractall_fact(query_requests(Id,_)),
	write_answer(Id,prolog_fail),
	!.

process_command(Id,prolog_terminate_query) :-
	%% Terminates the query given as argument.
	%% Case a: the query is still running.
        current_fact_nb(running_queries(Id, _)),
        assertz_fact(query_requests(Id,terminate)),
	retractall_fact(query_solutions(Id, _)),
	write_answer(Id,prolog_success),
	!.

process_command(Id,prolog_terminate_query) :-
	%% Terminates the query given as argument.
	%% Case b: The query is not running.
	write_answer(Id,prolog_success),
	!.

process_command(Id,prolog_use_module(Module)) :-
	%% Loads a module on this thread.
        catch(use_module(Module), Error, write_answer(Id,prolog_exception(0,jtopl(Error)))),
	(var(Error) -> write_answer(Id,prolog_success); true),
	!.

process_command(_Id,internal_use_module(Module)) :-
	%% Loads a module on this thread. This command
        %% is received internally from the Prolog-to-Java side.
        catch(use_module(Module), _Error, true),
	!.

process_command(Id,Command) :-
	%% Any other command throws an exception in client side.
        write_answer(Id,prolog_exception(jtopl('unexpected command', Command ))),
	!.

%---------------------------------------------------------------------------
:- pred solve(+Query, +JId)
        :: callable * term
        # "Runs the query on a separate thread and stores the solutions 
           on the @tt{query_solutions/2} data predicate.".
%---------------------------------------------------------------------------
solve(Query,JId) :-
	thread_id(Id),
	asserta_fact(running_queries(Id, Query)),
	write_answer(JId,prolog_query_id(Id)),
	next_request(Id,next_solution), % requesting next solution
	solve2(Query,Id),
        !.  %% Avoid choicepoints (none should have been pushed---just in case)

solve(Query,_JId) :-
	% No solutions requested: goal just launched and terminated.
        thread_id(Id),
	retract_fact_nb(running_queries(Id, Query)).

solve2(Query,Id) :-
%% Oops! Query launching should be intercepted, but some strange 
%%       behaviours prevent from using intercept/3.
%        intercept(Query, Error, assertz_fact(exception_flag(Id, Error))),
	java_debug_redo(solve2(Query,Id)),
	Query,
	java_debug_redo(solve2(Query,Id)),
%%
	(current_fact_nb(exception_flag(Id, Error)) ->
	 assertz_fact(query_solutions(Id, prolog_exception(Id, Error))),
	 retract_fact(exception_flag(Id, _))
	;
	 assertz_fact(query_solutions(Id, Query))
	),
%%%%	check_solution(Query,Id,Error),
	java_debug_redo('before next_request'(Query)),
	next_request(Id,T), % if next solution requested just fails.
	java_debug_redo('after next_request'(Query)),
	T = terminate,
	retract_fact_nb(running_queries(Id, Query)).

solve2(Query,Id) :-
	java_debug('solve2 failing'),
	% No more solutions. Query is terminated.
	assertz_fact(query_solutions(Id, prolog_fail)),
	retract_fact(running_queries(Id, Query)).

next_request(Id,Req):-
	retract_fact(query_requests(Id,Req)),
	!.

/*
check_solution(Query,Id,Error) :-
	(current_fact_nb(exception_flag(Id, Error)) ->
	 assertz_fact(query_solutions(Id, prolog_exception(Id, Error))),
	 retract_fact(exception_flag(Id, _))
	;
	 assertz_fact(query_solutions(Id, Query))
	),
	!.
*/
%---------------------------------------------------------------------------
:- use_module(library(read_from_string), [read_from_string_atmvars/2]).
:- pred prolog_parse(+String, -Term)
        :: string * term
        # "Parses the string received as first argument and returns
	   the prolog term as second argument.
           @bf{Important:} This is a private predicate but could be called
           from java side, to parse strings to Prolog terms.".
%---------------------------------------------------------------------------
prolog_parse(S,Term) :-
	read_from_string_atmvars(S, Term).

%---------------------------------------------------------------------------
:- pred write_answer(+Id,+Answer)
	:: prolog_query_id * answer
        # "writes to the output socket stream the given answer.".
%---------------------------------------------------------------------------
write_answer(Id,Answer) :-
	assertz_fact(prolog_response(Id,Answer)).

%---------------------------------------------------------------------------
:- pred read_command(-Id,-Command)
	:: prolog_query_id * command
        # "Reads from the input stream a new prolog server command.".
%---------------------------------------------------------------------------
read_command(Id,Command) :-
	retract_fact(prolog_query(Id,Command)).

% -----------------------------------------------------------------------
:- pred termination_check(+Term)
	:: atm # "Checks if the termination atom is received.".
% -----------------------------------------------------------------------
termination_check('$terminate').
termination_check('$disconnect').

% -----------------------------------------------------------------------
:- pred thread_id(-Term)
	:: term # "Gets an unique id for the current thread.".
% -----------------------------------------------------------------------
thread_id(Id) :-
	eng_goal_id(Id).
