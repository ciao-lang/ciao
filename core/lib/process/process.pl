:- module(_, [], [assertions, regtypes, isomodes, hiord, dcg]).

:- doc(title, "Processes (multitasking)").

:- doc(author, "Jose F. Morales").

:- doc(module, "This library offers predicates to create, communicate,
   and synchronize with child processes. The child processes are
   duplicates of the parent process, based on the @tt{fork()} function
   from POSIX-compatible operating systems (see @tt{man} page for a
   precise description of the non-inherited process attributes).

   The child process can execute a given goal (in a clone of the
   parent process) or start an external program.

   Contrary to threads, processes have a separate address space and
   communication must be performed via inter-process communication
   mechanisms. This is useful for executing external programs, or
   implementing coarse-grained parallelism and concurrency.

   @begin{alert}
   @begin{itemize}
   @item Process creation via @tt{fork()} is relatively costly. Use only
     when address separation is necessary. Consider other concurrency
     primitives otherwise.
   @item Channels connected to in-memory terms are transmited via pipes
     or temporary files (when needed to avoid deadlock problems).
   @item Deadlock problems may still appear if the user specifies two
     or more @tt{pipe(_)} channels for the same process (data must be
     send/received concurrently).
   @item The current implementation is not protected against Prolog
     signals (it can leak resources, including zombie processes, if
     interrupted by signals).
   @item Communication is currently only supported via file system, file
     descriptors, and sockets.
   @item Other mechanisms like file locks, semaphores, message queues,
     shared memory, etc. are not yet implemented.
   @item Arguments do not accept wildcards. See predicates in
     @lib{library(glob)} for glob expansions (shell wildcard
     patterns).
   @end{itemize}
   @end{alert}
").

:- doc(bug, "(feature) Poll if a process has terminated").

:- doc(bug, "(feature) Support more IPC primitives (file locks,
   semaphores, shared memory, etc.).").

:- doc(bug, "(feature) Fix asynchronous data transmission using POSIX
   @tt{select}? We may avoid temporary files then.").

:- doc(bug, "(feature) Complete support for daemons (see example)").
%
%  Example of daemon (missing: close parent IO, change dir, etc.)
%  
%  ?- process_fork(
%       (open('/tmp/ciaodaemon2.txt', append, S),
%        ( repeat,
%          pause(1),
%          write(S, 'foo\n'),
%          fail
%        ; true
%        )), [setsid, background(_)]).
%  
%  With @tt{setsid} the process will continue running even if the
%  parent process ends.
%

:- doc(bug, "(feature) Put together with other concurrency and
   parallelism libraries in Ciao").
% 
%   - connect streams to concurrent facts?
%   - implement shared memory?
%   - parallel aggregates?
%

:- doc(bug, "(performance) Consider using posix_spawn for exec
   (faster, works in systems without MMU).").

:- doc(bug, "(performance) Write benchmarks (see internal documentation)").
%
%  Performance (walltime) of process_call seems to be dominated by
%  forking, not replacing the current process. Example:
%
%    % Executing /usr/bin/true
%    ?- statistics(walltime,_),
%       (between(1,1000,_),process_call(path(true), [], []),fail;true),
%       statistics(walltime,[_|Time]).
%
%    Time = [1083.122]
%
%    % Executing the 'true' predicate in a forked engine
%    ?- statistics(walltime,_),
%       (between(1,1000,_),process_call('$fork'(true), [], []),fail;true),
%       statistics(walltime,[_|Time]).
% 
%    Time = [1234.122]
%
%  The numbers of 'runtime' are much slower.
%

:- doc(bug, "(stability) Protect against signals").

:- doc(bug, "(stability) Pending safety checks? Close file descriptors
   in child process?").

:- doc(bug, "(mingw) Add Windows specific options to create processes
   in a new console and create new process groups. Process groups will
   be necessary for killing subprocess using @pred{process_kill/1}.").

% ---------------------------------------------------------------------------

:- use_module(library(lists), [append/3, select/3]).
:- use_module(library(port_reify)).

:- use_module(engine(internals), ['$exec'/9]).
:- use_module(library(system), 
	[wait/2, kill/2, working_directory/2, using_windows/0]).

:- use_module(library(process/process_channel)).

% ---------------------------------------------------------------------------

:- export(process/1).
:- regtype process/1 # "A process handler".
process('$process'(_Joined, _OnReturn, _Pid,
		   _PCall,
		   _InChannelB, _OutChannelB, _ErrChannelB)).

:- export(process_option/1).
:- regtype process_option/1 # "Options that control command execution".
:- doc(process_option/1, "

   The predicate @pred{process_call/3} waits for process completion,
   throwing an exception if the return code is different than @tt{0}.
   This default exit behaviour can be controlled with the following
   options:

   @begin{description}
   @item @tt{status(ReturnCode)}: unifies return code with
     @var{ReturnCode} upon process completion (no exception is thrown,
     may fail).
   @item @tt{background(Process)}: execute asynchronously in
     background; most errors (including input/output) are delayed to
     @pred{process_join/1}.
   @item @tt{setsid}: call @tt{setsid()} on the child to create a new
     session (useful to create daemon processes
     @href{http://www.netzmafia.de/skripten/unix/linux-daemon-howto.html})
   @end{description}

   The process standard input, output, and error file descriptors
   (streams from the Prolog side) can be bind to several
   @regtype{process_channel/1}:

   @begin{description}
   @item @tt{stdin(Channel)}: set channel for standard input
   @item @tt{stdout(Channel)}: set channel for standard output
   @item @tt{stderr(Channel)}: set channel for standard error
   @end{description}

   The environment of the process can be modified with the following
   options:

   @begin{description}
   @item @tt{cwd(Dir)}: execute command at the @var{Dir} directory
     (does not affect relative path for input/output redirection).
   @item @tt{env(Env)}: modify the specified environment variables.
   @item @tt{noenv(Env)}: unset the specified environment variables.
   @end{description}
").

% Exit behaviour
process_option(background(Process)) :- process(Process).
process_option(status(_ReturnCode)).
process_option(setsid).
% IO redirection
process_option(stdin(Channel)) :- process_channel(Channel).
process_option(stdout(Channel)) :- process_channel(Channel).
process_option(stderr(Channel)) :- process_channel(Channel).
% Exec options
process_option(cwd(Dir)) :- atm(Dir).
process_option(env(_)).
process_option(noenv(_)).

% ---------------------------------------------------------------------------

% Remove an option from Opts
optdel(Opt, Opts, Opts1) :-
	( select(Opt, Opts, Opts0) ->
	    Opts1 = Opts0
	; fail
	).
% Get an option from Opts
optget(Opt, Opts) :-
	member(Opt, Opts), !.

:- export(process_cmd/1).
:- regtype process_cmd/1 # "Command for @pred{process_call/3}".
process_cmd(path(X)) :- atm(X).
process_cmd(X) :- atm(X).

:- export(process_arg/1).
:- regtype process_arg/1 # "Argument for @pred{process_call/3}".
process_arg(X) :- atm(X).

:- export(process_call/3).
:- pred process_call(Cmd, Args, Opts) :
	( process_cmd(Cmd), list(Args, process_arg), list(Opts, process_option) )
   # "Execute a command in a child process, where @var{Cmd} is the
      executable path. Use @tt{path(Exec)} for executing a program
      @var{Exec} reachable from the @tt{PATH} environment variable".

process_call(Cmd, Args, Opts) :-
	% Working directory for execution (may throw exceptions)
	( optget(cwd(AtDir), Opts) -> 
	    Cwd = AtDir,
	    % just check permissions, real cwd is done in '$exec'/9
	    working_directory(Cwd0, Cwd),
	    working_directory(_, Cwd0)
	; Cwd = _ 
	),
	% Get channels from Opts (may throw exceptions)
	getchannels(Opts, Channels),
	% Use a apropriate transmission channel for each channel
	channel_bindings(Channels, [InChannelB, OutChannelB, ErrChannelB]),
	% Call setsid
	( optget(setsid, Opts) -> SetSid = true ; SetSid = false ),
	% Environment variables
	parse_env(Opts, Env),
	% Send input (synchronous before process creation)
	send_input(sync, InChannelB),
	% Call process
	process_call__(Cmd, Args, InChannelB, OutChannelB, ErrChannelB,
	               Env, Cwd, SetSid, Pid),
	% Send input (asynchronous after process creation)
	send_input(async, InChannelB),
	% The process handler
	PCall = pcall(Cmd, Args, Opts2),
	Process = '$process'(_Joined, OnReturn, Pid,
	                     PCall,
	                     InChannelB, OutChannelB, ErrChannelB),
	% Action on process return (unify return code or throw exception if nonzero)
	( optget(status(ReturnCode), Opts) ->
	    OnReturn = get_status(ReturnCode)
	; OnReturn = check_zero_status
	),
	% Join process (if needed)
	( optdel(background(Process0), Opts, Opts2) -> % TODO: optdel is a hack to avoid cyclic terms in Opts
	    Process0 = Process
	; Opts2 = Opts, process_join(Process)
	).

process_call__(Cmd0, Args, InChannelB, OutChannelB, ErrChannelB,
	       Env, Cwd, SetSid, Pid) :-
	open_redirect(InChannelB, read, InS),
	open_redirect(OutChannelB, write, OutS),
	open_redirect(ErrChannelB, write, ErrS),
	% Compute flags
	Flags0 = 0,
	( SetSid = true -> Flags2 = 0x4 /* EXEC_FLAG_SETSID */
	; Flags2 = 0
	),
	( Cmd0 = '$fork'(Goal), nonvar(Goal), Goal = '$:'(_) ->
	    % Fork
	    % NOTE: fork() is not available in Windows (Cygwin poorly
	    %   emulates it)
	    Flags1 = 0x1 /* EXEC_FLAG_ENGFORK */,
	    Flags is Flags0 \/ Flags1 \/ Flags2,
	    ( '$exec'('', [], InS, OutS, ErrS,
	              Env, Cwd, Flags, Pid) ->
	        % Parent
	        true
	    ; % Child (must exit with a halt/1)
              % TODO: disallow call runtime module expansions here
	      once_port_reify(Goal, GoalP),
	      % TODO: more detailed communication with parent?
	      %   (Prolog bindings, failure, exceptions, etc.)
	      ( GoalP = success -> halt(0) ; halt(-1) )
	    )
	; % (this predicate never fails or throw exceptions)
	  ( Cmd0 = path(Cmd1) -> Flags1 = 0x2 /* EXEC_FLAG_PATHEXEC */
	  ; Cmd1 = Cmd0, Flags1 = 0
	  ),
	  Flags is Flags0 \/ Flags1 \/ Flags2,
	  '$exec'(Cmd1, Args, InS, OutS, ErrS,
	          Env, Cwd, Flags, Pid)
	),
	close_redirect(InChannelB, InS),
	close_redirect(OutChannelB, OutS),
	close_redirect(ErrChannelB, ErrS).

% Parse environment set/unset options (for internals:'$exec'/9 representation)
parse_env(Opts, Env) :-
	( optget(noenv(UnsetEnv), Opts) -> parse_env_(UnsetEnv, Env, Env0)
	; Env = Env0
	),
	( optget(env(SetEnv), Opts) -> parse_env_(SetEnv, Env0, [])
	; Env0 = []
	).

parse_env_([], Ys, Ys).
parse_env_([X|Xs], [Y|Ys], Zs) :-
	( X = (A=B), atom(A), atom(B) -> Y = (A-B) % Set
	; atom(X) -> Y = X % Unset
	; throw(error(wrong_environment(X), process_call/3))
	),
	parse_env_(Xs, Ys, Zs).

:- export(process_pid/2).
:- pred process_pid(Process, Pid) : (process(Process), int(Pid))
   # "The POSIX PID of the process @var{Process}.".
process_pid(Process, _Pid) :-
	var(Process), !, throw(error(instantiation_error, process_pid/2-1)).
process_pid(Process, Pid) :-
	Process = '$process'(_Joined, _OnReturn, Pid,
	                     _PCall,
	                     _InChannelB, _OutChannelB, _ErrChannelB).

:- export(process_is_joined/1).
:- pred process_is_joined(Process) : process(Process)
   # "The process has already been joined.".
process_is_joined(Process) :-
	var(Process), !, throw(error(instantiation_error, process_is_joined/1-1)).
process_is_joined(Process) :-
	Process = '$process'(Joined, _OnReturn, _Pid,
	                     _PCall,
	                     _InChannelB, _OutChannelB, _ErrChannelB),
        nonvar(Joined).

:- export(process_join/1).
:- pred process_join(Process) : process(Process)
   # "Wait for completion of process @var{Process}.".
process_join(Process) :-
	var(Process), !, throw(error(instantiation_error, process_join/1-1)).
process_join(Process) :-
	process_is_joined(Process), !.
process_join(Process) :-
	Process = '$process'(Joined, OnReturn, Pid,
	                     PCall,
			     InChannelB, OutChannelB, ErrChannelB),
	% Mark the process as joined (so that it is not waited twice)
	Joined = yes,
	% Receive output (asynchronous before process termination)
	% TODO: use this when possible for efficiency
	receive_output(async, OutChannelB),
	receive_output(async, ErrChannelB),
	% Wait for zombie process (if needed) and get return code
	once_port_reify(do_wait(Pid, ReturnCode), WaitR),
	% Receive output (synchronous after process termination)
	receive_output(sync, OutChannelB),
	receive_output(sync, ErrChannelB),
	% Cleanup temporaries due to channel file-based bindings
	cleanup_binding(InChannelB),
	cleanup_binding(OutChannelB),
	cleanup_binding(ErrChannelB),
	% Treat result of wait
	port_call(WaitR),
	% Treat return code (which may throw exceptions).
	treat_return_code(OnReturn, ReturnCode, PCall),
	% Treat results of IO (fail or re-throw exceptions)
	binding_port_call(InChannelB),
	binding_port_call(OutChannelB),
	binding_port_call(ErrChannelB).

do_wait(Pid, ReturnCode) :-
	% TODO: delay exceptions and failure of wait/2
	wait(Pid, ReturnCode).

treat_return_code(get_status(ReturnCode0), ReturnCode, _PCall) :- !,
	ReturnCode = ReturnCode0.
treat_return_code(check_zero_status, ReturnCode, PCall) :-
	( ReturnCode = 0 ->
	    true
	; throw(error(return_code(ReturnCode, PCall), process_join/1))
	).

% ---------------------------------------------------------------------------

% Get all channels from options
getchannels(Opts, [InChannel, OutChannel, ErrChannel]) :-
	getchannel(stdin, Opts, InChannel),
	getchannel(stdout, Opts, OutChannel),
	getchannel(stderr, Opts, ErrChannel).

% Extract the (normalized) communication channel for the specified
% stream.
getchannel(Std, Opts, Channel) :-
	( ( Std = stdin, optget(stdin(Channel0), Opts)
	  ; Std = stdout, optget(stdout(Channel0), Opts)
	  ; Std = stderr, optget(stderr(Channel0), Opts)
	  ) ->
	    ( nonvar(Channel0),
	      valid_channel(Channel0, Std) ->
	        norm_channel(Channel0, Channel)
	    ; throw(error(invalid_channel(Std, Channel0), getchannel/3))
	    )
	; Channel = default
	).

% TODO: treat 'null' in process_channel, use open_null_stream?
norm_channel(null, Channel) :- using_windows, !, Channel = file('nul').
norm_channel(null, Channel) :- !, Channel = file('/dev/null').
norm_channel(Channel, Channel).

% (check that Channel is a well formed and valid channel for the
% specified stream)
valid_channel(file_append(_), Std) :- !, ( Std = stdout ; Std = stderr ).
valid_channel(stdout, Std) :- !, Std = stderr.
valid_channel(default, _).
valid_channel(null, _).
valid_channel(pipe(_), _).
valid_channel(stream(_), _).
valid_channel(file(_), _).
valid_channel(string(_), _).
valid_channel(line(_), _).
valid_channel(atmlist(_), _).
valid_channel(terms(_), _).

% ---------------------------------------------------------------------------

:- export(process_terminate/1).
:- pred process_terminate(Process) : process(Process)
   # "Sends POSIX signal SIGTERM to the process @var{Process}, which
      asks politely for process termination.".
process_terminate(Process) :-
	process_pid(Process, Pid),
	kill(Pid, 15). % SIGTERM=15

:- export(process_kill/1).
:- pred process_kill(Process) : process(Process)
   # "Sends POSIX signal SIGKILL to the process @var{Process}, which
      forces process termination.".
process_kill(Process) :-
	process_pid(Process, Pid),
	kill(Pid, 9). % SIGTERM=9

:- export(process_send_signal/2).
:- pred process_send_signal(Process, Signal) : (process(Process), int(Signal))
   # "Sends POSIX signal @var{Signal} to process @var{Process}.".
process_send_signal(Process, Signal) :-
	process_pid(Process, Pid),
	kill(Pid, Signal).

% ---------------------------------------------------------------------------

:- export(process_fork/2).

:- meta_predicate process_fork(goal, ?).
:- pred process_fork(Goal, Opts) :
	( callable(Goal), list(Opts, process_option) )
   # "Execute @var{Goal} in a forked process.".

process_fork(Goal, Opts) :-
	process_call('$fork'(Goal), [], Opts).

% ===========================================================================

:- doc(section, "Pipelined execution of processes").

:- doc(bug, "Background execution is not supported in
   @pred{process_pipe/2}. That requires the creation of a new process
   abstraction to group together several processes. This compound
   process would be user-based and then, it is not clear that it
   belongs to this module.").

% TODO: Move to a separate module? Error handling seems complex

:- export(process_pipe/2).
:- pred process_pipe(Cmd, Opts)
   # "Execute the list @var{Cmds} of @pred{process_call/3}, connecting
      standard input and output of consecutive processes with
      pipes.

      Options are passed as follows: input redirection options are
      applied to the first process; output redirection and status are
      applied to the last process; the rest of options are applied to
      all commands. Background execution is not currently supported
      (see internal documentation for details).".

process_pipe([Cmd], Opts) :- !,
	pipe_call0(Cmd, Opts).
process_pipe(Cmds, Opts) :-
	% Split options into those that apply only to the first
	% process, those that apply to the last process, and the
	% common options.
	split_pipe_opts(Opts, FirstOpts, MidOpts, LastOpts),
	% Execute processes (reifying exit ports in CallRs)
	pipe_call(Cmds, FirstOpts, MidOpts, LastOpts, Ps, CallRs),
	% Wait for processes (also prevent resource leak)
	pipe_wait(Ps, WaitRs),
	% Finally, call reified ports of calls and waits
	ports_call(CallRs),
	ports_call(WaitRs).

% Split options for pipe processes (pipe_first_opt for first,
% pipe_last_opt for last, common otherwise).
split_pipe_opts([Opt|Opts], F, M, L) :-
	( var(Opt) -> throw(error(instantiation_error, process_pipe/2-2)) ; true ),
	( pipe_first_opt(Opt) ->
	    F = [Opt|F0], M = M0, L = L0
	; pipe_last_opt(Opt) ->
	    F = F0, M = M0, L = [Opt|L0]
	; pipe_bad_opt(Opt) ->
	    throw(error(not_supported_option(Opt), process_pipe/2-2))
	; F = [Opt|F0], M = [Opt|M0], L = [Opt|L0]
	),
	split_pipe_opts(Opts, F0, M0, L0).
split_pipe_opts([], [], [], []).

pipe_first_opt(stdin(_)).

pipe_last_opt(status(_)).
pipe_last_opt(stdout(_)).
pipe_last_opt(stderr(_)).

pipe_bad_opt(background(_)).

% Call all processes and obtain the reified exit port
% (stop when some process had troubles)
pipe_call([Cmd|Cmds], FirstOpts, MidOpts, LastOpts, [P|Ps], [CallR|CallRs]) :-
	pipe_call0r(Cmd, [background(P),stdout(pipe(Out))|FirstOpts], CallR),
	pipe_call2r(CallR, Cmds, Out, MidOpts, LastOpts, Ps, CallRs).

pipe_call2r(CallR, Cmds, Out, MidOpts, LastOpts, Ps, CallRs) :-
	( CallR = success ->
	    pipe_call2(Cmds, Out, MidOpts, LastOpts, Ps, CallRs)
	; Ps = [], CallRs = []
	).

pipe_call2([Cmd], PrevOut, _MidOpts, LastOpts, [], [CallR]) :- !,
	% last command, may wait here (unless background(_) is specified)
	pipe_call0r(Cmd, [stdin(stream(PrevOut))|LastOpts], CallR),
	close(PrevOut).
pipe_call2([Cmd|Cmds], PrevOut, MidOpts, LastOpts, [P|Ps], [CallR|CallRs]) :-
	% middle command
	pipe_call0r(Cmd,
	            [background(P),stdin(stream(PrevOut)),stdout(pipe(Out))|MidOpts],
		    CallR),
	close(PrevOut),
	%
	pipe_call2r(CallR, Cmds, Out, MidOpts, LastOpts, Ps, CallRs).

pipe_call0r(Cmd, Opts, Res) :-
	once_port_reify(pipe_call0(Cmd, Opts), Res).

% Call a pipe command (may throw exceptions)
pipe_call0(Cmd, Opts) :-
	( Cmd = process_call(Cmd0, Args) -> Opts2 = []
	; Cmd = process_call(Cmd0, Args, Opts2) -> true
	; throw(error(bad_cmd(Cmd), process_pipe/2))
	),
	append(Opts, Opts2, Opts3),
	process_call(Cmd0, Args, Opts3).
	
% Wait for all process, reifying exit port
% (waiting for all is needed to avoid resource leaks)
pipe_wait([], []) :- !.
pipe_wait([P|Ps], [WaitR|WaitRs]) :-
	once_port_reify(process_join(P), WaitR),
	pipe_wait(Ps, WaitRs).

ports_call([]).
ports_call([R|Rs]) :- port_call(R), ports_call(Rs).


