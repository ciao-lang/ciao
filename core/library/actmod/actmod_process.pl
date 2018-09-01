:- module(actmod_process, [], [assertions, fsyntax, hiord, isomodes, regtypes, datafacts]).

:- doc(title, "Active module processes").

:- doc(module, "
   This module provides predicates to create and handle active module
   instances. Note that each instance is associated with its own
   computational resources.

   Active module instance run on their own @concept{fiber}s
   (optionally in a new process) for an @var{DMod}.
   ").


:- use_module(engine(stream_basic), [absolute_file_name/2]).
:- use_module(library(system), [file_exists/1]).
:- use_module(library(system_extra), [del_file_nofail/1, file_to_line/2]).
:- use_module(library(process), [
	process_call/3, process_send_signal/2, process_pid/2, process_join/1
   ]).
:- use_module(library(actmod/actmod_rt), [singleton_actRef/2]).
:- use_module(library(actmod/actmod_dist), [actI_alloc_named/2]).
:- use_module(library(actmod/actmod_dist), [term_to_atom/2]).
:- include(library(actmod/actmod_hooks)).
:- use_module(library(actmod/actmod_rt), [dist_log/1]).
:- use_module(library(pathnames), [path_concat/3]).
:- use_module(library(lists), [member/2]).

% ---------------------------------------------------------------------------
:- doc(section, "Spawning active modules").

% ---------------------------------------------------------------------------
% Dependencies to builder

:- use_module(ciaobld(config_common), [libcmd_path/4, cmd_path/4]).
:- use_module(ciaobld(ciaoc_aux), [cmd_build/1]). % TODO: make sure it is weak dep

% ---------------------------------------------------------------------------
% Database of child processes

% actI_process(ActRef, Process):
%   ActRef runs on OS process Process
:- data actI_process/2.

% ---------------------------------------------------------------------------
% Daemon PIDs (stored at /tmp files)

daemon_lock_file(Daemon, PidPath) :-
	atom_concat(Daemon, '.pid', PidName),
	path_concat('/tmp', PidName, PidPath).

daemon_pid(Daemon, Pid) :-
	daemon_lock_file(Daemon, LockFile),
	file_exists(LockFile),
	file_to_line(LockFile, PidStr),
	number_codes(Pid, PidStr).

remove_daemon_lock_file(Daemon) :-
	daemon_lock_file(Daemon, LockFile),
	del_file_nofail(LockFile).

% ---------------------------------------------------------------------------

:- export(actref/1).
:- regtype actref(X) 
   # "@var{X} is the reference of an active module instance".

actref(X) :- atm(X).

:- export(spawn_opt/1).
:- regtype spawn_opt(X) # "@var{X} is an spawning option".
:- doc(spawn_opt(X), "Options to control the location of the fiber:
   @begin{itemize}
   @item @tt{child}: run on a new child OS process
   @item @tt{daemon}: run on a new
     @href{https://en.wikipedia.org/wiki/Daemon_(computing)}{daemon}
     OS process (background processes that are their own session and
     group leaders, and that are dissociated from a controlling
     terminal)
   @item (default): run on this OS process
   @end{itemize}

   Options to specify the active module implementation:
   @begin{itemize}
   @item @tt{exec(ExecPath)}: path for executable
     (containing the distributed runtime)
   @item @tt{dynmod(Mod)}: module spec (for spawning using a generic
     @tt{ciaosh} runtime)
   @item @tt{libexec}: like @tt{exec/1}, using binary created by
     @pred{actmod_compile/1} at @tt{build/libexec}
   @end{itemize}").

spawn_opt(child).
spawn_opt(daemon).
spawn_opt(exec(X)) :- atm(X).
spawn_opt(dynmod(X)) :- atm(X).
spawn_opt(libexec).

:- export(actmod_spawn/3).
:- pred actmod_spawn(+DMod, +Opts, -ActRef) :
	atm * list(spawn_opt) * actref
# "Create a new active module instance of @var{DMod}. @var{Opts} is a
   list of options and @var{ActRef} the active module instance
   reference.".

% TODO: split cpx_process:cpx_process_call into cpx_process_expand, reuse it here
% TODO: see eng_process_call/4 (eventually we must join any background process)

% TODO: complete support for name/1 in opts
%   get_name is DMod if singleton, or value from name(_)
%   If no name:
%    - create automatic fresh name
%   Always pass name to dist_start.
%   Always register (name is not address)
%   Temporary names should be deleted.

actmod_spawn(DMod, Opts, ActRef) :-
	need_other_process(Opts),
	!,
	actmod_spawn_process(DMod, Opts, ActRef).
actmod_spawn(_DMod, _Opts, _ActRef) :-
	% TODO: complete support for "same process" and "sibling":
	%  - add sibling(...) or at(...), a node may also be an actmod
	%  - a dist node may answer queries for spawning code there (like ciaosh does)
	%  - with name/1 and with automatic fresh name
	%
	throw(error(same_process_not_implemented, actmod_spawn/3)).

% need_other_process(+Opts): It must be run in another process
need_other_process(Opts) :-
	( member(child, Opts) -> true
	; member(daemon, Opts) -> true
	; fail
	).

% actmod_spawn_process(+DMod, +Opts, -ActRef):
%   Spawn a DMod instance in a separate process
actmod_spawn_process(DMod, Opts, ActRef) :-
	singleton_actRef(DMod, ActRef),
	dist_log(['spawning ', ~~(ActRef), ' (', ~~(DMod), ') with opts ', ~~(Opts)]),
	check_named(DMod, Opts, ActRef),
	actI_alloc_named(DMod, ActRef), % ready to use this ActRef
	process_args(DMod, Opts, ExecPath, ExecArgs),
	( member(daemon, Opts) ->
	    daemon_lock_file(ActRef, LockFile),
	    daemon_process_call(ExecPath, ExecArgs, [lock_file(LockFile)])
	; process_call(ExecPath, ExecArgs, [stdin(null), background(P)]),
	  assertz_fact(actI_process(ActRef, P))
	).

% check_named(+DMod, +Opts, +ActRef):
%   Make sure that we can use the given ActRef (for singleton_actRef/2)
check_named(DMod, Opts, ActRef) :-
	% Avoid duplicated instances 
	( actI_process(ActRef, _) ->
	    % Cannot create the same instance twice
	    throw(error(duplicated_instance(DMod, ActRef), actmod_spawn/3))
	; true
	),
	( member(daemon, Opts), daemon_pid(ActRef, _) ->
	    % Cannot create a daemon if one is already running
	    throw(error(daemon_lock_file(DMod, ActRef), actmod_spawn/3))
	; true
	).

% process_args(+DMod, +Opts, -ExecPath, -ExecArgs):
%   Obtain arguments for process call
process_args(DMod, Opts, ExecPath, ExecArgs) :-
	get_code_opt(DMod, Opts, Code),
	% TODO: allow initial query
	( Code = exec(ExecPath0) ->
	    ExecPath = ExecPath0,
	    ExecArgs = [] % TODO: pass ActRef (and RegProtocol?) as arguments to actmod_dist:dist_start/3
	; Code = dynmod(ModSpec) ->
	    ExecPath = ~ciaosh_exec,
	    absolute_file_name(ModSpec, ModPath),
	    term_to_atom(DMod:main([]), Goal), % TODO: pass ActRef (and RegProtocol?) as arguments to actmod_dist:dist_start/3
	    ExecArgs = ['-q', '-f', '-u', ModPath, '-e', Goal]
	; throw(error(unknown_code, actmod_spawn/3))
	).

% get_code_opt(+DMod, +Opts, -Code): Obtain code location
get_code_opt(DMod, Opts, Code) :-
	( X = exec(_), member(X, Opts) -> Code = X
	; X = dynmod(_), member(X, Opts) -> Code = X
	; '$dmod_prop'(DMod, libexec) -> % binary from libexec
	    Code = exec(~actmod_binpath(DMod))
	; '$dmod_src'(DMod, Src) -> % source from ModSpec
	    Code = dynmod(Src)
	; throw(error(unknown_code, actmod_spawn/3))
	).

% Run as a daemon
daemon_process_call(ExecPath, ExecArgs, Opts) :-
	( member(lock_file(LockFile), Opts) ->
	    true
	; throw(error(no_lock_file, daemon_process_call/3))
	),
	RunDaemon = ~rundaemon_exec,
	process_call(RunDaemon, [LockFile, ExecPath|ExecArgs], []).

% Helper to run daemons
rundaemon_exec := ~libcmd_path(core, exec, 'rundaemon').

% Used as generic runtime % TODO: define a simpler one or use ciao-serve
ciaosh_exec := ~cmd_path(core, plexe, 'ciaosh').

% ---------------------------------------------------------------------------

:- use_module(library(system), [kill/2]).

:- export(actmod_kill/2).
:- pred actmod_kill(ActRef, Msg) # "Sends SIGKILL signal to the
   active module instance @var{ActRef}. If not registered as a child
   process, it is considered to be a daemon. Return message in
   @var{Msg}.".

actmod_kill(ActRef, Msg) :-
	actmod_signal(ActRef, 9, Msg). % SIGKILL=9

:- export(actmod_terminate/2).
:- pred actmod_terminate(ActRef, Msg) # "Sends SIGTERM signal to the
   active module instance @var{ActRef}. If not registered as a child
   process, it is considered to be a daemon. Return message in
   @var{Msg}.".

actmod_terminate(ActRef, Msg) :-
	actmod_signal(ActRef, 15, Msg). % SIGTERM=15

% TODO: change Message by status? throw exception instead?
% TODO: add some predicate to wait until process is really killed before removing pid file?
actmod_signal(ActRef, Signal, Msg) :-
	( actI_process(ActRef, P) ->
	    process_send_signal(P, Signal),
	    ( catch(process_join(P), _E, true) -> true ; true ), % Ignore errors
	    % TODO: document that process_join/1 (wait/2) fails if process terminated incorrectly
	    retractall_fact(actI_process(ActRef, _)),
	    process_pid(P, Pid)
	; daemon_pid(ActRef, Pid) ->
	    kill(Pid, Signal),
	    remove_daemon_lock_file(ActRef)
	; throw(error(unknown_pid(ActRef), actmod_signal/3))
	),
	signal_msg(Signal, Pid, Msg).

signal_msg(9, Pid, Msg) :-
	number_codes(Pid, PidStr),
	Msg = "Sending kill signal to process " || PidStr.
signal_msg(15, Pid, Msg) :-
	number_codes(Pid, PidStr),
	Msg = "Sending terminate signal to process " || PidStr.

% TODO: alternative; get PID from AddressFile

%% actmod_pid(ActRef, PID) :-
%% 	% Get server PID from .addr file
%% 	atom_concat(ActRef, '.addr', AddrFile),
%% 	see(AddrFile),
%% 	read(_Address),
%% 	read(pid(PID)),
%% 	seen.

% ---------------------------------------------------------------------------

:- export(actmod_join/1).
:- pred actmod_join(ActRef) # "Block until the active module
   instance @var{ActRef} terminates".

actmod_join(ActRef) :-
	( actI_process(ActRef, P) ->
	    process_join(P),
	    retractall_fact(actI_process(ActRef, _))
	; throw(error(invalid_instance(ActRef), actmod_join/1))
	).

% ---------------------------------------------------------------------------
:- doc(section, "Compilation of active modules (libexec)").

:- use_module(library(system), [file_exists/1]).
:- use_module(library(system_extra), [del_file_nofail/1]).

% Path to executable for the given actmod
% TODO: bundle name is hardwired
actmod_binpath(DMod, BinPath) :-
	BinPath = ~libcmd_path(core, exec, DMod).

:- export(actmod_compile_all/0).
:- pred actmod_compile_all # "Compile all imported active modules
   (only for @tt{libexec})".

actmod_compile_all :-
	( % (failure-driven loop)
	  '$dmod_src'(M, _),
	    actmod_compile(M),
	    fail
	; true
	).

:- export(actmod_compile/1).
:- pred actmod_compile(DMod) # "Ensure that the (imported) active
   module @var{DMod} is compiled (only for @tt{libexec})".

actmod_compile(DMod) :-
	( '$dmod_prop'(DMod, libexec) ->
	    '$dmod_src'(DMod, ModSpec),
	    absolute_file_name(ModSpec, ModPath),
	    atom_concat(ModBase, '.pl', ModPath),
	    cmd_build(cmd_def(core, ModBase, DMod, [libexec])),
	    actmod_check_bin(DMod)
	; true
	).

:- export(actmod_check_bin/1).
:- pred actmod_check_bin(DMod) # "Check a binary for the (imported)
   active module @var{DMod} exists (only for @tt{libexec})".

actmod_check_bin(DMod) :-
        actmod_binpath(DMod, BinPath),
        ( file_exists(BinPath) ->
	    true
	; throw(error(bin_not_found(DMod), actmod_check_bin/1))
	).

% ---------------------------------------------------------------------------
:- doc(section, "Loading of active modules (same process)").

:- use_module(library(actmod/actmod_holder), [do_use_module/1]).

:- export(actmod_load_dynmod/1).
% Make sure that code is available, do not spawn yet.
actmod_load_dynmod(ModSpec) :-
	actmod_holder:do_use_module(ModSpec).
	