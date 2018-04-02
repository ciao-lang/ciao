:- module(actmod_process, [], [assertions, fsyntax, hiord]).

:- doc(title, "Active module processes").

:- doc(module, "Module to manage active module processes. Use it to
   start active module as as separate child or daemon process.

   NOTE: This assumes that active modules are reachable using the
   @pred{config_common:libcmd_path/4} predicate.

   @href{https://en.wikipedia.org/wiki/Daemon_(computing)}{Daemon
   processes} are background processes that are their own session and
   group leaders.  They are dissociated from a controlling
   terminal. This module can turn any compiled @concept{active module}
   in a deamon, spawning its execution using the @lib{rundaemon.c}
   command (included in this directory).

   Use @pred{actmod_kill/3} to manually kill an active module (child
   or daemon).").

:- use_module(library(process), [process_call/3, process_kill/1, process_pid/2]).
:- use_module(library(pathnames), [path_concat/3]).

:- use_module(ciaobld(config_common), [libcmd_path/4]).

% ---------------------------------------------------------------------------
% Child and daemons processes data 

:- use_module(library(system), [file_exists/1]).
:- use_module(library(system_extra), [del_file_nofail/1]).
:- use_module(library(system_extra), [file_to_line/2]).

% Database of child processes

% curr_actmod_process(Bundle, ActMod, Process)
:- data curr_actmod_process/3.

% Daemon PID is stored at /tmp files

daemon_lock_file(Daemon, PidPath) :-
	atom_concat(Daemon, '.pid', PidName),
	path_concat('/tmp', PidName, PidPath).

daemon_pid(ActMod, Pid) :-
	daemon_lock_file(ActMod, LockFile),
	file_exists(LockFile),
	file_to_line(LockFile, PidStr),
	number_codes(Pid, PidStr).

remove_daemon_lock_file(ActMod) :-
	daemon_lock_file(ActMod, LockFile),
	del_file_nofail(LockFile).

% ---------------------------------------------------------------------------

:- export(actmod_start/3).
:- pred actmod_start(Bundle, ActMod, Opts) # "Starts the active module
   @var{ActMod} from bundle @var{Bundle}. Use @tt{Opts=[daemon]} for
   starting as a daemon".

actmod_start(Bundle, ActMod, Opts) :-
	member(daemon, Opts),
	!,
	ActModPath = ~libcmd_path(Bundle, plexe, ActMod),
	daemon_lock_file(ActMod, LockFile),
	run_daemon_path(RunDaemon),
	process_call(RunDaemon, [LockFile, ActModPath], []).
actmod_start(Bundle, ActMod, _Opts) :-
	curr_actmod_process(Bundle, ActMod, _),
	!,
	throw(error(already_started(Bundle, ActMod), actmod_start/2)).
actmod_start(Bundle, ActMod, _Opts) :-
	ActModPath = ~libcmd_path(Bundle, plexe, ActMod),
	process_call(ActModPath, [], [background(P)]),
	assertz_fact(curr_actmod_process(Bundle, ActMod, P)).

% Helper to run daemons
run_daemon_path(RunDaemon) :-
	RunDaemon = ~libcmd_path(core, exec, 'rundaemon').

:- export(actmod_kill/3).
:- pred actmod_kill(Bundle, ActMod, Msg) # "Kills the active module
   @var{ActMod} from @var{Bundle}. If not registered as a child
   process, it is considered to be a daemon. Return kill message in
   @var{Msg}.".

:- use_module(library(system), [kill/2]).

% TODO: change Message by status? throw exception instead?
% TODO: wait until process is really killed before removing pid file?
actmod_kill(Bundle, ActMod, Msg) :-
	curr_actmod_process(Bundle, ActMod, P),
	!,
	retractall_fact(curr_actmod_process(Bundle, ActMod, _)),
	process_kill(P),
	%
	process_pid(P, Pid),
	kill_msg(Pid, Msg).
actmod_kill(_Bundle, ActMod, Msg) :-
	daemon_pid(ActMod, Pid),
	!,
	kill(Pid, 9), % SIGTERM=9
	remove_daemon_lock_file(ActMod),
	kill_msg(Pid, Msg).

kill_msg(Pid, Msg) :-
	number_codes(Pid, PidStr),
	Msg = "Killing actmod... pid=" || PidStr.
