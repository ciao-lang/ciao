:- module(_, [main/1], [assertions, foreign_interface]).

:- doc(title, "Run-daemon").
:- doc(author, "Jose F. Morales").

:- doc(module, "This is an auxiliary command used to start
   any other executable as a daemon process.

   Usage:
   @begin{itemize}
   @item Start the daemon:
@begin{verbatim}
rundaemon LOCKFILE EXECPATH <Args>
@end{verbatim}
   @item Stop the daemon:
@begin{verbatim}
kill `cat LOCKFILE`
@end{verbatim}
   @end{itemize}
   ").

:- doc(bug, "Use @lib{process} instead, see support for @tt{setuid}
   flag. What is really missing there is a lock file").

:- use_foreign_source(.(rundaemon)).

:- trust pred run_daemon(in(LockFile), in(ExecPath), in(Args))
       :: atm * atm * list(atm) + foreign_low(run_daemon).

main([LockFile, ExecPath|Args]) :-
    run_daemon(LockFile, ExecPath, Args).
