:- module(_, [main/1], [foreign_interface]).

% TODO: temporary! merge 'rundaemon' with process.pl (see setuid)
%   (this should be an option of process_call/3)

:- use_foreign_source(.(rundaemon)).

:- true pred run_daemon(in(LockFile), in(ExecPath))
        :: atm * atm + foreign(run_daemon).

main([LockFile, ExecPath]) :-
	display(running_daemon(LockFile, ExecPath)), nl,
	run_daemon(LockFile, ExecPath),
	display(returned(LockFile, ExecPath)), nl.
