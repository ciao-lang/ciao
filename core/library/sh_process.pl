:- module(_, [], [assertions, regtypes, isomodes, hiord, dcg]).

:- doc(title, "Execution of Bourne shell (POSIX sh) scripts").

:- doc(author, "Jose F. Morales").

:- doc(module, "This module allows the execution of Bourne shell
   (POSIX sh) scripts as separate processes (see @lib{process}).").

:- doc(bug, "Only works in POSIX systems where sh is available").
:- doc(bug, "See @lib{shlang}").

:- use_module(library(process)).
:- use_module(library(system), [using_windows/0]).

:- export(sh_process_call/3).
:- pred sh_process_call(Script, Args, Opts) :
	( atm(Script), list(Args, process_arg), list(Opts, process_option) )
   # "Execute a @tt{sh} script in a child process, where @var{Script}
      is the script path.".

sh_process_call(Script, Args, Opts) :-
	( using_windows -> % (e.g., MinGW, assumes sh.exe is in path)
	    process_call(path(sh), [Script|Args], Opts)
	; process_call(Script, Args, Opts)
	).

