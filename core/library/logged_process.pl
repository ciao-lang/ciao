:- module(logged_process, [], [assertions, regtypes, basicmodes, fsyntax]).

:- doc(title, "Logged Processes").
:- doc(author, "Jose F. Morales").

:- doc(module, "A wrapper on top of @pred{process_call/3} with some
   logging facilities. It the process does not exit with error status
   0, some special action is taken.

   When @tt{logbase(Base)} is specified, the standard output and error
   of the process is stored in files. The option
   @tt{show_logs(ShowLogs)} indicates how logs are handled:

   @begin{description}
   @item{@tt{silent}} do not show any log (default).
   @item{@tt{always}} always show logs (whether success or error).
   @item{@tt{note_always}} always show a note indicating where logs 
                      are stored, but do not show contents (whether 
                      success or error).
   @item{@tt{on_error}} show the logs if there is an error.
   @item{@tt{note_on_error}} on error, show a note indicating where logs are 
                      stored, but do not show contents.
   @item{@tt{on_error_stderr}} like @tt{on_error} but show the logs
                      from standard error and a note indicating where
                      the standard output log is.
   @end{description}
").

:- use_module(library(stream_utils), [file_to_string/2]).
:- use_module(library(messages), [error_message/2, note_message/2]).
:- use_module(library(lists), [select/3]).
:- use_module(library(process),
	[process_call/3, process_cmd/1, process_arg/1, process_option/1]).

:- export(logged_process_call/3).
:- pred process_call(Cmd, Args, Opts) :
	( process_cmd(Cmd), list(Args, process_arg), list(Opts, process_option) )
   # "Like @pred{process_call/3}, but logs of standard input and output.".

logged_process_call(Cmd, Args, Opts) :-
	% Base for logs
	select(logbase(Base), Opts, Opts1),
	!,
	% 
	( select(show_logs(ShowLogs), Opts1, Opts2) ->
	    Opts3 = Opts2
	; ShowLogs = silent,
	  Opts3 = Opts1
	),
	%
	% TODO: Option for (no)append logs?
	% TODO: use stderr_to_stdout?
	atom_concat(Base, '.log', LogFile),
	atom_concat(Base, '.err', ErrFile),
	%
	Opts4 = [stdout(file(LogFile)), stderr(file(ErrFile))|Opts3],
	% Action on process return (unify return code or throw exception if nonzero)
	( select(status(ReturnCode0), Opts4, Opts5) ->
	    OnReturn = get_status(ReturnCode0)
	; OnReturn = check_zero_status,
	  Opts5 = Opts4
	),
	Opts6 = [status(ReturnCode)|Opts5],
	%
	process_call(Cmd, Args, Opts6),
	%
	PCall = pcall(Cmd, Args, Opts),
	treat_logs(ReturnCode, LogFile, ErrFile, OnReturn, ShowLogs, PCall),
	treat_return_code(OnReturn, ReturnCode, PCall).
logged_process_call(Cmd, Args, Opts) :-
	( select(show_logs(_ShowLogs), Opts, Opts1) -> % TODO: ignored?
	    true
	; Opts1 = Opts
	),
	process_call(Cmd, Args, Opts1).

treat_return_code(get_status(ReturnCode0), ReturnCode, _PCall) :- !,
	ReturnCode = ReturnCode0.
treat_return_code(check_zero_status, ReturnCode, PCall) :-
	( ReturnCode = 0 ->
	    true
	; throw(error(return_code(ReturnCode, PCall), logged_process_call/3))
	).

% showlog(ShowLogs, Status, ShowOut, ShowErr):
%   How should standard output and error logs be shown
%   depending on Status for the given ShowLogs.
showlog(note_always,     _,     note, note).
showlog(always,          _,     show, show).
showlog(note_on_error,   error, note, note).
showlog(on_error,        error, show, show).
showlog(on_error_stderr, error, note, show).

treat_logs(ReturnCode, LogFile, ErrFile, OnReturn, ShowLogs, PCall) :-
	( ReturnCode = 0 ->
	    Status = success
	; Status = error
	),
	% Log 
	( showlog(ShowLogs, Status, show, _) ->
	    display_string(~file_to_string(LogFile))
	; true
	),
	( showlog(ShowLogs, Status, _, show) ->
	    display_string(~file_to_string(ErrFile))
	; true
	),
	%
	( ReturnCode = 0 ->
	    true
	; OnReturn = check_zero_status ->
	    % Do not show message (will throw exception on treat_return_code/3)
	    true
	; PCall = pcall(Cmd, Args, Opts),
	  error_message("process ~w returned code ~w (arguments: ~w, options: ~w) ",
	                [Cmd, ReturnCode, Args, Opts])
	),
        %
	( showlog(ShowLogs, Status, note, _) ->
	    note_message("Standard output log saved in: ~w", [LogFile])
	; true
	),
        %
	( showlog(ShowLogs, Status, _, note) ->
	    note_message("Standard error  log saved in: ~w", [ErrFile])
	; true 
	).

% ===========================================================================
:- doc(section, "Process call where all output is quoted").

:- use_module(library(stream_utils), [get_line/2, write_string/1]).
:- use_module(library(process), [process_call/3, process_join/1]).

:- export(quoted_process_call/3).
quoted_process_call(Cmd, Args, Options) :-
	process_call(Cmd, Args, 
	             [stderr(stdout),
		      stdout(pipe(Out)),
		      background(P)|Options]),
	quote_lines(Out),
	process_join(P).

quote_lines(Out) :-
	( repeat,
	  get_line(Out, Line),
	    ( Line = end_of_file ->
	        !
	    ; write_string("| "||Line), nl,
	      fail
	    )
	; true
	).

