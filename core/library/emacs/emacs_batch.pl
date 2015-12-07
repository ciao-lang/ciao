:- module(_, [], [assertions, fsyntax]).

:- doc(title,  "Emacs batch calls").

:- doc(author, "Jos@'{e} F. Morales (redesign, new code)").
:- doc(author, "Ciao Development Team").

:- doc(bug, "This needs to be merged with emacs.pl library").

:- use_module(library(process), [process_call/3]).
:- use_module(library(pathnames), [path_concat/3]).

% ---------------------------------------------------------------------------

% TODO: bundle_param_value/2 should not be used here
:- use_module(library(bundle/bundle_params), [bundle_param_value/2]).
:- use_module(library(system), [winpath/2]).

:- use_module(library(bundle/bundle_flags), [current_bundle_flag/2]).

:- export(emacs_type/1).
emacs_type(EmacsType) :-
	% Note: for 'ciao custom_run core environment_and_windows_bats'
	bundle_param_value(core:emacs_type, EmacsType0),
	!,
	EmacsType = EmacsType0.
emacs_type(posix).

:- export(emacs_path/1).
emacs_path(Emacs) :-
	% Note: for 'ciao custom_run core environment_and_windows_bats'
	bundle_param_value(ciao:emacs_path, Emacs0),
	winpath(Emacs, Emacs0),
	!.
emacs_path(Emacs) :-
	( current_bundle_flag(core:emacs_for_ciao, Emacs0) -> Emacs = Emacs0
	; throw(error(no_emacs, emacs_path/1))
	).

% ---------------------------------------------------------------------------

:- use_module(library(system), [cyg2win_a/3]).

:- export(emacs_style_path/2).
emacs_style_path(Path0, Path) :- emacs_type('Win32'), !,
	cyg2win_a(Path0, Path, noswap).
emacs_style_path(Path, Path).

% ---------------------------------------------------------------------------

:- use_module(library(logged_process), [logged_process_call/3]).

% TODO: Use 'append' creation mode for logs? (use a single log)
:- export(emacs_batch_call/3).
emacs_batch_call(Dir, LogName, Args) :-
	% Environment variables unset for this call
	NoEnv = ['SHELL', 'EMACSLOADPATH', 'EMACSDOC'],
	%
	Log0 = ~path_concat(Dir, LogName),
	%
	logged_process_call(~emacs_path, ['-batch'|Args],
	    [cwd(Dir),
	     noenv(NoEnv),
	     logbase(Log0, ''),
	     show_logs(on_error), status(_)]).

% ---------------------------------------------------------------------------

:- use_module(library(system_extra), [del_file_nofail/1]).

% TODO: move to library(logged_process)
:- export(emacs_clean_log/2).
% Clean log files created during a emacs batch call
emacs_clean_log(Dir, LogName) :-
	Log0 = ~path_concat(Dir, LogName),
	Out = ~atom_concat(Log0, '.log'),
	Err = ~atom_concat(Log0, '.err'),
	del_file_nofail(Out),
	del_file_nofail(Err).

% ---------------------------------------------------------------------------

:- use_module(library(terms), [atom_concat/2]).

:- export(emacs_update_autoloads/3).
:- pred emacs_update_autoloads(Dir, Log, AutoloadEL)
   # "Invoke the Emacs @tt{'batch-update-autoloads'} function to generate
      the autoload file @var{AutoloadEL}.".
 
emacs_update_autoloads(Dir, Log, AutoloadEL) :-
	AutoloadEL2 = ~emacs_style_path(AutoloadEL),
	% TODO: espape AutoloadEL2
	emacs_batch_call(Dir, Log, 
          ['--eval', ~atom_concat(['(setq generated-autoload-file "', AutoloadEL2, '")']),
	   '-f', 'batch-update-autoloads', '.']).

:- export(emacs_batch_byte_compile/3).
:- pred emacs_batch_byte_compile(Dir, Log, EL_Files)
   # "Invoke the Emacs '@tt{'batch-byte-compile'} function to byte compile 
      the specified @var{EL_Files} elisp files.".

emacs_batch_byte_compile(Dir, Log, EL_Files) :-
	emacs_batch_call(Dir, Log,
	  ['--eval', '(setq load-path (cons "." load-path))',
	   '-f', 'batch-byte-compile'|
           EL_Files]).

