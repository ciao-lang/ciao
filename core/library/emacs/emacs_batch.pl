:- module(_, [], [assertions, fsyntax, datafacts]).

:- doc(title,  "Emacs batch calls").

:- doc(author, "Jos@'{e} F. Morales (redesign, new code)").
:- doc(author, "The Ciao Development Team").

:- doc(bug, "This needs to be merged with emacs.pl library").

:- use_module(library(process), [process_call/3]).
:- use_module(library(pathnames), [path_concat/3]).

% ---------------------------------------------------------------------------

:- use_module(library(system), [winpath/2]).
% TODO: This may be expensive. Compute statically instead?
:- use_module(library(bundle/bundle_flags), [current_bundle_flag/2]).

% TODO: only for 'ciao custom_run core environment_and_windows_bats'; do in other way?
:- data custom_emacs_type/1.
:- data custom_emacs_path/1.

:- export(set_emacs_type/1).
% Select dynamically a custom emacs type
set_emacs_type(Type) :- set_fact(custom_emacs_type(Type)).

:- export(unset_emacs_type/0).
% Unselect the custom emacs type (uses default)
unset_emacs_type :- retractall_fact(custom_emacs_type(_)).

:- export(set_emacs_path/1).
% Select dynamically a custom emacs path
set_emacs_path(Path) :- set_fact(custom_emacs_path(Path)).

:- export(unset_emacs_path/0).
% Unselect the custom emacs path (uses default)
unset_emacs_path :- retractall_fact(custom_emacs_path(_)).

:- export(emacs_type/1).
emacs_type(Type) :-
    custom_emacs_type(Type0),
    !,
    Type = Type0.
emacs_type(posix).

:- export(emacs_path/1).
emacs_path(Path) :-
    custom_emacs_path(Path0),
    winpath(Path, Path0),
    !.
emacs_path(Path) :-
    ( current_bundle_flag(ciao_emacs:emacs_for_ciao, Path0) -> Path = Path0
    ; throw(error(no_emacs, emacs_path/1))
    ).

% ---------------------------------------------------------------------------

:- use_module(library(system), [cyg2win_a/3]).

:- export(emacs_style_path/2).
emacs_style_path(Path0, Path) :- emacs_type('Win32'), !,
    cyg2win_a(Path0, Path, noswap).
emacs_style_path(Path, Path).

% ---------------------------------------------------------------------------

:- use_module(library(process), [process_call/3]).

:- export(emacs_batch_call/2).
:- pred emacs_batch_call(Args, Opts)
   # "Process call to @tt{emacs} in batch mode with extra arguments
      @var{Args} and process call options @var{Opts}".

emacs_batch_call(Args, Opts) :-
    % unset some environment variables
    NoEnv = ['SHELL', 'EMACSLOADPATH', 'EMACSDOC'],
    process_call(~emacs_path,
                 ['-batch'|Args],
                 [noenv(NoEnv)|Opts]).

% ---------------------------------------------------------------------------

:- use_module(library(terms), [atom_concat/2]).

:- export(emacs_update_autoloads/2).
:- pred emacs_update_autoloads(Dir, AutoloadEL)
   # "Invoke the Emacs @tt{'loaddefs-generate-batch'} function to generate
      the autoload file @var{AutoloadEL}.".
 
emacs_update_autoloads(Dir, AutoloadEL) :-
    AutoloadEL2 = ~emacs_style_path(AutoloadEL),
    % TODO: espape AutoloadEL2
    emacs_batch_call(
        % simulate loaddefs-generate-batch if emacs version <29
        ['--eval', ~atom_concat(['(if (version< emacs-version "29.0")',
         ' (defun loaddefs-generate-batch ()',
         '   (let ((args command-line-args-left))',
         '     (setq command-line-args-left nil)',
         '     (setq generated-autoload-file (car args))',
         '     (update-directory-autoloads (car (cdr args))))))']),
         '-f', 'loaddefs-generate-batch', AutoloadEL2, '.'],
        [cwd(Dir), status(_)]).

:- export(emacs_batch_byte_compile/2).
:- pred emacs_batch_byte_compile(Dir, EL_Files)
   # "Invoke the Emacs '@tt{'batch-byte-compile'} function to byte compile 
      the specified @var{EL_Files} elisp files.".

emacs_batch_byte_compile(Dir, EL_Files) :-
    emacs_batch_call(
        ['--eval', '(setq load-path (cons "." load-path))',
         '-f', 'batch-byte-compile'|EL_Files],
        [cwd(Dir), status(_)]).

