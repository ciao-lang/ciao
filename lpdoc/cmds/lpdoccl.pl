:- module(lpdoccl, [main/1], [assertions]).

:- doc(title,"The LPdoc top-level and command-line interface").
:- doc(author, "Manuel Hermenegildo").
:- doc(author, "Jos@'{e} Francisco Morales").

:- doc(module, "This is the top-level and command-line interface to
   LPdoc. Please look at @lib{lpdoc} documentation for top-level usage
   information. The command-line interface allows the use of the
   system in batch mode, using arguments for specifiying documentation
   setting, targets, and actions.
").

%% Version information
:- include(lpdoc(version_auto)).

:- use_module(library(messages), [error_message/2]).
:- use_module(library(system), [file_exists/1]).

% entry point for lpdoc library
:- use_module(lpdoc(docmaker), [make_doc/4]).

:- use_module(library(toplevel), [toplevel/1]). % (for built-in toplevel)

% ===========================================================================

:- doc(section, "(Definitions for this Application)").

% (defines a main/1 that calls start/1)
:- include(library(lpsettings_based_app)). % TODO: use real interfaces...

% TODO: Many of those definitions should come from the documentation
% of this file.

% The application name
app_name('LPdoc'). % TODO: like packname in Manifest
%app_name(lpdoc).
% Version atom
%% version(_)
% Copyright
app_copyright(_) :- fail.

app_options_message("
LPdoc options:

-cv,--comment-version The source files contain version information.
                If not specified lpdoc will asume the opposite.

-c FILE         Process FILE as a separate (standalone) component.
").

is_option0('-cv').
handle_option0('-cv') :- do_comment_version.
%
is_option0('--comment-version').
handle_option0('--comment-version') :- do_comment_version.
do_comment_version :-
	retractall_fact(opt_name_value(comment_version, _)),
	assertz_fact(opt_name_value(comment_version, yes)).

% ---------------------------------------------------------------------------

start(ConfigFile, Opts, Args) :-
	( Args = ['-T'] ->
	    ( Opts = [] ->
	        true
	    ; error_message("No options allowed in lpdoc toplevel mode", []),
	      fail
	    ),
	    lpdoc_toplevel([])
	; make_doc(ConfigFile, Opts, Args, '.') % TODO: it may be better to use '_' here for output dir
	).

lpdoc_toplevel(Opts2) :-
	set_prolog_flag(quiet, warning),
	Opts = ['-p', 'lpdoc ?- '|Opts0], 
	CiaoRC = '~/.ciaorc',
	( file_exists(CiaoRC) ->
	    Opts0 = ['-l', '~/.ciaorc'|Opts1]
	; Opts0 = Opts1
	),
	Opts1 = [
		   '-u', lpdoc(docmaker),
		   '-g', set_prolog_flag(quiet, off)
               |Opts2],
	toplevel:toplevel(Opts).

