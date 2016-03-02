:- module(lpdoccl, [], [assertions, dcg]).

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
:- use_module(lpdoc(docmaker), [doc_cmd/4]).
% command line documentation
:- use_module(lpdoc(lpdoccl_help)).

:- use_module(library(toplevel), [toplevel/1]). % (for built-in toplevel)

% ===========================================================================
% Common definitions and main/1 entry point for an application using a
% command line interface, SETTINGS.pl
% ===========================================================================

:- use_module(library(errhandle), [handle_error/2]).
:- use_module(library(messages), [error_message/2]).
:- use_module(library(format), [format/3, sformat/3]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(system), [working_directory/2, file_exists/1]).
:- use_module(library(make/make_rt), [parse_name_value/3]).

% ---------------------------------------------------------------------------

% Invocation from the command-line interface
:- export(main/1).
main(Args) :-
	catch(main_(Args), E, (handle_lpdoc_error(E), fail)).

main_([Help0|Args]) :-
	help_mode(Help0, Mode),
	!,
	( Args = [] ->
	    show_help(Mode, normal)
	; Args = [Arg0] ->
	    show_help_cmd(Arg0, normal)
	; fail
	).
main_(Args) :-
	reset_opts,
	parse_args(Args, Cmd),
	lpdoc_cmd(Cmd).

help_mode('-h', all).
help_mode('--help', all).
help_mode('help', all).

version_cmd('version').

% ---------------------------------------------------------------------------
% Parse command line arguments

parse_args(Args, Cmd) :-
	parse_opts(Args, Rest),
	!,
	parse_cmd(Rest, Cmd).
parse_args(Args, _) :-
	error_bad_args(Args).

% (general options)
parse_opts([Opt|Args], Rest) :-
	is_option0(Opt),
	!,
	handle_option0(Opt),
	parse_opts(Args, Rest).
parse_opts([Opt, Arg|Args], Rest) :-
	is_option1(Opt),
	!,
	handle_option1(Opt, Arg),
	parse_opts(Args, Rest).
parse_opts(Args, Rest) :-
	Rest = Args.

% (command)
parse_cmd([X], Cmd) :- version_cmd(X), !, Cmd = version.
parse_cmd([X], Cmd) :- view_cmd(X, Suffix), !, Cmd = view(Suffix).
parse_cmd([X], Cmd) :- clean_cmd(X, Mode), !, Cmd = clean(Mode).
parse_cmd(['-T'], Cmd) :- !, Cmd = toplevel.
parse_cmd(Targets, Cmd) :- !,
	Cmd = start(Targets).

view_cmd(view, html).
view_cmd(pdfview, pdf).
view_cmd(psview, ps).
view_cmd(htmlview, html).
view_cmd(infoview, info).
view_cmd(manlview, manl).

clean_cmd(clean, intermediate).
clean_cmd(docsclean, docs_no_texi).
clean_cmd(distclean, all_temporary).
clean_cmd(realclean, all).

error_bad_args(Args) :-
	format(user_error, "ERROR: illegal arguments: ~w~n", [Args]),
	format(user_error, "Use 'lpdoc help' for help.~n", []),
	halt(1).

% ---------------------------------------------------------------------------

lpdoc_cmd(version) :- !,
	version(Version),
	format(user_error, "LPdoc version ~w~n", [Version]).
lpdoc_cmd(toplevel) :- !,
	lpdoc_toplevel([]).
lpdoc_cmd(Cmd) :-
	opt_settings_file(ConfigFile),
	get_make_opts(Opts),
	% TODO: it may be better to use '_' here for output dir
	doc_cmd(ConfigFile, Opts, Cmd, '.').

% ---------------------------------------------------------------------------

% Command line options may be handled with:
%   is_option0/1, handle_option0/1
%   is_option1/1, handle_option1/2

% Options with 0 arguments
:- discontiguous(is_option0/1).
:- discontiguous(handle_option0/1).
% Options with 1 argument
:- discontiguous(is_option1/1).
:- discontiguous(handle_option1/2).

% ---------------------------------------------------------------------------
% Parsed options (for make_rt) 

:- data opt_settings_file/1.
:- data opt_name_value/2.
:- data opt_make_option/1.

reset_opts :-
	retractall_fact(opt_settings_file(_)),
	retractall_fact(opt_make_option(_)),
	retractall_fact(opt_name_value(_, _)),
	%
	assertz_fact(opt_settings_file('SETTINGS')).

get_make_opts(Opts) :- findall(O, opt(O), Opts).

opt(make_option(Opt)) :- opt_make_option(Opt).
opt(name_value(Name, Value)) :- opt_name_value(Name, Value).

% ---------------------------------------------------------------------------

is_option1('-d').
handle_option1('-d', NameValue) :-
	parse_name_value(NameValue, Name, Value),
	assertz_fact(opt_name_value(Name, Value)).

is_option0('-v').
handle_option0('-v') :-
	assertz_fact(opt_make_option('-v')).

is_option0('--trace-deps').
handle_option0('--trace-deps') :-
	assertz_fact(opt_make_option('--trace-deps')).

is_option1('-f').
handle_option1('-f', CfgFile) :-
	retractall_fact(opt_settings_file(_)),
	assertz_fact(opt_settings_file(CfgFile)).

% ===========================================================================

is_option0('-cv').
handle_option0('-cv') :- do_comment_version.
%
is_option0('--comment-version').
handle_option0('--comment-version') :- do_comment_version.
do_comment_version :-
	retractall_fact(opt_name_value(comment_version, _)),
	assertz_fact(opt_name_value(comment_version, yes)).

% ---------------------------------------------------------------------------

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

% ===========================================================================
:- doc(section, "Handle errors").

handle_lpdoc_error(error(system_error(E, Goal))) :- !,
	error_message("Execution of ~w returned ~w", [Goal, E]).
handle_lpdoc_error(error(Error)) :- !,
	error_message("Unknown error: ~w", [Error]).
handle_lpdoc_error(error(Error, Where)) :- !,
	handle_error(Error, Where).
handle_lpdoc_error(make_error(Format, Args)) :- !,
	error_message(Format, Args).
handle_lpdoc_error(Error) :-
	error_message("Unknown error: ~w", [Error]).

