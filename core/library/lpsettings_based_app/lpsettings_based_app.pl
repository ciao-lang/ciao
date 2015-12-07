% ===========================================================================
% Common definitions and main/1 entry point for an application using a
% command line interface, lpmake package and SETTINGS.pl
%
% Author: Jose F. Morales based on code from 'lpdoc'
%
% ===========================================================================
%
% The user must include this file in its module and and define the
% following entry points:
%
%   startup/1
%   app_name/1
%   app_copyright/1
%   app_options_message/1
%
% Command line options may be handled with:
%   is_option0/1, handle_option0/1
%   is_option1/1, handle_option1/2
%
% ---------------------------------------------------------------------------
% TODO: merge with other package to get options
% ---------------------------------------------------------------------------

% (dependencies of lpsettings_based_app)
:- use_module(library(errhandle), [handle_error/2]).
:- use_module(library(messages), [error_message/2]).
:- use_module(library(format), [format/3, sformat/3]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(system), [working_directory/2, file_exists/1]).
:- use_module(library(make/make_rt), [parse_name_value/3]).

:- multifile m_target_comment/4. % TODO: access via make_rt

% ---------------------------------------------------------------------------

main(Args) :-
	startup(Args).

startup(Args) :-
	reset_make_opts,
	catch(handle_args(Args), E, (my_handle_error(E), fail)).

my_handle_error(error(system_error(E, Goal))) :- !,
	error_message("Execution of ~w returned ~w", [Goal, E]).
my_handle_error(error(Error)) :- !,
	error_message("Unknown error: ~w", [Error]).
my_handle_error(error(Error, Where)) :- !,
	handle_error(Error, Where).
my_handle_error(make_error(Format, Args)) :- !,
	error_message(Format, Args).
my_handle_error(Error) :-
	error_message("Unknown error: ~w", [Error]).

% ---------------------------------------------------------------------------
% Parse command line arguments

handle_args([X]) :- help_option(X), !, report_usage.
handle_args([X]) :- version_option(X), !, report_version.
handle_args(Args) :-
	handle_args_(Args, Targets), !,
	opt_settings_file(ConfigFile),
	get_make_opts(Opts),
	start(ConfigFile, Opts, Targets).
handle_args(Args) :-
	format(user_error, "~nIllegal arguments: ~w~n~n", [Args]),
	report_usage.

handle_args_([Opt|Args], Targets) :-
	is_option0(Opt),
	!,
	handle_option0(Opt),
	handle_args_(Args, Targets).
handle_args_([Opt, Arg|Args], Targets) :-
	is_option1(Opt),
	!,
	handle_option1(Opt, Arg),
	handle_args_(Args, Targets).
handle_args_(Args, Targets) :- !,
	Targets = Args.

% Options with 0 arguments
:- discontiguous(is_option0/1).
:- discontiguous(handle_option0/1).
% Options with 1 argument
:- discontiguous(is_option1/1).
:- discontiguous(handle_option1/2).

help_option('-h').
help_option('--help').

version_option('--version').

% ---------------------------------------------------------------------------
% Parsed options (for make_rt) 

:- data opt_settings_file/1.
:- data opt_name_value/2.
:- data opt_make_option/1.

reset_make_opts :-
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

% ---------------------------------------------------------------------------
% Help messages

% This is in narrow format because that way it looks nicer in a man page.
common_options_message("
Help options:

-h,--help       Print this help message.
--version       Print version

General options:

-v              Verbose
--trace-deps    Trace dependencies (for debugging)
-f FILE         Uses file FILE as configuration file. Default is SETTINGS.pl.

-d Name=Value   Indicates that a variable binding Name=Value follows.
                If Name is one of the options used, then this will
                override the value defined in the configuration file.
                To get the value of this option in your program, simply
                call the 'name_value(Name, Value).'  (i.e., 'name_value(Name)
                := Value.') predicate, defined in the module
                library(make/make_rt). 
").

% ---------------------------------------------------------------------------

report_version :-
	version(Version),
	app_name(AppName),
	format(user_error, "~w ~w~n", [AppName, Version]),
	app_copyright(Copyright),
	format(user_error, "~s~n", [Copyright]).

report_usage :-
	report_version,
	report_usage_text,
	report_commands.

report_usage_text :-
	% TODO: Why not using lpdoc itself to format this?
	format(user_error, "~nUsage:~n", []),
	app_name(AppName),
	common_options_message(Text2),
	app_options_message(Text3),
	format(user_error, "~w [OPTIONS] COMMANDS~n", [AppName]),
	format(user_error, "~nProcesses each command using the specified options.~n~n", []),
	format(user_error, Text2, []),
	format(user_error, Text3, []).

report_commands :-
	format(user_error, "~nSupported commands:~n", []),
	findall(tc(Target, Comment),
	    target_comment_action(Target, Comment),
	    TCs),
	( member(tc(Target, Comment), TCs),
	    format(user_error, "    ~w~n    ~s~n~n", [Target, Comment]),
	    fail
	; true
	).

target_comment_action(Target, SComment) :-
	m_target_comment(_AC, Target, Comment, Args),
	sformat(SComment, Comment, Args).

% TODO: This should be consulted in the documentation, not here!
%% report_index_names :-
%% 	format(user_error, "~nAcceptable index names:~n", []),
%% 	( index_comment(Index, IText),
%% 	    format(user_error, "    ~w~n    ~s~n~n", [Index, IText]),
%% 	    fail
%% 	; true ).
%% 
%% report_additional_options :-
%% 	format(user_error, "~nAdditional options (MiscOpts):~n", []),
%% 	( option_comment(Option, OText),
%% 	    format(user_error, "    ~w~n    ~s~n~n", [Option, OText]),
%% 	    fail
%% 	; true
%% 	).

