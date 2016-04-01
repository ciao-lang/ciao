:- module(lpdoccl, [], [assertions, dcg]).

:- doc(title,"The LPdoc top-level and command-line interface").
:- doc(author, "Manuel Hermenegildo").
:- doc(author, "Jos@'{e} Francisco Morales").

:- doc(module, "This is the top-level and command-line interface to
   LPdoc. Please look at @lib{lpdoc} documentation for top-level usage
   information. The command-line interface allows the use of the
   system in batch mode, using arguments for specifiying documentation
   setting, targets, and actions.

@section{Usage (lpdoc)}

The following provides details on the different command line options
available when invoking @apl{lpdoc}:

@sp{2}

@begin{verbatim}
@includefact{usage_message/1}
@end{verbatim}
").

:- use_module(library(errhandle), [handle_error/2]).
:- use_module(library(messages), [error_message/2]).
:- use_module(library(format), [format/3]).
:- use_module(library(toplevel), [toplevel/1]). % (for built-in toplevel)

:- use_module(lpdoc(docmaker), [doc_cmd/3]).

usage_message("\
lpdoc [<opts>] <input>

Generates documentation for the given input

General options:

  -h|--help              Show help
  --version              Show version and exit
  -T                     Start a LPdoc toplevel (single option)

  -v                     Verbose
  --trace-deps           Trace dependencies (for debugging)

  --Name=Value           Set or override the option Name (doccfg)

  -t TARGET              Format (pdf|ps|html|info|manl)

Options to view or clean the documentation output:

  --view                 Open documentation in selected format

  --clean                Clean all except .texi and targets (E.g., .pdf)"|| /*intermediate*/ "
  --docsclean            Clean all except .texi output"|| /* intermediate, temp_no_texi */ "
  --distclean            Clean all except final output (e.g., .pdf)"|| /* intermediate, texi*/ "
  --realclean            Clean all generated files"|| /* intermediate, temp_no_texi, texi*/ "
").

% Invocation from the command-line interface
:- export(main/1).
main(Args) :-
	catch(main_(Args), E, (handle_lpdoc_error(E), halt(1))).

main_(Args) :-
	reset_opts,
	parse_opts(Args, Rest),
	select_cmd(Cmd),
	check_args(Cmd, Rest),
	!,
	lpdoc_cmd(Cmd, Rest).
main_(Args) :-
	error_bad_args(Args).

error_bad_args(Args) :-
	throw(autodoc_error("Illegal arguments: ~w~n"||
			    "Use 'lpdoc --help' for help.~n", [Args])).

select_cmd(Cmd) :-
	( opt_mode(help) -> Cmd = help
	; opt_mode(version) -> Cmd = version
	; opt_mode(toplevel) -> Cmd = toplevel
	; opt_mode(clean(Mode)) -> Cmd = clean(Mode)
	; opt_mode(view) -> view_target(Target), Cmd = view(Target)
	; gen_target(Target), Cmd = gen(Target)
	).

check_args(help, []).
check_args(version, []).
check_args(toplevel, []).
check_args(clean(_), [_]). % TODO: fixme, InFile not used?
check_args(view(_), [_]).
check_args(gen(_), [_]).

default_gen_target(all). % TODO: sure?

default_view_target(html).

gen_target(Target) :-
	( opt_target(Target) -> true
	; default_gen_target(Target)
	).

view_target(Target) :-
	( opt_target(Target) -> true
	; default_view_target(Target)
	).

lpdoc_cmd(help, _) :- !, usage.
lpdoc_cmd(version, _) :- !,
	version(Version),
	format(user_error, "LPdoc version ~w~n", [Version]).
lpdoc_cmd(toplevel, _) :- !,
	lpdoc_toplevel([]).
lpdoc_cmd(Cmd, [InFile]) :-
	get_opts(Opts),
	doc_cmd(InFile, Opts, Cmd).

% ---------------------------------------------------------------------------

:- include(lpdoc(version_auto)). % Version information

usage :-
	usage_message(Str),
	message([$$(Str)]).

% ---------------------------------------------------------------------------

:- use_module(library(hiordlib), [map/3]).

% Parse options and normal arguments
parse_opts([Opt|Args], Rest) :-
	is_option0(Opt),
	!,
	handle_option0(Opt),
	parse_opts(Args, Rest).
parse_opts([Opt|Args], Rest) :-
	parse_name_value(Opt, Name, Value),
	!,
	handle_name_value(Name, Value),
	parse_opts(Args, Rest).
parse_opts([Opt, Arg|Args], Rest) :-
	is_option1(Opt),
	!,
	handle_option1(Opt, Arg),
	parse_opts(Args, Rest).
parse_opts([Arg|Args], Rest) :- !,
	Rest = [Arg|Rest0],
	parse_opts(Args, Rest0).
parse_opts([], []).

parse_name_value(NameValue, Name, Value) :-
	parse_name_value_string(NameValue, Name, ValueS),
	atom_codes(Value, ValueS).

parse_name_value_string(NameValue, Name, ValueS) :-
	atom_codes(NameValue, NameValueS),
	list_concat(["--", NameS, "=", ValueS], NameValueS),
	!,
	norm_name(NameS, NameS2),
	atom_codes(Name, NameS2).

% Replace 0'- by 0'_ in names of flags
norm_name(Cs0, Cs) :-
	map(Cs0, normunderscore, Cs).

normunderscore(0'-, 0'_) :- !.
normunderscore(C,   C).

% ---------------------------------------------------------------------------

% Command line options may be handled with:
%   is_option0/1, handle_option0/1
%   is_option1/1, handle_option1/2
%   handle_name_value/2

% Options with 0 arguments
:- discontiguous(is_option0/1).
:- discontiguous(handle_option0/1).
% Options with 1 argument
:- discontiguous(is_option1/1).
:- discontiguous(handle_option1/2).
% --Name=Value
:- discontiguous(handle_name_value/2).

% ---------------------------------------------------------------------------
% Parsed options

:- use_module(library(aggregates), [findall/3]).

:- data opt_target/1.
:- data opt_mode/1.
:- data opt_name_value/2.
:- data opt_autodoc_option/1.

reset_opts :-
	retractall_fact(opt_target(_)),
	retractall_fact(opt_mode(_)),
	retractall_fact(opt_autodoc_option(_)),
	retractall_fact(opt_name_value(_, _)).

get_opts(Opts) :- findall(O, opt(O), Opts).

opt(autodoc_option(Opt)) :- opt_autodoc_option(Opt).
opt(name_value(Name, Value)) :- opt_name_value(Name, Value).

% ---------------------------------------------------------------------------

:- use_module(library(lists), [list_concat/2]).

is_option0('-h').
handle_option0('-h') :-
	assertz_fact(opt_mode(help)).

is_option0('--help').
handle_option0('--help') :-
	assertz_fact(opt_mode(help)).

is_option0('--version').
handle_option0('--version') :-
	assertz_fact(opt_mode(version)).

is_option0('-T').
handle_option0('-T') :-
	assertz_fact(opt_mode(toplevel)).

is_option0('-v').
handle_option0('-v') :-
	assertz_fact(opt_autodoc_option('-v')).

is_option0('--trace-deps').
handle_option0('--trace-deps') :-
	assertz_fact(opt_autodoc_option('--trace-deps')).

is_option1('-t').
handle_option1('-t', Target) :-
	retractall_fact(opt_target(_)),
	assertz_fact(opt_target(Target)).

is_option0('--view').
handle_option0('--view') :-
	assertz_fact(opt_mode(view)).

is_option0(X) :- clean_cmd(X, _).
handle_option0(X) :- clean_cmd(X, Mode), !,
	assertz_fact(opt_mode(clean(Mode))).

handle_name_value(Name, Value) :-
	assertz_fact(opt_name_value(Name, Value)).

clean_cmd('--clean', intermediate).
clean_cmd('--docsclean', docs_no_texi).
clean_cmd('--distclean', all_temporary).
clean_cmd('--realclean', all).

% ---------------------------------------------------------------------------

:- use_module(library(system), [file_exists/1]).

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

% ---------------------------------------------------------------------------
:- doc(section, "Handle errors").

handle_lpdoc_error(error(system_error(E, Goal))) :- !,
	error_message("Execution of ~w returned ~w", [Goal, E]).
handle_lpdoc_error(error(Error)) :- !,
	error_message("Unknown error: ~w", [Error]).
handle_lpdoc_error(error(Error, Where)) :- !,
	handle_error(Error, Where).
handle_lpdoc_error(autodoc_error(Format, Args)) :- !,
	error_message(Format, Args).
handle_lpdoc_error(Error) :-
	error_message("Unknown error: ~w", [Error]).

