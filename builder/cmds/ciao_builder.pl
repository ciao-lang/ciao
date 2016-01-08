:- module(_, [], [fsyntax, assertions, regtypes, dcg]).

:- doc(title, "The standalone command-line builder").
:- doc(author, "Ciao Development Team").
:- doc(author, "Jose F. Morales").

:- doc(module, "
@cindex{builder, standalone} @apl{ciao_builder} is the Ciao standalone
command-line builder. @apl{ciao_builder} can be used to manage Ciao
source code organized as @concept{bundle}s. 

This command is available to the user in two forms: wrapped around the
@tt{ciao-boot.sh} (or @tt{ciao-boot.bat} in Windows) scripts (see
@ref{Bootstrapping the build system}), or as part of the general
@apl{ciao} command-line tool.

@section{Introduction to the build process}

This picture shows a detailed view all the elements of the build and
installation process of a @concept{bundle} or collection of bundles.

@begin{verbatim}
    Phase 0-
       Bootstrapping the build system
    Phase 1-
       SOURCE ---(configure)--> CONFIG
    Phase 2- (needs CONFIG)
       SOURCE ---(build)--> BUILD --(install)--> INSTALLED
@end{verbatim}

Above, arrows specify actions and nodes are collections of files:

@begin{description}
@item{SOURCE} Source code
@item{CONFIG} Configuration of the source
@item{BUILD} Binaries, compiled libraries, and generated
  documentation (.pdf, .html, etc.)
@item{INSTALLED} Copies of BUILD that exclude temporary files (mostly
  caches for separate and incremental compilation).
@end{description}

The source distribution only contains SOURCE elements. The actions
@tt{build} and @tt{install} create the BUILD and INSTALLED elements.

NOTE: Bootstrapping is a transparent process and its details are not
needed to understand and use the build system. See @ref{Bootstrapping
the build system} for more details on @em{phase 0}.

@section{Configuration and build}

@begin{alert}
TODO: complete
@end{alert}

Configuration and build can be reverted with @em{clean} commands.  The
meaning of @tt{clean} and @tt{distclean} is based on their standard
meaning (see the @tt{Makefile} example at
@href{http://www.gnu.org/software/make/manual/html_node/Complex-Makefile.html}),
except for @tt{realclean} that cleans both the system and the
bootstrap (equivalent to @tt{distclean} and @tt{boot-clean}).

The following table summarizes the actions that @em{undo} each build
or install operation:

@begin{verbatim}
    ('undo' reverts the effect of each marked 'action') 
                  ,--------+-----------+-----------.
   actions \\ undo | clean  | distclean | realclean |
 .----------------+--------+-----------+-----------+
 | boot-build     |        |           |    x      |
 +----------------+--------+-----------+-----------+
 | configure      |        |     x     |    x      |
 +----------------+--------+-----------+-----------+
 | build          |   x    |     x     |    x      |
 `----------------+--------+-----------+-----------+
@end{verbatim}

@section{Installation}

Different installation areas are supported. For personal
installations, the installation area can overlap with the build
staging area, such that no extra space is necessary.

@begin{verbatim}

          .............         install             ...........
          .           .---------------------------->.         .
          .           .        (generated           .         .
          .............         files such          ...........
       Build Staging Area       as binaries)      Installation Area
               .^.                                 _
                | build                            /|
                |                                 /
          .............          install         /
          .           .-------------------------'
          .           .         (source files
          .............          such as examples,
           Source Code           images, etc.)
@end{verbatim}

The @tt{install} command is undone with the @tt{uninstall} command.

@section{Bootstrapping the build system}

The @tt{ciao-boot.sh} (or @tt{ciao-boot.bat} in Windows) automatically
bootstraps and invokes the Ciao build system. The process is described
below.

Bootstrapping is the process that compiles the Ciao compiler and
builder in an environment where no existing Ciao binary exists. We
follow the same @em{phase 1} and @em{phase 2} steps above but the
whole process is driven by a very simplified version written in
(portable) shell-script code (since there is no running Ciao) and
using the @tt{ciaoc} @em{bootstrap compiler}.

The @tt{ciaoc} bootstrap compiler comes in a pre-compiled bytecode
form. This bytecode, together with the engine parts written in C, can
be executed in most systems with a C compiler. 

Once this bootstrap compiler is available, the system compiles the
bootstrap @apl{ciao_builder}, which drives the rest of the build and
installation process.

This step is transparent to the user and separated in a different
build directory (@tt{build-boot/}). However, there are options to
force the recompilation and cleaning of that part (see
@tt{ciao-boot.sh} help for more information).").

:- doc(bug, "Avoid ciao_builder mix .po/.itf of bootstrap and final
   ciaoc; use CIAOCACHEDIR for @apl{ciao_builder} compilation,
   independently of whether CIAOCACHEDIR is used by the user.").

% ===========================================================================

:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(format), [format/3]).
:- use_module(library(errhandle), [handle_error/2]).
:- use_module(library(lists), [append/3]).
:- use_module(library(messages), [error_message/2]).

:- use_module(library(bundle/bundle_info), [root_bundle/1]).

:- use_module(ciaobld(builder_cmds), [builder_cmd/3, cleanup_builder/0]).
:- use_module(ciaobld(builder_aux), [bundle_at_dir/2, ciao_path_at_dir/2]).
:- use_module(ciaobld(ciaocl_help)).

% ===========================================================================

% Definitions for build and install directories:
%
% - builddir: directory to store configuration and compilation files
% - bindir: directory where command binaries are installed
% - storedir: directory where library files are installed
% - bundledir: directory inside storedir for a particular bundle
%
% The bootstrap is compiled and run from its own 'builddir'
% ('build-boot/'), which is different from the builddir of the system
% ('build/').

% ===========================================================================

% Invocation from the command-line interface
:- export(main/1).
main(Args) :-
	catch(main_(Args), E, handle_builder_error(E)),
	!.
    
main_([Help0|Args]) :-
	norm_cmd(Help0, Help),
	help_mode(Help, Mode, Boot), 
	!,
	( Args = [] ->
	    show_help(Mode, Boot)
	; Args = [Arg0] ->
	    parse_flag(Arg0, ciao:Flag), % (parse commands like flags)
	    show_help_cmd(Flag, Boot)
	; fail
	).
main_(CmdArgs) :- !,
	parse_args(CmdArgs, Cmd, Target, Opts),
	cleanup_builder, % TODO: just in case...
	builder_cmd(Cmd, Target, Opts),
	cleanup_builder.

help_mode('help', summary, normal).
help_mode('help_all', all, normal).
help_mode('help_boot', summary, boot).
help_mode('help_all_boot', all, boot).

% ===========================================================================
:- doc(section, "Parsing of builder commands from the CLI").

% The format is 'CMD ARGS', where:
%
%  - 'CMD' is a builder command (e.g., 'build') 
%  - 'ARGS' is a sequence of options (e.g., --interactive) and bundle
%    targets (e.g., 'lpdoc')
%
% Most commands only accept one target. Some commands do not accept
% options.

% From options (term) to CLI arguments (atoms)
opts_to_args([flag(Bundle:Name, Value)|Opts0], [Opt|Opts]) :- !,
	atom_concat(['--', Bundle, ':', Name, '=', Value], Opt),
	opts_to_args(Opts0, Opts).
opts_to_args([], []).

parse_args(CmdArgs, Cmd, Target, Opts) :-
	catch(parse_args_(CmdArgs, Cmd, Opts, Target),
              E, handle_builder_error(E)).

% Parse command, arguments, targets, and options
parse_args_([Cmd00|Args], Cmd2, Opts, Target) :- !,
	% Normalize command
	norm_cmd(Cmd00, Cmd0),
	( cmd_alias(Cmd0, Cmd1) -> Cmd = Cmd1
	; Cmd = Cmd0
	),
	% Parse
	parse_cmd(Cmd, Args, Cmd2, Opts, Target).
parse_args_([], _Cmd, _Opts, _Target) :-
	throw(args_error("No arguments were specified", [])).

parse_cmd(Cmd, Args, Cmd2, Opts, Target) :-
	check_cmd(Cmd),
	cmd_opts(Cmd, TargetParse, OptsParse),
	parse_opts(Args, Opts, Args1),
	% Select default target if none
	( TargetParse = arg_bundle ->
	    ( Args1 = [] -> bundle_at_dir('.', Target), Args2 = []
	    ; Args1 = [Target|Args2]
	    )
	; TargetParse = no_bundle -> % TODO: not very nice
	    Target = '$no_bundle',
	    Args2 = Args1
	; throw(invalid_target_parse(TargetParse))
	),
	( cmd_opts_custom(Cmd, Args2, Cmd2) ->
	    true
	; OptsParse = config_opts ->
	    % NOTE: checked later in configure
	    ( Args2 = [] -> true
	    ; throw(args_error("Invalid additional arguments ('~w')", [Args2]))
	    ),
	    Cmd2 = Cmd
	; OptsParse = raw_opts ->
	    ( Args2 = [] -> true
	    ; throw(args_error("Invalid additional arguments ('~w')", [Args2]))
	    ),
	    ( member(flag(Flag, _), Opts), % TODO: do not qualify in raw_opt
	      \+ raw_opt(Cmd, Flag) ->
	        throw(args_error("Unrecognized option '~w' for command '~w'", [Flag, Cmd]))
	    ; true
	    ),
	    Cmd2 = Cmd
	; OptsParse = no_opts ->
	    ( Args2 = [] -> true
	    ; throw(args_error("Invalid additional arguments ('~w')", [Args2]))
	    ),
	    ( Opts = [] ->
	        true
	    ; opts_to_args(Opts, Opts2),
	      throw(args_error("Unrecognized options '~w' for command '~w'", [Opts2, Cmd]))
	    ),
	    Cmd2 = Cmd
	; fail
	).

parse_opts(['--interactive'|Args], Opts, RestArgs) :- % TODO: special arg parser?
	!,
	Opts = [flag(ciao:interactive_config, true)|Opts0],
	parse_opts(Args, Opts0, RestArgs).
parse_opts(['--list-flags'|Args], Opts, RestArgs) :- % TODO: special arg parser?
	!,
	Opts = [flag(ciao:list_flags, true)|Opts0],
	parse_opts(Args, Opts0, RestArgs).
parse_opts(['--describe-flag', Param|Args], Opts, RestArgs) :- % TODO: special arg parser?
	!,
	parse_flag(Param, Flag),
	Flag = Bundle:_Name,
	RestArgs = [Bundle|RestArgs0],
	Opts = [flag(ciao:describe_flag, Flag)|Opts0],
	parse_opts(Args, Opts0, RestArgs0).
parse_opts(['--set-flag', Assign|Args], Opts, RestArgs) :- % TODO: special arg parser?
	!,
	parse_flag_assign_atm(Assign, Flag, Value),
	Flag = Bundle:_Name,
	RestArgs = [Bundle|RestArgs0],
	Opts = [flag(ciao:set_flag_flag, Flag),
	        flag(ciao:set_flag_value, Value)|Opts0],
	parse_opts(Args, Opts0, RestArgs0).
parse_opts([Assign0|Args], Opts, RestArgs) :-
	atom_concat('--', Assign, Assign0),
	parse_flag_assign_atm(Assign, Flag, Value),
	!,
	Opts = [flag(Flag, Value)|Opts0],
	parse_opts(Args, Opts0, RestArgs).
parse_opts([Arg|Args], Opts, RestArgs) :- !,
	RestArgs = [Arg|RestArgs0],
	parse_opts(Args, Opts, RestArgs0).
parse_opts([], [], []).

% Replace 0'- by 0'_ in names of commands
norm_cmd(X0, X) :-
	atom_codes(X0, Cs0),
	map(Cs0, normunderscore, Cs),
	atom_codes(X, Cs).

% Cmd is a valid command
check_cmd(Cmd) :-
	( is_builder_boot_cmd(Cmd) ->
	    throw(not_in_builder_boot(Cmd))
	; true
	),
	( is_builder_cmd(Cmd) ->
	    true
	; throw(unknown_builder_cmd(Cmd))
	).

:- regtype is_builder_boot_cmd/1 # "Commands exclusive for
   @tt{builder_boot.sh} (aka ciao-boot.sh)".

is_builder_boot_cmd(emergency_clean).
is_builder_boot_cmd(boot_build).
is_builder_boot_cmd(boot_rebuild).
is_builder_boot_cmd(boot_clean).
is_builder_boot_cmd(realclean).

% cmd_opts(Cmd, TargetParse, OptsParse):
%
%   Parsing specification for command Cmd, where:
%
%   TargetParse = arg_bundle  % target is the non-flag argument
%               | no_bundle  % no bundle target
%   OptsParse = config_opts % configuration flags
%             | raw_opts    % raw flags
%             | no_opts     % no flags

% TODO: ideas for TargetParse extensions:
%   - opt_bundle: use --bundle=BUNDLE argument
%   - def_bundle: like arg_bundle with no argument

cmd_opts(local_install,          arg_bundle, config_opts) :- !.
cmd_opts(global_install,         arg_bundle, config_opts) :- !.
cmd_opts(local_install_paranoid, arg_bundle, config_opts) :- !.
cmd_opts(configure,              arg_bundle, config_opts) :- !.
%
cmd_opts(rescan_bundles,         no_bundle, raw_opts) :- !.
cmd_opts(list,                   no_bundle, raw_opts) :- !.
cmd_opts(get,                    no_bundle, raw_opts) :- !.
% (internal)
cmd_opts(scan_and_config,        arg_bundle, config_opts) :- !.
cmd_opts(config_noscan,          arg_bundle, config_opts) :- !.
cmd_opts(config_list_flags,      arg_bundle, raw_opts) :- !.
cmd_opts(config_describe_flag,   arg_bundle, raw_opts) :- !.
cmd_opts(config_set_flag,        arg_bundle, raw_opts) :- !.
%
cmd_opts(install,                arg_bundle, raw_opts) :- !.
cmd_opts(uninstall,              arg_bundle, raw_opts) :- !.
%
cmd_opts(custom_run,             arg_bundle, raw_opts) :- !. % TODO: use def_bundle?
cmd_opts(gen_bundle_commit_info, arg_bundle, raw_opts) :- !.
%
cmd_opts(clean_tree,             no_bundle, raw_opts) :- !.
%
% (any other command: no flags)
cmd_opts(_,                      arg_bundle, no_opts).

% TODO: pass raw_opt as regular arguments (do not call set_params on them)
% NOTE: '--destdir' flag (staged installations) is used in: 
%   Portfile.skel
%   pbundle_gen_mac.pl
%   Ciao.spec.skel
%   and some code in ciaobot
raw_opt(install, ciao:destdir).
raw_opt(uninstall, ciao:destdir).
raw_opt(config_list_flags, ciao:list_flags).
raw_opt(config_describe_flag, ciao:describe_flag).
raw_opt(config_set_flag, ciao:set_flag_flag).
raw_opt(config_set_flag, ciao:set_flag_value).
%   --git-repo-dir=Dir: location of the Git repository (if not using default)
raw_opt(gen_bundle_commit_info, ciao:git_repo_dir).

cmd_opts_custom(rescan_bundles, Args2, Cmd2) :- !,
	% do not check flags % TODO: pass them as arguments instead?
	( Args2 = [File] -> true
	; Args2 = [] -> File = '.'
	; throw(args_error("'rescan_bundles' needs at most one path", []))
	),
	ciao_path_at_dir(File, Path),
	Cmd2 = rescan_bundles(Path).
cmd_opts_custom(get, Args2, Cmd2) :- !,
	% do not check flags
	( Args2 = [BundleAlias] -> true
	; throw(args_error("'get' needs a bundle alias", []))
	),
	Cmd2 = get(BundleAlias).
%
cmd_opts_custom(clean_tree, Args2, Cmd2) :- !,
	% do not check flags
	( Args2 = [Dir] -> true
	; throw(args_error("'clean_tree' needs a directory", []))
	),
	Cmd2 = clean_tree(Dir).
%
cmd_opts_custom(custom_run, Args2, Cmd2) :- !,
	% do not check flags % TODO: pass them as arguments instead?
	( Args2 = [CustomCmd] -> true
	; throw(args_error("'custom_run' needs a target and a custom command name", []))
	),
	Cmd2 = custom_run(CustomCmd).

% TODO: src and bin targets have WRONG names ('nothing' implies tgz and tbz)
cmd_alias(gen_pbundle__win32, gen_pbundle(win32)).
cmd_alias(gen_pbundle__descfile, gen_pbundle(descfile)).
cmd_alias(gen_pbundle__rpm, gen_pbundle(rpm)).
cmd_alias(gen_pbundle__src, gen_pbundle(src)).
cmd_alias(gen_pbundle__tgz, gen_pbundle(tgz)).
cmd_alias(gen_pbundle__tbz, gen_pbundle(tbz)).
cmd_alias(gen_pbundle__bin, gen_pbundle(bin)).
cmd_alias(gen_pbundle__bin_tgz, gen_pbundle(bin_tgz)).
cmd_alias(gen_pbundle__bin_tbz, gen_pbundle(bin_tbz)).
cmd_alias(gen_pbundle__noa, gen_pbundle(noa)).
cmd_alias(gen_pbundle__noa_tgz, gen_pbundle(noa_tgz)).
cmd_alias(gen_pbundle__noa_tbz, gen_pbundle(noa_tbz)).
cmd_alias(gen_pbundle__pkg, gen_pbundle(pkg)).
cmd_alias(gen_pbundle__macport, gen_pbundle(macport)).
cmd_alias(gen_pbundle__app, gen_pbundle(app)).

is_builder_cmd(help).
is_builder_cmd(help_all).
is_builder_cmd(boot_promote).
is_builder_cmd(local_install).
is_builder_cmd(global_install).
is_builder_cmd(local_install_paranoid).
is_builder_cmd(fullinstall).
is_builder_cmd(configure).
is_builder_cmd(config_noscan).
is_builder_cmd(rescan_bundles).
is_builder_cmd(scan_and_config).
is_builder_cmd(build).
is_builder_cmd(build_nodocs).
is_builder_cmd(build_libraries).
is_builder_cmd(build_bin).
is_builder_cmd(prebuild_nodocs).
is_builder_cmd(prebuild_docs).
is_builder_cmd(build_docs).
is_builder_cmd(build_docs_manuals).
is_builder_cmd(build_docs_readmes).
is_builder_cmd(clean).
is_builder_cmd(clean_nodocs).
is_builder_cmd(clean_norec).
is_builder_cmd(clean_docs).
is_builder_cmd(clean_docs_manuals).
is_builder_cmd(clean_docs_readmes).
is_builder_cmd(clean_tree).
is_builder_cmd(distclean).
is_builder_cmd(configclean).
is_builder_cmd(install).
is_builder_cmd(uninstall).
is_builder_cmd(install_docs).
is_builder_cmd(uninstall_docs).
is_builder_cmd(register).
is_builder_cmd(unregister).
is_builder_cmd(runbenchmarks).
is_builder_cmd(runtests).
%
is_builder_cmd(list). % (list bundles)
is_builder_cmd(info). % (info on bundle)
is_builder_cmd(get). % (download and install bundles)
%
is_builder_cmd(gen_bundle_commit_info). % ciao
is_builder_cmd(gen_pbundle(_)).
%
% temporary subtargets
is_builder_cmd(config_list_flags). % core (wrapped, for 'configure' with some flags)
is_builder_cmd(config_describe_flag). % core (wrapped, for 'configure' with some flags)
is_builder_cmd(config_set_flag). % core (wrapped, for 'configure' with some flags)
%
% TODO: (custom hooks)
is_builder_cmd(custom_run).
%
% TODO: add as build/clean special targets?
is_builder_cmd(create_tags). % common
is_builder_cmd(delete_tags). % common

% ---------------------------------------------------------------------------

:- use_module(library(lists), [list_concat/2]).

% Parse a (maybe qualified) flag name
parse_flag(Param, Flag) :-
	atom_codes(Param, ParamS),
	parse_flag_(ParamS, Flag).

parse_flag_(ParamS, Bundle:Name) :-
	( list_concat([BundleS, ":", NameS], ParamS) ->
	    atom_codes(Bundle, BundleS)
	; root_bundle(Bundle), % default bundle % TODO: use cwd_bundle?
	  NameS = ParamS
	),
	map(NameS, normalizecode, ParamS2),
	atom_codes(Name, ParamS2).

% Parse a (maybe qualified) flag assignment (value as atom)
parse_flag_assign_atm(Param, Flag, Value) :-
	parse_flag_assign_str(Param, Flag, ValueS),
	atom_codes(Value, ValueS).

% Parse a (maybe qualified) flag assignment (value as string)
parse_flag_assign_str(Assign, Flag, ValueS) :-
	atom_codes(Assign, AssignS),
	list_concat([ParamS, "=", ValueS], AssignS),
	!,
	parse_flag_(ParamS, Flag).

:- use_module(library(hiordlib), [map/3]).

normalizecode(X0,X) :- tolowercode(X0,X1), normunderscore(X1,X).

normunderscore(0'-, 0'_) :- !.
normunderscore(C,   C).

tolowercode(C, U) :-
	0'A =< C,
	C =< 0'Z,
	!,
	U is C + 0'a - 0'A.
tolowercode(C, C).

% ===========================================================================
:- doc(section, "Handle errors").

handle_builder_error(args_error(Format, Args)) :-
	append("ERROR: ", Format, T1),
	append(T1, "~n", T2),
	format(user_error, T2, Args),
	halt(1).
handle_builder_error(make_error(Format, Args)) :- % TODO: used?
	error_message(Format, Args),
	halt(1).
handle_builder_error(not_in_builder_boot(Cmd)) :-
	format(user_error, "ERROR: Command '~w' only available in 'ciao-boot.sh' or 'ciao-boot.bat'.~n", [Cmd]),
	halt(1).
handle_builder_error(unknown_bundle(Bundle)) :-
	format(user_error, "ERROR: Unknown bundle '~w' (try 'rescan-bundles').~n", [Bundle]),
	halt(1).
handle_builder_error(unknown_builder_cmd(Cmd)) :-
	format(user_error, "ERROR: Unknown command '~w'.~n", [Cmd]),
	halt(1).
handle_builder_error(builder_cmd_failed(Bundle, '', Target)) :- !,
	format(user_error, "ERROR: Command '~w' on bundle '~w' failed.~n", [Target, Bundle]),
	halt(1).
handle_builder_error(builder_cmd_failed(Bundle, Part, Target)) :-
	format(user_error, "ERROR: Command '~w' on bundle '~w' (part '~w') failed.~n", [Target, Bundle, Part]),
	halt(1).
handle_builder_error(error(Error, Where)) :-
	handle_error(Error, Where).
handle_builder_error(Error) :-
	format(user_error, "ERROR: Unknown error '~w'.~n", [Error]),
	halt(1).

