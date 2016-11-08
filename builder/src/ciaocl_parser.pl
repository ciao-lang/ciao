:- module(_, [], [assertions, isomodes, regtypes, dcg]).

:- doc(title, "Ciao Command-line Parser").
:- doc(author, "Ciao Development Team").
:- doc(author, "Jose F. Morales").

:- doc(module, "Parser of builder commands (based on a generic command line parser).").

:- use_module(library(lists), [select/3]).

% (exports parse_cmd/3)
:- include(library(cmdline/cmdline_parser)).

:- export(norm_underscores/2). % defined in cmdline_parser.pl

:- use_module(library(bundle/bundle_info), [root_bundle/1]).

% Commands exclusive for @tt{builder_boot.sh} (aka ciao-boot.sh)
cmd_fmt(emergency_clean, [raw_args]).
cmd_rw(emergency_clean, _Cmd2, _Opts, _Opts2, _Args, _Args2, _CmdFmt2) :- !,
	throw(not_in_builder_boot(emergency_clean)).
%
cmd_fmt(boot_build, [raw_args]).
cmd_rw(boot_build, _Cmd2, _Opts, _Opts2, _Args, _Args2, _CmdFmt2) :- !,
	throw(not_in_builder_boot(boot_build)).
%
cmd_fmt(boot_rebuild, [raw_args]).
cmd_rw(boot_rebuild, _Cmd2, _Opts, _Opts2, _Args, _Args2, _CmdFmt2) :- !,
	throw(not_in_builder_boot(boot_rebuild)).
%
cmd_fmt(boot_clean, [raw_args]).
cmd_rw(boot_clean, _Cmd2, _Opts, _Opts2, _Args, _Args2, _CmdFmt2) :- !,
	throw(not_in_builder_boot(boot_clean)).
%
cmd_fmt(realclean, [raw_args]).
cmd_rw(realclean, _Cmd2, _Opts, _Opts2, _Args, _Args2, _CmdFmt2) :- !,
	throw(not_in_builder_boot(realclean)).

cmd_fmt(boot_promote, [target_args]).

cmd_fmt(local_install, [opts([interactive]), target_args, config_flags]).
cmd_fmt(global_install, [opts([interactive]), target_args, config_flags]).
cmd_fmt(local_install_paranoid, [opts([interactive]), target_args, config_flags]).
cmd_fmt(full_install, [opts([interactive]), target_args, config_flags]).

cmd_fmt(rescan_bundles, [target_args]).

cmd_fmt(configure, [
           opts([interactive,
                 list_flags,
                 (describe_flag,f),
                 (get_flag,f),
                 (set_flag,f=v)
               ]),
	   target_args,
           config_flags]).
cmd_rw(configure([]), Cmd2, Opts, Opts2, Args, Args2, CmdFmt2) :- !,
	( select(opt(list_flags), Opts, Opts1) ->
	    Cmd2 = config_list_flags,
	    Opts2 = Opts1,
	    Args2 = Args,
	    CmdFmt2 = [target_args]
	; select(opt(describe_flag, Flag), Opts, Opts1) ->
	    Args = [],
	    Cmd2 = config_describe_flag(Flag),
	    Opts2 = Opts1,
	    CmdFmt2 = [target_arg],
	    Flag = Bundle:_, Args2 = [Bundle]
	; select(opt(get_flag, Flag), Opts, Opts1) ->
	    Args = [],
	    Cmd2 = config_describe_flag(Flag),
	    Opts2 = Opts1,
	    CmdFmt2 = [target_arg],
	    Flag = Bundle:_, Args2 = [Bundle]
	; select(opt(set_flag, Flag=Value), Opts, Opts1) ->
	    Args = [],
	    Cmd2 = config_set_flag(Flag, Value),
	    Opts2 = Opts1,
	    CmdFmt2 = [target_arg],
	    Flag = Bundle:_, Args2 = [Bundle]
	; fail
	).

cmd_fmt(build, [target_args]).
cmd_fmt(build_nodocs, [target_args]).
cmd_fmt(prebuild_nodocs, [target_args]).
cmd_fmt(prebuild_docs, [target_args]).
cmd_fmt(build_docs, [target_args]).
cmd_fmt(build_docs_manuals, [target_args]).
cmd_fmt(build_docs_readmes, [target_args]).
cmd_fmt(clean, [target_args]).
cmd_fmt(clean_nodocs, [target_args]).
cmd_fmt(clean_norec, [target_args]).
cmd_fmt(clean_docs, [target_args]).
cmd_fmt(clean_docs_manuals, [target_args]).
cmd_fmt(clean_docs_readmes, [target_args]).
cmd_fmt(distclean, [target_args]).
cmd_fmt(configclean, [target_args]).

cmd_fmt(install, [opts([
  destdir=v % --destdir=Dir: prepend to each final installation dir
	    % (do not confuse with 'prefix')
]), target_args]).
cmd_fmt(uninstall, [opts([destdir=v]), target_args]).
cmd_fmt(install_docs, [target_args]).
cmd_fmt(uninstall_docs, [target_args]).
cmd_fmt(register, [target_args]).
cmd_fmt(unregister, [target_args]).
cmd_fmt(runbenchmarks, [target_args]).
cmd_fmt(test, [target_args]).

cmd_fmt(list, []). % (list bundles)
cmd_fmt(info, [target_args]). % (info on bundle)

% TODO: add a separate 'fetch' command? (only downloads)
cmd_fmt(get, [target_args, config_flags]). % (download and install bundles)

cmd_fmt(rm, [target_args]). % (remove downloaded bundles)

cmd_fmt(gen_bundle_commit_info, [opts([
  git_repo_dir=v % --git-repo-dir=Dir: location of the Git repository
		 % (if not using default)
]), target_arg]).

cmd_fmt(gen_pbundle, [opts([
  kind=v % --kind=Kind: kind of packaged bundle
]), target_arg]).

cmd_fmt(clean_tree, [raw_args]).
cmd_rw(clean_tree, Cmd2, Opts, Opts2, Args, Args2, CmdFmt2) :- !,
	( Args = [Dir] -> true
	; throw(error_msg("'clean_tree' needs a directory", []))
	),
	Cmd2 = clean_tree(Dir),
	Opts2 = Opts, Args2 = [],
	CmdFmt2 = [].

cmd_fmt(custom_run, [raw_args]). % TODO: use def_bundle?
cmd_rw(custom_run, Cmd2, Opts, Opts2, Args, Args2, CmdFmt2) :- !,
	( Args = [Target, CustomCmd|CustomArgs] -> true
	; throw(error_msg("'custom_run' needs a target and a custom command name and arguments", []))
	),
	Cmd2 = custom_run(CustomCmd, CustomArgs),
	Opts2 = Opts, Args2 = [Target],
	CmdFmt2 = [target_arg].

