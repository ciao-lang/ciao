:- module(_, [], [assertions, isomodes, regtypes, dcg]).

:- doc(title, "Ciao command-line parser").
:- doc(author, "Jose F. Morales").
:- doc(author, "The Ciao Development Team").

:- doc(module, "Parser of builder commands (based on a generic command line parser).").

:- use_module(library(lists), [select/3]).

% (exports parse_cmd/3)
:- include(library(cmdline/cmdline_parser)).

:- export(norm_underscores/2). % defined in cmdline_parser.pl

% default qualifier for flags
default_flag_qual('builder'). % TODO: use default target too? do search?

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
cmd_rw(local_install(Flags), Cmd2, Opts, Opts2, Args, Args2, CmdFmt2) :- !,
    Flags2 = [flag(builder:instype, 'local')|Flags],
    Cmd2 = full_install(Flags2),
    Opts2 = Opts, Args2 = Args,
    CmdFmt2 = [target_args].
%
cmd_fmt(global_install, [opts([interactive]), target_args, config_flags]).
cmd_rw(global_install(Flags), Cmd2, Opts, Opts2, Args, Args2, CmdFmt2) :- !,
    Flags2 = [flag(builder:instype, 'global')|Flags],
    Cmd2 = full_install(Flags2),
    Opts2 = Opts, Args2 = Args,
    CmdFmt2 = [target_args].
%
cmd_fmt(local_install_paranoid, [opts([interactive]), target_args, config_flags]).
cmd_rw(local_install_paranoid(Flags), Cmd2, Opts, Opts2, Args, Args2, CmdFmt2) :- !,
    Flags2 = [flag(builder:instype, 'local')
              /*,flag(builder:unused_pred_warnings, 'yes')*/|Flags], % TODO: enable lints, style checks, analysis, etc.?
    Cmd2 = full_install(Flags2),
    Opts2 = Opts, Args2 = Args,
    CmdFmt2 = [target_args].

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
        Cmd2 = config_get_flag(Flag),
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

common_opts([
  s(r),    % -r: recursive (same worksace)
  s(x),    % -x: recursive (all dependencies)
  grade=v, % --grade=Grade: select grade
  bin,     % --bin: exclude documentation build (alias for --grade=bin)
  docs     % --docs: just build documentation (alias for --grade=docs)
]).

cmd_fmt(build, [opts(Opts), target_args]) :- common_opts(Opts).
cmd_fmt(clean, [opts(Opts), target_args]) :- common_opts(Opts).
cmd_fmt(distclean, [target_args]).
cmd_fmt(configclean, [target_args]).

install_opts([
  destdir=v % --destdir=Dir: prepend to each final installation dir
        % (do not confuse with 'prefix')
|Opts]) :- common_opts(Opts).

cmd_fmt(install, [opts(Opts), target_args]) :- install_opts(Opts).
cmd_fmt(uninstall, [opts(Opts), target_args]) :- install_opts(Opts).
cmd_fmt(register, [target_args]).
cmd_fmt(unregister, [target_args]).
cmd_fmt(bench, [target_args]).
cmd_fmt(test, [target_args]).
cmd_fmt(analyze, [target_args]).

cmd_fmt(list, []). % (list bundles)
cmd_fmt(info, [target_args]). % (info on bundle)

cmd_fmt(fetch, [target_args]). % (download bundles)
cmd_fmt(rm, [target_args]). % (remove downloaded bundles)

cmd_fmt(get, [target_args, config_flags]). % (download and install bundles)

cmd_fmt(gen_commit_info, [opts([
  git_repo_dir=v % --git-repo-dir=Dir: location of the Git repository
             % (if not using default)
]), target_args]).

cmd_fmt(gen_pbundle, [opts([
  kind=v % --kind=Kind: kind of packaged bundle
]), target_args]).

cmd_fmt(clean_tree, [raw_args]).
cmd_rw(clean_tree, Cmd2, Opts, Opts2, Args, Args2, CmdFmt2) :- !,
    ( Args = [Dir] -> true
    ; throw(error_msg("'clean_tree' needs a directory", []))
    ),
    Cmd2 = clean_tree(Dir),
    Opts2 = Opts, Args2 = [],
    CmdFmt2 = [].

cmd_fmt(doc, [target_args]). % (info on bundle)

cmd_fmt(custom_run, [raw_args]). % TODO: use def_bundle?
cmd_rw(custom_run, Cmd2, Opts, Opts2, Args, Args2, CmdFmt2) :- !,
    ( Args = [Target, CustomCmd|CustomArgs] -> true
    ; throw(error_msg("'custom_run' needs a target and a custom command name and arguments", []))
    ),
    Cmd2 = custom_run(CustomCmd, CustomArgs),
    Opts2 = Opts, Args2 = [Target],
    CmdFmt2 = [target_arg].

cmd_fmt(third_party_install, [raw_args]). % TODO: allow others?
cmd_rw(third_party_install, Cmd2, Opts, Opts2, Args, Args2, CmdFmt2) :- !,
    ( Args = [Target|CustomArgs] -> true
    ; throw(error_msg("'third_party_install' needs a target", []))
    ),
    Cmd2 = third_party_install(CustomArgs),
    Opts2 = Opts, Args2 = [Target],
    CmdFmt2 = [target_arg].

