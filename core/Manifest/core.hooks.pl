:- module(_, [], [dcg, ciaobld(bundlehooks)]).

:- doc(title,  "Bundle Hooks for Ciao core").

% ---------------------------------------------------------------------------

% Command-line environment
'$builder_hook'(item_nested(dot_shell)).
:- include(.('dot_shell.hooks')).

% PiLLoW
'$builder_hook'(item_nested(pillow)).
:- include(.('pillow.hooks')).

% ---------------------------------------------------------------------------
% Libraries and manuals

'$builder_hook'(lib('lib')).
% WARNING: Ciao cannot compile 'clpq' and 'clpr' together.
%   We build each of 'clpq' and 'clpr' as separate processes. They
%   cannot be compiled together since the translation module uses the
%   constraint solver itself and the attributed variable hooks are
%   mixed.
%
% TODO: obtain a more recent version of clpq,clpr (this is not
%   the latest), adapt to Ciao and port to multiattributes (or
%   qualify each term).
'$builder_hook'(lib_force_build('library/clpq')). % See WARNING note above
'$builder_hook'(lib_force_build('library/clpr')). % See WARNING note above
'$builder_hook'(lib('library')).
'$builder_hook'(src('examples')).
%
'$builder_hook'(manual('ciao_internals', [main='doc/internals/SETTINGS.pl'])).

% ---------------------------------------------------------------------------
% ciaobase: the minimum part needed for @apl{ciao_builder} to compile
% the rest of the system.
%
% NOTE: This target is invoked implicitly by the builder when needed.

% NOTE: each item contains 'item_dep's to enforce a precise build order:
%   engine < exec_header < ciaoc < toplevel

% (engine needs bootstrap ciaoc)
'$builder_hook'(item_nested(ciaobase)). % (Enforced by the builder for any build_bin command)
'$builder_hook'(ciaobase:item_dep(engine)). % (needs bootstrap ciaoc)
'$builder_hook'(ciaobase:item_dep(exec_header_)).
'$builder_hook'(ciaobase:item_dep(ciaoc_)). % (depends on engine and exec_header)
'$builder_hook'(ciaobase:item_dep(shell_)).
%
'$builder_hook'(exec_header_:item_alias(exec_header)).
'$builder_hook'(exec_header_:item_dep(engine)).
%
'$builder_hook'(ciaoc_:item_alias(ciaoc)).
'$builder_hook'(ciaoc_:item_dep(exec_header_)).
'$builder_hook'(ciaoc_:item_dep(engine)).
%
'$builder_hook'(shell_:item_alias(shell)).
'$builder_hook'(shell_:item_dep(ciaoc_)).

% ---------------------------------------------------------------------------
% Enumeration of the standalone utilities in */cmds/

'$builder_hook'(item_nested(core_cmds)).
'$builder_hook'(core_cmds:cmd('cmds/ciaodump')).
'$builder_hook'(core_cmds:cmd('cmds/pldiff')).
'$builder_hook'(core_cmds:cmd('cmds/ciaoc_sdyn')).

% ---------------------------------------------------------------------------
% Standalone compiler

'$builder_hook'(item_nested(ciaoc)).
:- include(.('ciaoc.hooks')).

% ---------------------------------------------------------------------------
% Interactive toplevel (ciaosh) and ciao-script runtime (ciao-shell)

'$builder_hook'(item_nested(shell)).
'$builder_hook'(shell:cmd('ciaosh', [main='shell/ciaosh'])).
'$builder_hook'(shell:cmd('ciao-shell', [main='shell/ciao-shell', static])).

% ---------------------------------------------------------------------------
% Header for executables
% TODO: merge with do_exe_header code?

'$builder_hook'(item_nested(exec_header)).
'$builder_hook'(exec_header:eng_exec_header(eng('engine/ciaoengine', []))).

% ---------------------------------------------------------------------------
% The 'ciao' super-command

% Option for ciaocl
:- bundle_flag(install_prolog_name, [
    comment("Symbolic link from Ciao to 'prolog' executable"),
    details(
      % .....................................................................
      "Set to \"yes\" if you wish to create a link that brings up Ciao \n"||
      "when you type \"prolog\". You may want to say no if there are other\n"||
      "systems that support the Prolog language in your machine and you\n"||
      "do not want to make Ciao the default."),
    valid_values(['yes', 'no']),
    %
    rule_default('yes'),
    %
    interactive
]).

:- use_module(ciaobld(eng_defs), [bld_eng_path/3]).
:- use_module(ciaobld(config_common), [boot_eng_def/1, cmdname_ver/5]).
:- use_module(ciaobld(builder_aux), [builddir_bin_link_as/4]).
:- use_module(library(bundle/bundle_flags), [get_bundle_flag/2]).
:- use_module(library(bundle/bundle_paths), [bundle_path/3, bundle_path/4]).

install_prolog_name := ~get_bundle_flag(core:install_prolog_name).

%
'$builder_hook'(item_nested(ciaocl)).
% (definition for installation)
% (merge below?)
%'$builder_hook'(ciaocl:cmd('ciao', [main='NONE_AUTOGEN', shscript])). % TODO: only for installation
'$builder_hook'(ciaocl:bin_copy_and_link(shscript, 'ciao', Opts)) :- % TODO: only for installation
	( install_prolog_name(yes) ->
	    Opts = [link_as('prolog')]
	; Opts = []
	).
'$builder_hook'(ciaocl:build_bin) :- % (overrides build)
	BootEng = ~boot_eng_def,
	wr_template(as_cmd(core, shscript), ~bundle_path(core, 'cmds'), 'ciao', [
	    'ExtraCommands' = ~ciao_extra_commands, % (for toplevel)
	    %
	    'ciao_builder_cmdV' = ~cmdname_ver(yes, builder, 'ciao_builder', plexe),
	    'ciao_builder_cmd'  = ~cmdname_ver(no,  builder, 'ciao_builder', plexe),
	    'ciaosh_cmdV' = ~cmdname_ver(yes, core, 'ciaosh', plexe),
	    'ciaosh_cmd'  = ~cmdname_ver(no,  core, 'ciaosh', plexe),
	    'ciao_shell_cmdV' = ~cmdname_ver(yes, core, 'ciao-shell', plexe),
	    'ciao_shell_cmd'  = ~cmdname_ver(no,  core, 'ciao-shell', plexe),
	    'ciaoc_cmdV' = ~cmdname_ver(yes, core, 'ciaoc', plexe),
	    'ciaoc_cmd'  = ~cmdname_ver(no,  core, 'ciaoc', plexe),
	    % Access to boot builder
	    % TODO: only used in local-install, global installation uses the builder exec (is it OK?)
	    % TODO: (MinGW) is cmd.exe enough? (at least for bootstrap) consider PowerShell scripts for Windows?
	    'boot_ciaolib' = ~bundle_path(core, '.'),
	    'boot_bindir' = ~bundle_path(core, bootbuilddir, 'bin'),
	    'boot_ciaohdir' = ~bld_eng_path(hdir, BootEng),
	    'boot_ciaoengine' = ~bld_eng_path(exec, BootEng)
        ]),
 	( install_prolog_name(yes) ->
 	    builddir_bin_link_as(core, shscript, 'ciao', 'prolog')
 	; true
 	).

:- use_module(library(format), [sformat/3]).
:- use_module(ciaobld(bundle_configure), [
    set_prolog_flags_from_bundle_flags/1
]).

% TODO: store these flags during prepare_build_bin in a separate file, load them without need to rebuild the commands?
ciao_extra_commands(ExtraCommands) :-
	sformat(ExtraCommands, "-e '~w'",
	    [~list_to_lits(~set_prolog_flags_from_bundle_flags)]).

% TODO: import from formulae
list_to_lits([],     true).
list_to_lits([X|Xs], Lits) :-
	list_to_lits2(Xs, X, Lits).

list_to_lits2([],     X,  X).
list_to_lits2([X|Xs], X0, (X0, Lits)) :-
	list_to_lits2(Xs, X, Lits).

% ---------------------------------------------------------------------------
% Command for detection of system configuration

% TODO: install a custom version for Win32? 'ciao_sysconf' is a sh script
% TODO: 'ciao_sysconf' needs to be installed for multiplatform installations
%   (sharing a .bashrc or .cshrc across multiple OS)

:- use_module(ciaobld(builder_aux), [builddir_bin_copy_as/4]).
:- use_module(library(bundle/bundle_paths), [bundle_path/3]).

ciao_sysconf_sh := ~bundle_path(builder, 'sh_src/config-sysdep/ciao_sysconf').

% TODO: strange target, only for installation, define in another way?
'$builder_hook'(item_nested(ciao_sysconf)).
'$builder_hook'(ciao_sysconf:cmd('ciao_sysconf', [main='NONE_AUTOGEN', shscript])).
'$builder_hook'(ciao_sysconf:build_bin) :- % (override build)
	% (we just copy the script from the builder)
	builddir_bin_copy_as(core, shscript, ~ciao_sysconf_sh, 'ciao_sysconf').

% ---------------------------------------------------------------------------
% Engine

'$builder_hook'(item_nested(engine)).
:- include(.('engine.hooks')).

% ===========================================================================

:- doc(section, "Tests and Benchmarks").
% TODO: Add bundle defs for unit tests, integration tests, regression
%   tests, etc.

:- use_module(ciaobld(ciaoc_aux), [runtests_dir/2]).
:- use_module(library(bundle/bundle_paths), [bundle_path/3]).

'$builder_hook'(test) :- !,
	runtests_dir(core, 'lib'),
	runtests_dir(core, 'library'),
	runtests_ciaotests_hook. % integration tests

:- use_module(library(system), [working_directory/2]).
:- use_module(ciaobld(ciaoc_aux), [
	exists_and_compilable/1,
        invoke_ciaosh_batch/1]).

% Run Ciao integration tests
runtests_ciaotests_hook :-
	( exists_and_compilable(~bundle_path(core, 'tests')) ->
	    do_ciaotests % TODO: missing cleaning code! (see below)
	; true
	).

do_ciaotests :-
	working_directory(ThisDir, ~bundle_path(core, 'tests')),
	invoke_ciaosh_batch([
	  use_module(core_tests(run_tests), [run_tests/0]),
	  run_tests:run_tests
	]),
	working_directory(_, ThisDir).

% TODO: Missing cleaning code! (incomplete)
% 	find . -name '*.po' -exec rm {} \;
% 	find . -name '*.itf' -exec rm {} \;
% 	find . -name '*.a' -exec rm {} \;
% 	find . -name '*.o' -exec rm {} \;
% 	find . -name '*.so' -exec rm {} \;
% 	find . -name '*.dll' -exec rm {} \;
% 	find . -name '*_glue.c' -exec rm {} \;
% 	find . -name 'tmpciao*' -exec rm {} \;
% 	find . -name '*.log' -exec rm {} \;
% 	-rm -f load_dynlibs/load_dynlibs
% 	-rm -f object_test/object_test
% 	-rm -f test_java/plserver
% 	-rm -f test_java/j2pl_test.class
% 	-rm -f actmods_test/simple_server
% 	-rm -f actmods_test/simple_client_with_main 
% 	-rm -f actmods_test/*.addr
% 	-rm -f persistentdb/persistentdb
% 	-rm -f persistentdb/queue 
% 	-rm -f persistentdb/example_static
% 	-rm -f persistentdb/example_dynamic 
% 	-rm -f remote_exec/client
% 	-rm -f remote_exec/server
% 	-rm -f run_tests
% 	-rm -rf pers
% 	-rm -rf pers_queue

% ===========================================================================

:- doc(section, "Windows-specific").

% TODO: This should be a postbuild or postinstall operation

:- use_module(ciaobld(config_common), [default_eng_def/1]).
:- use_module(ciaobld(ciaoc_aux), [create_windows_bat/6]).

:- use_module(library(emacs/emacs_batch),
	[set_emacs_type/1, unset_emacs_type/0,
	 set_emacs_path/1, unset_emacs_path/0]).
:- use_module(ciaobld(bundle_configure), [config_set_flag/2]). % TODO: dangerous!
:- use_module(engine(internals), ['$bundle_id'/1]).

:- use_module(ciaobld(messages_aux), [normal_message/2]).
:- use_module(ciaobld(builder_cmds), [builder_cmd/2]). % TODO: dangerous!

% TODO: do configure and install of just the emacs mode (this is too complex)
% 'environment_and_windows_bats' required by runwin32.bat, Ciao.iss.skel

% NOTE: Called from Ciao.iss.skel
% Accepts this parameter:
%   --emacs_path=EmacsDir (as a windows path)
'$builder_hook'(custom_run(environment_and_windows_bats, Args)) :-
	( '$bundle_id'(ciao_emacs) -> % has ciao_emacs?
	    ciao_emacs_reinstall_win32(Args)
	; true
	),
	% Fix executables
	builder_cmd(build_bin, 'core.exec_header'),
	windows_bats.

ciao_emacs_reinstall_win32(Args) :-
	( member(Arg, Args),
	    atom_concat('--emacs_path=', EmacsPath, Arg) ->
	    set_emacs_path(EmacsPath)
	; true
	),
	set_emacs_type('Win32'), % TODO: patch config instead?
	config_set_flag(ciao_emacs:enabled, 'yes'), % TODO: dangerous!
	builder_cmd(build_bin, 'ciao_emacs.emacs_mode'), % TODO: needed?
	builder_cmd(install, 'ciao_emacs.emacs_mode'), % TODO: needed?
	% (put ciao-mode-init.el in place)
	builder_cmd(install, 'ciao_emacs.dot_emacs'),
	unset_emacs_type,
	unset_emacs_path.

% TODO: make sure that 'ciao' is the same in Win32 and unix (currently it isn't)
% TODO: Add a build_cmds_fix_win action that just creates this
windows_bats :-
	normal_message("creating .bat files for commands", []),
	Eng = ~default_eng_def,
	( % (failure-driven loop)
	  win_cmd_and_opts(BatCmd, Opts, EngExecOpts, OrigCmd),
	    create_windows_bat(Eng, BatCmd, Opts, EngExecOpts, core, OrigCmd),
	    fail
	; true
	).

% TODO: Pass '-i' to the toplevel, not the engine, from the emacs mode
%   (not all ciaosh calls need interactive mode).
%
%   Alternatively, we could use fakecygpty.c (different versions
%   forked at https://github.com/jfmc/fakecygpty,
%   https://github.com/jfmc/fakecygpty-1,
%   https://github.com/jfmc/fakecygpty-2) to fix tty treatment in
%   emacs.

win_cmd_and_opts(ciaosh, '', '-i', ciaosh).
win_cmd_and_opts(ciaoc, '', '', ciaoc).
% TODO: move together with ciao (sh) generation (see ciaocl:build_bin)
win_cmd_and_opts(ciao, Atm, '-i', ciaosh) :- 
	atom_codes(Atm, ~ciao_extra_commands).

