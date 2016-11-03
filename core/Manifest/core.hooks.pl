:- module(_, [], [ciaobld(bundlehooks), dcg]).

:- doc(title,  "Bundle Hooks for Ciao core").

'$builder_hook'(manual_dir(as('doc/internals', 'ciao_internals'))).

% ===========================================================================

:- use_module(library(bundle/bundle_flags), [get_bundle_flag/2]).
:- use_module(library(bundle/bundle_paths), [bundle_path/3, bundle_path/4]).
:- use_module(ciaobld(builder_aux), [
        builddir_bin_copy_as/4,
        builddir_bin_link_as/4
	]).

% ============================================================================

'$builder_hook'(item_subs(['core/dot_shell', 'core/dot_emacs', 'core/emacs_mode', 'core/java', 'core/pillow', 'core/persdb_mysql'])).

:- include(.('dot_shell.hooks')).
:- include(.('dot_emacs.hooks')).
:- include(.('emacs_mode.hooks')).
:- include(.('java.hooks')).
:- include(.('persdb_mysql.hooks')).
:- include(.('pillow.hooks')).

% ============================================================================
% (engine, libraries, and compiler)

'$builder_hook'(bundle_def([
  ciaobase, % Must be the first one
  %
  ciaocl,
  ciao_sysconf,
  core_cmds,
  %
  lib('lib'),
  % WARNING: Ciao cannot compile 'clpq' and 'clpr' together.
  %   We build each of 'clpq' and 'clpr' as separate processes. They
  %   cannot be compiled together since the translation module uses the
  %   constraint solver itself and the attributed variable hooks are
  %   mixed.
  %
  % TODO: obtain a more recent version of clpq,clpr (this is not
  %   the latest), adapt to Ciao and port to multiattributes (or
  %   qualify each term).
  lib_force_build('library/clpq'), % See WARNING note above
  lib_force_build('library/clpr'), % See WARNING note above
  lib('library'),
  src('examples')
])).

% This is the minimum part needed for @apl{ciao_builder} to compile
% the rest of the system.
%
% NOTE: this must be build before rest of 'core' compilation
%
'$builder_hook'(ciaobase:item_def([
    % Note: should be build in this precise order
    % TODO: Add dependencies instead
    item_group("engine", engine), % (needs bootstrap ciaoc)
    item_group("exec_header", exec_header),
    ciaoc, % (depends on engine and exec_header)
    shell
])).

% Enumeration of the standalone utilities in */cmds/
'$builder_hook'(core_cmds:item_def(
    cmds_list('cmds', [
        'ciaodump'-[plexe],
        'pldiff'-[plexe],
        'lpmake'-[plexe],
        'plindent'-[plexe],
        'checkline'-[plexe],
        'ciaoc_sdyn'-[plexe]
    ]))).

'$builder_hook'(ciaoc:item_def(
        cmds_list('ciaoc', [
          'ciaoc'-[
            name="standalone compiler",
            plexe,
	    bootstrap_ciaoc, static
          ]
        ]))).
	
'$builder_hook'(shell:item_def(
	cmds_list('shell', [
          'ciaosh'-[plexe, name="interactive toplevel", final_ciaoc],
          'ciao-shell'-[plexe, name="ciao-script runtime", final_ciaoc, static]
        ]))).

% ===========================================================================

:- doc(section, "Windows-specific").

% TODO: This should be a postbuild or postinstall operation

:- use_module(ciaobld(config_common), [default_eng_def/1]).
:- use_module(ciaobld(ciaoc_aux), [create_windows_bat/6]).

:- use_module(library(emacs/emacs_batch),
	[set_emacs_type/1, unset_emacs_type/0,
	 set_emacs_path/1, unset_emacs_path/0]).
:- use_module(ciaobld(bundle_configure), [config_set_flag/2]). % TODO: dangerous!

% TODO: do configure and install of just the emacs mode (this is too complex)
% 'environment_and_windows_bats' required by runwin32.bat, Ciao.iss.skel

% NOTE: Called from Ciao.iss.skel
% Accepts this parameter:
%   --emacs_path=EmacsDir (as a windows path)
'$builder_hook'(custom_run(environment_and_windows_bats, Args)) :-
	( member(Arg, Args),
	  atom_concat('--emacs_path=', EmacsPath, Arg) ->
	    set_emacs_path(EmacsPath)
	; true
	),
	set_emacs_type('Win32'), % TODO: patch config instead?
	config_set_flag(core:with_emacs_mode, 'yes'), % TODO: dangerous!
	environment,
	windows_bats,
	unset_emacs_type,
	unset_emacs_path.

environment :-
	builder_cmd(build_nodocs, 'core/exec_header', []),
	builder_cmd(build_nodocs, 'core/emacs_mode', []), % (needed?)
	builder_cmd(install, 'core/emacs_mode', []), % (needed?)
	% (put ciao-mode-init.el in place)
	builder_cmd(install, 'core/dot_emacs', []).

% TODO: make sure that 'ciao' is the same in Win32 and unix (currently it isn't)
% TODO: Add a build_cmds_fix_win action that just creates this
windows_bats :-
	normal_message("Creating .bat files for commands", []),
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
% TODO: move together with ciao (sh) generation (see ciaocl:build_nodocs)
win_cmd_and_opts(ciao, Atm, '-i', ciaosh) :- 
	atom_codes(Atm, ~ciao_extra_commands).

% ---------------------------------------------------------------------------

:- doc(subsection, "Header for executables").
% TODO: merge with do_exe_header code?

'$builder_hook'(exec_header:item_def([
    eng_exec_header(eng('engine/ciaoengine', []))
])).

% ---------------------------------------------------------------------------

install_prolog_name := ~get_bundle_flag(core:install_prolog_name).

:- use_module(ciaobld(eng_defs), [bootbld_eng_path/3]).
:- use_module(ciaobld(config_common), [cmdname_ver/5]).

% The 'ciao' super-command
%
% (definition for installation)
% (merge below?)
%'$builder_hook'(ciaocl:item_def(
%    cmds_list('cmds', [ % TODO: only for installation
%        'ciao'-[shscript]
%    ]))).
'$builder_hook'(ciaocl:item_def(R)) :- % TODO: only for installation
	( install_prolog_name(yes) ->
	    Opts = [link_as('prolog')]
	; Opts = []
	),
	R = item_group("'ciao' (command)", 
	      bin_copy_and_link(shscript, 'ciao', Opts)).
% (definition for build_nodocs)
'$builder_hook'(ciaocl:build_nodocs) :-
	cmd_message(core, "building '~w' (command)", ['ciao']),
	Eng = ~default_eng_def,
	wr_template(as_cmd(core, shscript), ~bundle_path(core, 'cmds'), 'ciao', [
	    'ExtraCommands' = ~ciao_extra_commands, % (for toplevel)
	    %
	    'ciao_builder_cmdV' = ~cmdname_ver(yes, builder, 'ciao_builder', plexe), 'ciao_builder_cmd' = ~cmdname_ver(no, builder, 'ciao_builder', plexe),
	    'ciaosh_cmdV' =     ~cmdname_ver(yes, core, 'ciaosh', plexe),         'ciaosh_cmd' = ~cmdname_ver(no, core, 'ciaosh', plexe),
	    'ciao_shell_cmdV' = ~cmdname_ver(yes, core, 'ciao-shell', plexe), 'ciao_shell_cmd' = ~cmdname_ver(no, core, 'ciao-shell', plexe),
	    'ciaoc_cmdV' =      ~cmdname_ver(yes, core, 'ciaoc', plexe),           'ciaoc_cmd' = ~cmdname_ver(no, core, 'ciaoc', plexe),
	    'ciaopp_cmdV' =     ~cmdname_ver(yes, ciaopp, 'ciaopp', plexe),       'ciaopp_cmd' = ~cmdname_ver(no, ciaopp, 'ciaopp', plexe),
	    'lpdoc_cmdV' =      ~cmdname_ver(yes, lpdoc, 'lpdoc', plexe),          'lpdoc_cmd' = ~cmdname_ver(no, lpdoc, 'lpdoc', plexe),
	    % Access to boot builder
	    % TODO: only used in local-install, global installation uses the builder exec (is it OK?)
	    % TODO: (MinGW) is cmd.exe enough? (at least for bootstrap) consider PowerShell scripts for Windows?
	    'boot_ciaolib' = ~bundle_path(core, '.'),
	    'boot_bindir' = ~bundle_path(core, bootbuilddir, 'bin'),
	    'boot_ciaohdir' = ~bootbld_eng_path(hdir, Eng),
	    'boot_ciaoengine' = ~bootbld_eng_path(exec, Eng)
        ]),
 	( install_prolog_name(yes) ->
 	    builddir_bin_link_as(core, shscript, 'ciao', 'prolog')
 	; true
 	).

:- use_module(library(format), [sformat/3]).
:- use_module(ciaobld(bundle_configure), [
    set_prolog_flags_from_bundle_flags/1
]).

% TODO: store these flags during prebuild in a separate file, load them without need to rebuild the commands?
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
% TODO: install a custom version for Win32? 'ciao_sysconf' is a sh script
% TODO: 'ciao_sysconf' needs to be installed for multiplatform installations
%   (sharing a .bashrc or .cshrc across multiple OS)

ciao_sysconf_sh := ~bundle_path(builder, 'sh_src/config-sysdep/ciao_sysconf').

'$builder_hook'(ciao_sysconf:item_def(
    cmds_list('cmds', [ % TODO: strange; only for installation
        'ciao_sysconf'-[shscript]
    ]))).
'$builder_hook'(ciao_sysconf:build_nodocs) :-
	cmd_message(core, "building '~w' (command)", ['ciao_sysconf']),
	% TODO: we build nothing here (just copy) but the user does not want to know
	builddir_bin_copy_as(core, shscript, ~ciao_sysconf_sh, 'ciao_sysconf').

% ===========================================================================
% Engine installation/uninstallation

'$builder_hook'(engine:item_def([
    eng('engine/ciaoengine', [])
])).

% NOTE: experimental (see options)
'$builder_hook'(static_engine:item_def([
    eng('engine/ciaoengine', [
      % TODO: Uses bootstrap ciaoc (see eng_maker.pl) -- allow configuration here
      add_stat_libs, % link statically against C system libraries
      static_mods([library(random),
                   library(sockets),
                   library(sha1),
		   library(concurrency)]) % link statically against foreign code
    ])
])).

% ===========================================================================

:- doc(section, "Tests and Benchmarks").
% TODO: Add bundle defs for unit tests, integration tests, regression
%   tests, etc.

:- use_module(ciaobld(ciaoc_aux), [runtests_dir/2]).

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
	cmd_message(core, "running 'tests' tests", []),
	working_directory(ThisDir, tests),
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

:- use_module(ciaobld(ciaoc_aux), [
	% TODO: use bundle defs instead
        invoke_ciaosh_batch/1]).

% Run benchmarks
'$builder_hook'(runbenchmarks) :- !,
	invoke_ciaosh_batch([
	  use_module(library(benchmarks/ecrc), [main/1]),
	  ecrc:main([])
        ]).

% TODO: also include (or merge) these:
%   core/examples/misc/ (see Makefile) -- too small, need at least scaling

