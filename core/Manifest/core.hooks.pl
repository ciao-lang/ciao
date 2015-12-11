:- module(_, [], [ciaobld(bundlehooks), dcg]).

:- doc(title,  "Bundle Hooks for Ciao core").
:- doc(author, "Ciao Development Team").

'$builder_hook'(desc_name('Core')).

'$builder_hook'(manual_dir(as('doc/internals', 'ciao_internals'))).

% ===========================================================================

:- use_module(library(bundle/bundle_flags), [get_bundle_flag/2]).

% ============================================================================

:- use_module(ciaobld(bundle_configure), [
    foreign_config_var/3,
    foreign_config_version/2
]).

% ===========================================================================

:- use_module(library(bundle/paths_extra), [fsR/2]).

:- use_module(ciaobld(builder_aux), [
        builddir_bin_copy_as/3,
        builddir_bin_link_as/3
	]).

% ============================================================================

% TODO: Add missing subs
'$builder_hook'(item_subs(['core/dot_shell', 'core/emacs_mode', 'core/java'])).

:- include(.('dot_shell.hooks')).
:- include(.('emacs_mode.hooks')).
:- include(.('java.hooks')).
:- include(.('persdb_mysql.hooks')).
:- include(.('pillow.hooks')).

% ============================================================================

:- doc(section, "Build").
% (engine, libraries, and compiler)

:- use_module(ciaobld(ciaoc_aux), [build_libs/2]).

'$builder_hook'(build_libraries) :-
	build_libs(core, 'lib'),
	%
	% WARNING: Ciao cannot compile 'clpq' and 'clpr' together in the same
        %   program!
	%
	% So we build each of 'clpq' 'clpr' as separate processes.
	% (it cannot be compiled together since the translation module
	% uses the constraint solver itself)
	%
	% TODO: obtain a more recent version of clpq,clpr (this is not
	%   the latest), adapt to Ciao and port to multiattributes (or
	%   qualify each term).
	%
	build_libs(core, 'library/clpq'),
	build_libs(core, 'library/clpr'),
	%
	build_libs(core, 'library').

'$builder_hook'(build_bin) :-
	bundleitem_do(core_cmds, core, build_nodocs),
	bundleitem_do(ciao_sysconf, core, build_nodocs),
	bundleitem_do(ciaocl, core, build_nodocs).

% Prepare source for build
% (e.g., for automatically generated code, foreign interfaces, etc.)
'$builder_hook'(prebuild_nodocs) :-
	bundleitem_do([pillow, persdb_mysql], core, prebuild_nodocs).

% ============================================================================

% (used from 'ciaobase' target)
% TODO: Use "ciaoc:build_nodocs" or "build_nodocs(ciaoc)" as head?
'$builder_hook'(ciaoc:build_nodocs) :- bundleitem_do(ciaoc, core, build_nodocs).
'$builder_hook'(ciaoc:build_docs) :- !.

'$builder_hook'(ciaoc:item_def(
        cmds_list(core, bundle_src(core)/ciaoc, [
          'ciaoc'-[
            name="standalone compiler",
            plexe,
	    bootstrap_ciaoc, static
          ]
        ]))).
	
% (used from 'ciaobase' target)
'$builder_hook'(shell:build_nodocs) :- bundleitem_do(shell, core, build_nodocs).
'$builder_hook'(shell:build_docs) :- !.

'$builder_hook'(shell:item_def(
	cmds_list(core, bundle_src(core)/shell, [
          'ciaosh'-[plexe, name="interactive toplevel", final_ciaoc],
          'ciao-shell'-[plexe, name="ciao-script runtime", final_ciaoc, static]
        ]))).

% ===========================================================================

:- doc(section, "Windows-specific").

% TODO: This should be a postbuild or postinstall operation

:- use_module(library(bundle/bundle_params), [set_bundle_param_value/2]).
:- use_module(ciaobld(config_common), [default_eng/1]).
:- use_module(ciaobld(ciaoc_aux), [create_windows_bat/5]).

% TODO: do configure and install of just the emacs mode (this is too complex)
% 'environment_and_windows_bats' required by runwin32.bat, Ciao.iss.skel
% NOTE: Called from Ciao.iss.skel
% Uses this parameter:
%   --emacs_path=EmacsDir (as a windows path)

'$builder_hook'(custom_run(environment_and_windows_bats)) :-
	% TODO: move these param set operations to library(emacs/emacs_batch)
	set_bundle_param_value(core:emacs_type, 'Win32'), % TODO: strange
	set_bundle_param_value(core:with_emacs_mode, 'yes'), % TODO: strange
	environment,
	windows_bats.

environment :-
	bundleitem_do(emacs_mode, core, build_nodocs),
	bundleitem_do(emacs_mode, core, install), % (put ciao-mode-init.el in place)
	bundleitem_do(exec_header, core, build_nodocs). % TODO: why here?

% TODO: make sure that 'ciao' is the same in Win32 and unix (currently it isn't)
% TODO: Add a build_cmds_fix_win action that just creates this
windows_bats :-
	normal_message("Creating .bat files for commands", []),
	EngMainMod = ~default_eng,
	( % (failure-driven loop)
	  win_cmd_and_opts(BatCmd, Opts, EngOpts, OrigCmd),
	    create_windows_bat(EngMainMod, BatCmd, Opts, EngOpts, OrigCmd),
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
% TODO: move together with ciao (sh) generation (see ciaocl:item_build_nodocs)
win_cmd_and_opts(ciao, Atm, '-i', ciaosh) :- 
	atom_codes(Atm, ~ciao_extra_commands).

% ---------------------------------------------------------------------------

:- doc(subsection, "Header for executables").
% TODO: merge with do_exe_header code?

% (used from 'ciaobase' target)
'$builder_hook'(exec_header:build_nodocs) :- bundleitem_do(exec_header, core, build_nodocs).
'$builder_hook'(exec_header:build_docs) :- !.
'$builder_hook'(exec_header:clean_norec) :- !, bundleitem_do(exec_header, core, clean_norec).
%'$builder_hook'(exec_header:install) :- !, bundleitem_do(exec_header, core, install).
%'$builder_hook'(exec_header:uninstall) :- !, bundleitem_do(exec_header, core, uninstall).

'$builder_hook'(exec_header:item_def([
    eng_exec_header(ciaoengine)
])).

% ============================================================================

:- doc(section, "Register in the System").
% TODO: Should this be just a subtask in the installation? Users
%       should not invoke it...
% TODO: This should be done for each anchor (bash, csh, emacs, etc.)
%       and each bundle

% Modifies the .bashrc/.cshrc/.emacs files to let Ciao run from the
% installed lib files.
'$builder_hook'(register) :-
        bundleitem_do(bashrc, core, register),
	bundleitem_do(cshrc, core, register),
	bundleitem_do(emacs_mode, core, register).

% Leaves the .bashrc/.cshrc/.emacs file in its original state.
'$builder_hook'(unregister) :-
        bundleitem_do(bashrc, core, unregister),
	bundleitem_do(cshrc, core, unregister),
	bundleitem_do(emacs_mode, core, unregister).

% ============================================================================

:- doc(section, "Installation").

'$builder_hook'(install) :- bundleitem_do(only_global_ins(~core_desc), core, install).

'$builder_hook'(uninstall) :- bundleitem_do(only_global_ins(~core_desc), core, uninstall).

core_desc := [
  engine, ciaoc, shell, % TODO: ciaobase?
  %
  dot_shell_, % TODO: not really commands?
  ciaocl_,
  core_cmds,
  %
  lib(core, 'engine'),
  lib(core, 'lib'),
  lib(core, 'library'),
  pillow_,
  src(core, 'examples'),
  %
  emacs_mode
].

% Enumeration of the standalone utilities in */cmds/
cmds_dir := bundle_src(core)/cmds.

'$builder_hook'(core_cmds:item_def(
          cmds_list(core, ~cmds_dir, ~core_cmds))).

:- use_module(library(aggregates), [findall/3]).
core_cmds := ~findall(B-[K], core_cmd(B, K)).

core_cmd('fileinfo', plexe).
core_cmd('pldiff', plexe).
core_cmd('viewpo', plexe).
core_cmd('lpmake', plexe).
core_cmd('plindent', plexe).
core_cmd('show_asr', plexe).
core_cmd('compiler_output', plexe).
core_cmd('checkline', plexe).

% TODO: strange... enumerated for installation, add a table of exec+kind instead
core_cmd('ciao_sysconf', shscript).
core_cmd('ciao', shscript). % TODO: twice?!

% ---------------------------------------------------------------------------

install_prolog_name := ~get_bundle_flag(core:install_prolog_name).

:- use_module(ciaobld(config_common), [bld_eng_path/4, cmdname_ver/5]).

% Generate 'ciao' super-command
'$builder_hook'(ciaocl:item_build_nodocs) :-
	cmd_message(core, "building '~w' (command)", ['ciao']),
	EngMainMod = ~default_eng,
	wr_template(k(shscript), ~cmds_dir, 'ciao', [
	    'ExtraCommands' = ~ciao_extra_commands, % (for toplevel)
	    %
	    'ciao_builder_cmdV' = ~cmdname_ver(yes, builder, 'ciao_builder', plexe), 'ciao_builder_cmd' = ~cmdname_ver(no, builder, 'ciao_builder', plexe),
	    'ciaosh_cmdV' =     ~cmdname_ver(yes, core, 'ciaosh', plexe),         'ciaosh_cmd' = ~cmdname_ver(no, core, 'ciaosh', plexe),
	    'ciao_shell_cmdV' = ~cmdname_ver(yes, core, 'ciao-shell', plexe), 'ciao_shell_cmd' = ~cmdname_ver(no, core, 'ciao-shell', plexe),
	    'ciaoc_cmdV' =      ~cmdname_ver(yes, core, 'ciaoc', plexe),           'ciaoc_cmd' = ~cmdname_ver(no, core, 'ciaoc', plexe),
	    'ciaopp_cmdV' =     ~cmdname_ver(yes, ciaopp, 'ciaopp', plexe),       'ciaopp_cmd' = ~cmdname_ver(no, ciaopp, 'ciaopp', plexe),
	    'lpdoc_cmdV' =      ~cmdname_ver(yes, lpdoc, 'lpdoc', plexe),          'lpdoc_cmd' = ~cmdname_ver(no, lpdoc, 'lpdoc', plexe),
	    % TODO: only works in local-install! add global installation code
	    % TODO: (MinGW) is cmd.exe enough? (at least for bootstrap) consider PowerShell scripts for Windows?
	    'boot_ciaolib' = ~fsR(bundle_src(core)),
	    'boot_bindir' = ~fsR(builddir_bin(bootbuild)),
	    'boot_ciaohdir' = ~bld_eng_path(hdir, bootbuild, EngMainMod),
	    'boot_ciaoengine' = ~bld_eng_path(exec, bootbuild, EngMainMod)
        ]),
 	( install_prolog_name(yes) ->
 	    builddir_bin_link_as(shscript, 'ciao', 'prolog')
 	; true
 	).

'$builder_hook'(ciaocl_:item_def(R)) :-
	( install_prolog_name(yes) ->
	    Opts = [link_as('prolog')]
	; Opts = []
	),
	R = item_group("'ciao' (command)", 
	      bin_copy_and_link(shscript, core, 'ciao', Opts)).

:- use_module(library(format), [sformat/3]).
:- use_module(ciaobld(bundle_configure), [
    set_prolog_flags_from_bundle_flags/1
]).

% TODO: set prolog flags from bundle flags dynamically (otherwise we need to rebuild everytime flags are changed)
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

ciao_sysconf_sh := ~fsR(bundle_src(builder)/sh_src/'config-sysdep'/'ciao_sysconf').

'$builder_hook'(ciao_sysconf:item_build_nodocs) :-
	cmd_message(core, "building '~w' (command)", ['ciao_sysconf']),
	% TODO: we build nothing here (just copy) but the user does not want to know
	builddir_bin_copy_as(shscript, ~ciao_sysconf_sh, 'ciao_sysconf').

% ===========================================================================
% Engine installation/uninstallation

% (used from 'ciaobase' target)
'$builder_hook'(engine:build_nodocs) :- bundleitem_do(engine, core, build_nodocs).
'$builder_hook'(engine:build_docs) :- !.
'$builder_hook'(engine:clean_norec) :- !, bundleitem_do(engine, core, clean_norec).
'$builder_hook'(engine:install) :- !, bundleitem_do(engine, core, install).
'$builder_hook'(engine:uninstall) :- !, bundleitem_do(engine, core, uninstall).

'$builder_hook'(engine:item_def([
    eng(ciaoengine, []) % core/engine/ciaoengine.pl
])).

% NOTE: experimental (see options)
% (used from 'ciaobase' target)
'$builder_hook'(static_engine:build_nodocs) :- bundleitem_do(static_engine, core, build_nodocs).
'$builder_hook'(static_engine:build_docs) :- !.
'$builder_hook'(static_engine:clean_norec) :- !, bundleitem_do(static_engine, core, clean_norec).
'$builder_hook'(static_engine:install) :- !, bundleitem_do(static_engine, core, install).
'$builder_hook'(static_engine:uninstall) :- !, bundleitem_do(static_engine, core, uninstall).

'$builder_hook'(static_engine:item_def([
    eng(ciaoengine, [ % core/engine/ciaoengine.pl
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

% Run tests
'$builder_hook'(runtests) :- !,
	runtests_dir(core, 'lib'),
	runtests_dir(core, 'library'),
	runtests_ciaotests_hook. % integration tests

:- use_module(library(system), [working_directory/2]).
:- use_module(ciaobld(ciaoc_aux), [
	exists_and_compilable/1,
        invoke_ciaosh_batch/1]).

% Run Ciao integration tests
runtests_ciaotests_hook :-
	( exists_and_compilable(~fsR(bundle_src(core)/'tests')) ->
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

