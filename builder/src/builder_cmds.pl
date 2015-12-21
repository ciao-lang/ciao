:- module(_, [], [dcg, fsyntax, hiord, assertions, regtypes, isomodes]).

:- doc(title,  "Builder Commands").
:- doc(author, "Jose F. Morales").

:- use_module(library(format), [format/3]). % TODO: only use throw/1
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(system), [cd/1, working_directory/2]).
:- use_module(library(pathnames), [path_split_list/2]).
:- use_module(library(glob), [glob/3]).
:- use_module(library(terms), [atom_concat/2]).

:- use_module(library(bundle/bundle_params),
	[set_bundle_param_value/2,
	 bundle_param_value/2]).
:- use_module(library(bundle/bundle_flags), [reset_all_bundle_flags/0]).
%
:- use_module(library(bundle/paths_extra), [fsR/2, bundle_metasrc/3]).
:- use_module(library(bundle/bundle_info), [
	root_bundle/1,
	enum_sub_bundles/2,
	enumrev_sub_bundles/2
	]).
:- use_module(engine(internals), [reload_bundleregs/0, '$bundle_id'/1]).

:- use_module(ciaobld(builder_aux), [
	root_bundle_source_dir/1,
	rootprefixed/2,
	ensure_builddir/0,
        ensure_builddir_doc/0,
        storedir_install/1,
        storedir_uninstall/1,
	eng_active_bld/1
	]).
:- use_module(ciaobld(bundle_scan), [bundle_scan/1]).
:- use_module(ciaobld(ciaoc_aux),
	[promote_bootstrap/1,
	 %
	 eng_build/2,
	 eng_clean/1,
	 %
	 build_eng_exec_header/1,
	 clean_eng_exec_header/1,
	 %
	 build_cmds_list/3,
	 builddir_clean/1,
	 builddir_clean_bundlereg/0,
	 clean_tree/1,
	 clean_mod/1
	]).
:- use_module(library(system_extra), [del_file_nofail/1]).
:- use_module(library(source_tree), [remove_dir/1]).
:- use_module(ciaobld(config_common), [
    instype/1,
    instciao_bindir/1,
    instciao_storedir/1,
    instciao_bundledir/2,
    with_docs/1
]).
:- use_module(ciaobld(messages_aux), [cmd_message/3, normal_message/2]).

% ===========================================================================
:- doc(section, "Builder commands").

:- doc(bug, "See cleaning targets for 'lpdoc' and fix it. See other
   clean targets in core/Manifest/core.hooks.pl").

:- doc(bug, "Improve output for foreign clean").

% ---------------------------------------------------------------------------

% Set builder parameters from parsed list

set_params([flag(Flag, Value)|Flags]) :- !,
	( bundle_param_value(Flag, _) ->
	    true % TODO: check
	; set_bundle_param_value(Flag, Value)
	),
	set_params(Flags).
set_params([]).

% ---------------------------------------------------------------------------

builder_cmd_on_set(_Cmd, [], _Opts) :- !.
builder_cmd_on_set(Cmd, [Target|Targets], Opts) :-
	builder_cmd(Cmd, Target, Opts),
	builder_cmd_on_set(Cmd, Targets, Opts).

% (commands that are already completed)
:- data builder_cmd_done/3.

:- export(cleanup_builder/0).
% Cleanup builder state
cleanup_builder :-
	retractall_fact(builder_cmd_done(_, _, _)).

:- export(builder_cmd/3).
builder_cmd(Cmd, Target, Opts) :-
	current_fact(builder_cmd_done(Cmd, Target, Opts)),
	!,
	% TODO: errors, etc.?
	true.
builder_cmd(Cmd, Target, Opts) :-
	builder_cmd_(Cmd, Target, Opts),
	assertz_fact(builder_cmd_done(Cmd, Target, Opts)).

builder_cmd_(local_install, Target, Opts) :- !,
	Opts2 = [flag(ciao:instype, 'local')|Opts],
	builder_cmd(fullinstall, Target, Opts2).
builder_cmd_(global_install, Target, Opts) :- !,
	Opts2 = [flag(ciao:instype, 'global')|Opts],
	builder_cmd(fullinstall, Target, Opts2).
builder_cmd_(local_install_paranoid, Target, Opts) :- !,
	Opts2 = [flag(ciao:instype, 'local'),
		 flag(ciao:unused_pred_warnings, 'yes')|Opts],
	builder_cmd(fullinstall, Target, Opts2).
builder_cmd_(fullinstall, Target, Opts) :- !,
	builder_cmd(scan_and_config, Target, Opts),
	builder_cmd(build, Target, []),
	builder_cmd(install, Target, []).
%
builder_cmd_(boot_promote, Target, _Opts) :- !,
	do_boot_promote(Target).
%
builder_cmd_(configure, Target, Opts) :- !,
	( member(flag(ciao:list_flags, true), Opts) ->
	    set_params(Opts), % TODO: needed?
	    builder_cmd(config_list_flags, Target, Opts)
	; member(flag(ciao:describe_flag, _Opt), Opts) ->
	    set_params(Opts), % TODO: needed, pass arg instead
	    builder_cmd(config_describe_flag, Target, Opts)
	; member(flag(ciao:set_flag_flag, _Opt), Opts) ->
	    set_params(Opts), % TODO: needed, pass arg instead
	    builder_cmd(config_set_flag, Target, Opts)
	; builder_cmd(scan_and_config, Target, Opts),
	  post_config_message(Target)
	).
%
builder_cmd_(scan_and_config, Target, Opts) :- !,
	% Note: scanning bundles must be done before configuration
	% Note: fsR cannot be used until bundles are scanned
	( root_bundle(Target) ->
	    % TODO: avoid scanning for some options... allow scanning for other bundles
	    cmd_message(Target, "scanning (sub-)bundles", []),
	    root_bundle_source_dir(CiaoSrc),
	    bundle_scan(CiaoSrc),
	    reload_bundleregs % (reload, bundles has been scanned)
	; true
	),
	%
	builder_cmd(config_noscan, Target, Opts).
builder_cmd_(config_noscan, Target, Opts) :- root_bundle(Target), !,
	cmd_message(Target, "configuring", []),
	set_params(Opts),
	ensure_builddir,
	( bundle_param_value(ciao:interactive_config, true) ->
	    true % use saved config values
	; % TODO: do not reset, skip load instead
	  reset_all_bundle_flags
	),
	do_config_noscan,
	cmd_message(Target, "configured", []).
builder_cmd_(config_noscan, Target, _Opts) :- !,
	% TODO: implement configuration for individual bundles
	format(user_error, "ERROR: Cannot configure bundle `~w'.~n", [Target]),
	halt(1).
%
builder_cmd_(rescan_bundles, Target, _Opts) :- !,
        cmd_message(Target, "re-scanning (sub-)bundles", []),
	( root_bundle(Target) -> % Allow scanning without previous 'configure'
	    root_bundle_source_dir(BundleDir)
	; BundleDir = ~fsR(bundle_src(Target)) ->
	    true
	; format(user_error, "ERROR: bundle '~w' is unknown (did you run 'configure'?)~n", [Target]),
	  halt(1)
	),
	( root_bundle(Target) ->
	    % TODO: Add an option to preserve scanned bundles?
	    %   Cleaning is needed for rescaning bundles of pruned
	    %   sources.
	    builddir_clean_bundlereg, % clean previous bundlereg
	    bundle_scan(BundleDir)
	; % TODO: implement rescan_bundles for individual bundles
	  format(user_error, "ERROR: sub-bundle scan not supported yet~n", []),
	  halt(1)
	).
% List bundles
builder_cmd_(list, '$no_bundle', _Opts) :- !,
	list_bundles.
% Download and install bundles
builder_cmd_(get(BundleAlias), '$no_bundle', _Opts) :- !,
	bundle_get(BundleAlias).
%
builder_cmd_(build, Target, Opts) :- !,
	% TODO: Make sure that lpdoc is build before creating the documentation
	%       (in general, 'build' should delay tasks)
	builder_cmd(build_nodocs, Target, Opts),
	builder_cmd(build_docs, Target, Opts).
builder_cmd_(Cmd, ciaobase, Opts) :- ( Cmd = build_nodocs ; Cmd = build_docs ), !,
        % TODO: define 'ciaobase' as a bundle or bundle part (minimum
        % 'core/ciaoc', 'core/shell', support, needed for
        % @apl{ciao_builder} to compile the rest of the system)
	%
	% TODO: NOTE: this must be build before rest of 'core' compilation
	%   (starting with bootstrap ciaoc and reaching a fixpoint)
	%   
	builder_cmd_on_set(Cmd, [
          'core/engine', % NOTE: Not the bootstrap engine!
	  'core/exec_header',
	  'core/ciaoc',
	  'core/shell'
        ], Opts).
builder_cmd_(build_nodocs, Target, Opts) :- root_bundle(Target), !,
	check_ready_for_build(build_nodocs, Target),
	% NOTE: 'ciaobase' must be built before anything else
	builder_cmd(build_nodocs, ciaobase, Opts),
	% Treat sub-bundles
	( % (failure-driven loop)
	  enum_sub_bundles(Target, P),
	  builder_cmd(build_nodocs, P, Opts),
	    fail
	; true
	),
	% TODO: add dependencies instead of fixing a build order
	% Treat bundle
	cmd_message(Target, "building [no docs]", []),
	% TODO: (missing for root)
	cmd_message(Target, "built [no docs]", []).
builder_cmd_(build_nodocs, Target, Opts) :- !,
	check_ready_for_build(build_nodocs, Target),
	( has_cmd1(prebuild_nodocs, Target) ->
	    cmd_message(Target, "building [prebuild no docs]", []),
	    builder_cmd(prebuild_nodocs, Target, Opts),
	    cmd_message(Target, "built [prebuild no docs]", [])
	; true
	),
	cmd_message(Target, "building [no docs]", []),
	( has_cmd1(build_nodocs, Target) ->
	    builder_cmd1(build_nodocs, Target, Opts)
	; '$bundle_id'(Target) ->
	    % (default: prebuild, and build bin and libraries)
	    builder_cmd(build_bin, Target, Opts),
	    builder_cmd(build_libraries, Target, Opts)
	; format(user_error, "ERROR: unknown bundle '~w'.~n", [Target]),
	  halt(1)
        ),
	cmd_message(Target, "built [no docs]", []),
	% Treat item_subs
	( builder_pred(Target, item_subs(SubPs)) -> true ; fail ),
	builder_cmd_on_set(build_nodocs, SubPs, Opts),
	% Treat sub-bundles
	% TODO: (missing for non-root)
	true.
builder_cmd_(prebuild_docs, Target, Opts) :- !,
	% TODO: prebuild docs needs a different order in processing of
	% item_subs and sub-bundles; can it be simplified?
	check_ready_for_build(prebuild_docs, Target),
	% Treat sub-bundles
	( % (failure-driven loop)
	  enum_sub_bundles(Target, P),
	  builder_cmd(prebuild_docs, P, Opts),
	    fail
	; true
	),
	% Treat item_subs (for prebuild, it must be done before main)
	( builder_pred(Target, item_subs(SubPs)) -> true ; fail ),
	builder_cmd_on_set(prebuild_docs, SubPs, Opts),
	%
	( with_docs(yes) ->
	    ( has_cmd1(prebuild_docs, Target) ->
	        cmd_message(Target, "building [prebuild docs]", []),
	        ensure_builddir, % TODO: needed?
		ensure_builddir_doc, % TODO: needed?
	        builder_cmd1(prebuild_docs, Target, Opts),
		cmd_message(Target, "built [prebuild docs]", [])
	    ; % (default)
	      true
	    )
	; true
	).
builder_cmd_(build_docs, Target, Opts) :- !, % (internal, without prebuild)
	check_ready_for_build(build_docs, Target),
	%
	builder_cmd(prebuild_docs, Target, Opts),
	%
	( with_docs(yes) ->
	    cmd_message(Target, "building [build docs]", []),
	    ( has_cmd1(build_docs, Target) ->
	        builder_cmd1(build_docs, Target, Opts)
	    ; % (default: readmes and manuals)
	      builder_cmd(build_docs_readmes, Target, Opts),
	      builder_cmd(build_docs_manuals, Target, Opts)
	    ),
	    cmd_message(Target, "built [build docs]", [])
	; normal_message("documentation omitted", [])
	),
	% Treat sub-bundles
	( % (failure-driven loop)
	  enum_sub_bundles(Target, P),
	  builder_cmd(build_docs, P, Opts),
	    fail
	; true
	),
	% Treat item_subs (for prebuild, it be done before main)
	( builder_pred(Target, item_subs(SubPs)) -> true ; fail ),
	builder_cmd_on_set(build_docs, SubPs, Opts).
builder_cmd_(build_libraries, Target, Opts) :- root_bundle(Target), !, % (special case)
	check_ready_for_build(build_libraries, Target),
	( % (failure-driven loop)
	  enum_sub_bundles(Target, P),
	  builder_cmd(build_libraries, P, Opts),
	    fail
	; true
	).
%
builder_cmd_(build_bin, Target, Opts) :- root_bundle(Target), !,
	check_ready_for_build(build_bin, ciao),
	( % (failure-driven loop)
	  enum_sub_bundles(Target, P),
	  builder_cmd(build_bin, P, Opts),
	    fail
	; true
	).
%
% Deletes all intermediate files, but leaves configuration settings
builder_cmd_(clean, Target, Opts) :- !,
	builder_cmd(clean_docs, Target, Opts),
	builder_cmd(clean_nodocs, Target, Opts).
%
% Like 'clean', but keeps documentation targets.
% (This reverses the 'build_nodocs' and part of 'build_docs' actions)
builder_cmd_(clean_nodocs, Target, Opts) :- !,
	check_ready_for_build(clean_nodocs, Target),
	% Treat sub-bundles
	findall(P, enumrev_sub_bundles(Target, P), Ps),
	builder_cmd_on_set(clean_nodocs, Ps, Opts),
	% Treat item_subs
	( builder_pred(Target, item_subs(SubPs)) -> true ; fail ),
	builder_cmd_on_set(clean_nodocs, SubPs, Opts),
	%
	cmd_message(Target, "cleaning [no docs]", []),
	builder_cmd(clean_norec, Target, Opts),
	cmd_message(Target, "cleaned [no docs]", []).
%
% Like 'clean_nodocs' but non-recursive on sub-bundles
builder_cmd_(clean_norec, Target, Opts) :- !,
	check_ready_for_build(clean_norec, Target),
	%
	( root_bundle(Target) ->
	    clean_tree(~fsR(bundle_src(Target)/'Manifest')),
	    clean_tree(~fsR(bundle_src(Target)/doc)), % TODO: ad-hoc, clean .po,.itf, etc.
            % TODO: Clean Manifest/... in each bundle too
	    % TODO: clean all builddir except configuration?
	    builddir_clean(pbundle),
	    builddir_clean(bin),
	    builder_cmd(clean_norec, 'core/engine', []),
	    builder_cmd(clean_norec, 'core/exec_header', [])
	; has_cmd1(clean_norec, Target) ->
	    builder_cmd1(clean_norec, Target, Opts)
	; '$bundle_id'(Target) ->
	    % TODO: use some specific dirs instead
	    % TODO: does not work with CIAOCACHEDIR! fix
	    clean_tree(~fsR(bundle_src(Target)))
	; format(user_error, "ERROR: unknown bundle '~w'.~n", [Target]),
	  halt(1)
	).
% Clean of a directory tree, recursively
builder_cmd_(clean_tree(Dir), '$no_bundle', _Opts) :- !,
	clean_tree(Dir).
%
% Clean the final documentation targets (READMEs and manuals)
builder_cmd_(clean_docs, Target, Opts) :- !,
	check_ready_for_build(clean_docs, Target),
	% TODO: refine targets
	findall(P, enum_sub_bundles(Target, P), Ps),
	builder_cmd_on_set(clean_docs, Ps, Opts),
	builder_cmd(clean_docs_manuals, Target, Opts),
	builder_cmd(clean_docs_readmes, Target, Opts).
%
% Like 'clean' but also removes configuration settings.
builder_cmd_(distclean, Target, Opts) :- !,
	check_ready_for_build(distclean, Target),
	builder_cmd(clean, Target, Opts),
	%
	% Clean config (this must be the last step)
	cmd_message(Target, "cleaning [config]", []),
	builder_cmd(configclean, Target, Opts),
	( root_bundle(Target) ->
	    % TODO: make sure that no binary is left after 'clean' (outside builddir)
	    builddir_clean_bundlereg,
	    builddir_clean(all)
	; true
	).
%
% Clean the bundle configuration
% TODO: split in a configclean for each (sub)bundle?
% Warning! configclean is the last cleaning step. If you clean all
%          the configuration files then many scripts will not run.
builder_cmd_(configclean, Target, _Opts) :- !,
	check_ready_for_build(configclean, Target),
	( root_bundle(Target) ->
	    builddir_clean(config)
	; true
	).
% ----------
builder_cmd_(Cmd, Target, Opts) :-
	builder_cmd1(Cmd, Target, Opts).

builder_cmd1(Cmd, Target, Opts) :-
	set_params(Opts),
	split_target(Target, Bundle, Part),
	builder_hookcmd(Bundle, Part, Cmd).

has_cmd1(Cmd, Target) :-
	split_target(Target, Bundle, Part),
	bundle_defines_hookcmd(Bundle, Part, Cmd).

split_target(Target, Bundle, Part) :-
	path_split_list(Target, Cs),
	( Cs = [Bundle, Part] ->
	    true
	; Cs = [Bundle] ->
	    Part = ''
	; throw(unknown_bundle(Target))
	).

:- export(builder_pred/2).
builder_pred(Target, Head) :-
	split_target(Target, Bundle, Part),
	builder_hookpred(Bundle, Part, Head).

% ---------------------------------------------------------------------------
:- doc(section, "Invoking the Configuration").

:- use_module(library(bundle/bundle_flags),
	[get_bundle_flag/2,
	 restore_all_bundle_flags/0]).
:- use_module(ciaobld(bundle_configure), [config_noscan/0]).

% Invoke configuration (do not scan bundles)
do_config_noscan :-
	ensure_load_bundleconfig_rules, % TODO: make it more fine grained (not always needed, not all bundles needed)
 	restore_all_bundle_flags, % TODO: Necessary? bundle(bundle_flags) contains an initialization directive
	config_noscan.

% ---------------------------------------------------------------------------
:- use_module(library(system_extra), [using_tty/0]).

% Show a help message after (only when run from a TTY)
post_config_message(Target) :-
	( using_tty ->
	    show_post_config_message(Target)
	; true
	).

show_post_config_message(Target) :- root_bundle(Target), !,
	normal_message(
"Please check that all the configuration values above are correct. If
not, you can change or customize the configuration using the command
line or --interactive configure flag.

To continue the installation, execute 'build' and 'install' commands.", []).
show_post_config_message(_).

% ===========================================================================
:- doc(section, "Promote bootstrap compiler (dangerous!)").

:- use_module(ciaobld(interactive_aux), [ask_yesno/1]).
:- use_module(ciaobld(config_common), [default_eng/1]).

% TODO: Create a backup/ directory with a NODISTRIBUTE flag?
% TODO: This is an operation of core/ciaoc bundle
%
% NOTE: It only saves the static ciaoc (but it could save other
% bootstrapping data like documentation in the future).

do_boot_promote(Target) :-
	cmd_message(Target, "promoting bootstrap", []),
	%
	( root_bundle(Target) ->
	    true
	; % TODO: implement configuration for individual bundles
	  format(user_error, "ERROR: Cannot promote bundle '~w'.~n", [Target]),
	  halt(1)
	),
	%
	check_ready_for_build(boot_promote, Target),
	ask_promote_bootstrap(~default_eng).

ask_promote_bootstrap(EngMainMod) :-
	normal_message("The current compiler, including automatically generated C code for the", []),
	normal_message("emulator will become the next bootstrap system.", []),
	%
	normal_message("(Warning: do promote unless you are completely sure)", []),
	normal_message("", []),
	( ask_yesno("Are you sure?"),
	  ask_yesno("Really?") ->
	    promote_bootstrap(EngMainMod),
	    normal_message("Bootstrap compiler promoted", [])
	; normal_message("Promotion canceled", [])
	).

% ===========================================================================

% Check that there exist a configuration. It also implies that there
% exist a running ciao_builder and that bundles has been scanned.

:- use_module(library(system), [file_exists/1]).

check_ready_for_build(Cmd, Target) :-
	( % TODO: fsR/1 will fail if bundles are not scanned (that also means that there is no configuration)
	  ConfigSH = ~fsR(builddir(build)/'ciao.config_saved'),
	  file_exists(ConfigSH) ->
	    true
	; format(user_error, "ERROR: Cannot do '~w' on bundle '~w' without a configuration. Please run 'configure' before.~n", [Cmd, Target]),
	  halt(1)
	).

% ============================================================================

:- include(ciaobld(bundlehooks/bundlehooks_defs)).

% ============================================================================
% Load bundle metasrc (configuration rules and build hooks)

% TODO: move to other module?

% TODO: add reference counting?
% TODO: reused for bundleconfig too, change name?
:- use_module(ciaobld(bundlehooks_holder)).

% % A descriptive name for the target (bundle or subtarget)
% target_desc_name(Target, Name) :-
% 	( builder_pred(Target, desc_name(Name0)) ->
% 	    Name = Name0
% 	; Name = Target % use plain target name
% 	).

:- export(ensure_load_bundlehooks/1).
ensure_load_bundlehooks(Bundle) :-
	% display(user_error, 'LOADING_BUNDLE_HOOKS'(Bundle)), nl(user_error),
	% TODO: (assume that all bundles define at least desc_name/1)
	( BundleHooksMod = ~atom_concat(Bundle, '.hooks'),
	  m_bundlehook_decl(BundleHooksMod, _, _) ->
	    true
	; load_bundle_metasrc(Bundle, bundle_hooks)
	).

load_bundle_metasrc(Bundle, Metasrc) :-
	( '$bundle_id'(Bundle), BundleDir = ~fsR(bundle_src(Bundle)) ->
	    true
	; throw(unknown_bundle(Bundle))
	),
	% TODO: make it optional (some bundles may not need customized hooks)
	bundle_metasrc(Bundle, Metasrc, BundleMetasrcFile0),
	fsR(BundleMetasrcFile0, BundleMetasrcFile),
	( file_exists(BundleMetasrcFile) ->
	    working_directory(PWD, BundleDir), % TODO: Needed here?
	    ( catch(bundlehooks_holder:do_use_module(BundleMetasrcFile), 
	            E, R = exception(E)) ->
	        R = ok
	    ; R = fail
	    ),
	    cd(PWD),
	    ( R = exception(E) -> throw(E)
	    ; R = fail -> fail
	    ; true
	    )
	; optional_metasrc(Metasrc) ->
	    true
	; % TODO: write handler?
	  throw(no_bundle_metasrc(Bundle, Metasrc))
	).

% TODO: use optional_metasrc/1 (do not unload if it was optional)
unload_bundle_metasrc(Bundle, Metasrc) :-
	bundle_metasrc(Bundle, Metasrc, BundleMetasrcFile0),
	fsR(BundleMetasrcFile0, BundleMetasrcFile),
	bundlehooks_holder:do_unload(BundleMetasrcFile).

optional_metasrc(bundle_config).

:- data bundleconfig_rules_loaded/0.

:- export(ensure_load_bundleconfig_rules/0).
% TODO: make it fine-grained (one bundle at a time)
ensure_load_bundleconfig_rules :-
	bundleconfig_rules_loaded, !.
ensure_load_bundleconfig_rules :-
	assertz_fact(bundleconfig_rules_loaded),
	( % (failure-driven loop)
	  '$bundle_id'(Bundle),
	    load_bundle_metasrc(Bundle, bundle_config),
	    fail
	; true
	).

% ============================================================================
% Invoke a command on Bundle that needs bundle hooks. Bundle hooks for
% the bundle are loaded automatically. The bundle Manifest must be
% already registered.

% TODO: Simplify!

builder_hookcmd(Bundle, Part, Cmd) :-
	ensure_load_bundleconfig_rules, % TODO: make it more fine grained (not always needed, not all bundles needed)
	ensure_load_bundlehooks(Bundle), % TODO: Not unloaded! (do refcount or gc of modules)
	%
	% TODO: is cd/1 really necessary here?
	BundleDir = ~fsR(bundle_src(Bundle)),
	working_directory(PWD, BundleDir),
	catch(bundlehook_call(Bundle, Part, Cmd), E, OK = no(E)),
	( var(OK) -> OK = yes ; true ),
	cd(PWD),
	%
%	display(user_error, 'UNLOADING_BUNDLE_HOOKS'(Bundle)), nl(user_error),
	%
	( OK = no(E) ->
	    throw(builder_cmd_failed(Bundle, Part, Cmd, E))
	; true
	).

% The command has an explicit definition in Bundle .hooks.pl
% TODO: Synchronize with builder_hookcmd/3
bundle_defines_hookcmd(Bundle, Part, Cmd) :-
	ensure_load_bundleconfig_rules,
	ensure_load_bundlehooks(Bundle),
	%
	BundleHooksMod = ~atom_concat(Bundle, '.hooks'),
	m_bundlehook_decl(BundleHooksMod, Part, Cmd).

% (for predicates, not commands)
% TODO: Synchronize with builder_hookcmd/3
builder_hookpred(Bundle, Part, Head) :-
	ensure_load_bundleconfig_rules,
	ensure_load_bundlehooks(Bundle),
	%
	BundleHooksMod = ~atom_concat(Bundle, '.hooks'),
	( m_bundlehook_decl(BundleHooksMod, Part, Head) ->
	    m_bundlehook_do(BundleHooksMod, Part, Head)
	; default_pred(Head, Bundle, Part)
	).

% Default pred, when no hook is provided (true, fail, etc.)
default_pred(desc_name(_), _, _) :- !, fail.
default_pred(manual_dir(_), _, _) :- !, fail.
default_pred(readme_path(_), _, _) :- !, fail.
default_pred(item_def(_), _, _) :- !, fail.
default_pred(item_subs(SubPs), _, _) :- !, SubPs = [].
default_pred(Head, Bundle, Part) :-
	% Everything else is not in the interface
	functor(Head, F, N),
	throw(error(bundlehook_pred_undeclared(Bundle,Part,F/N), builder_hookpred/3)).

:- use_module(ciaobld(bundle_configure),
	[config_list_flags/1,
	 config_describe_flag/1,
	 config_set_flag/1]).
%
:- use_module(ciaobld(bundle_hash), [gen_bundle_commit_info/1]).
%
:- use_module(library(bundle/bundle_info),
	[list_bundles/0, bundle_info/1]).
%
% Backends for pbundle (distribution) generation 
% TODO: disable 'unused module' warnings
:- use_module(ciaobld(pbundle_generator)).
%
:- use_module(ciaobld(pbundle_gen_win32)).
:- use_module(ciaobld(pbundle_gen_rpm)).
:- use_module(ciaobld(pbundle_gen_mac)).
:- use_module(ciaobld(pbundle_gen_src)).
:- use_module(ciaobld(pbundle_gen_bin)).

% (hooks for gen_pbundle)
:- include(ciaobld(pbundle_gen_hookdefs)).

bundlehook_call(Bundle, Part, Cmd) :-
	( bundlehook_call_(Cmd, Bundle, Part) ->
	    true
	; throw(error(bundlehook_call/3, bundlehook_failed(Bundle,Part,Cmd)))
	).

bundlehook_call_(config_list_flags, Bundle, '') :- !,
	config_list_flags(Bundle).
bundlehook_call_(config_describe_flag, Bundle, '') :- !,
	% (the flag is the value of ciao:describe_flag)
	config_describe_flag(Bundle).
bundlehook_call_(config_set_flag, Bundle, '') :- !,
	% (the flag is the value of ciao:set_flag_flag)
	% (the value is the value of ciao:set_flag_value)
	config_set_flag(Bundle).
%
bundlehook_call_(build_docs_readmes, Bundle, '') :- !,
	( with_docs(yes) ->
	    ensure_builddir, % TODO: needed?
	    ensure_builddir_doc, % TODO: needed?
	    build_docs_readmes(Bundle)
	; true
	).
bundlehook_call_(build_docs_manuals, Bundle, '') :- !,
	( with_docs(yes) ->
	    ensure_builddir, % TODO: needed?
	    ensure_builddir_doc, % TODO: needed?
	    build_docs_manuals(Bundle)
	; true
	).
%
bundlehook_call_(clean_docs_readmes, Bundle, '') :- !,
	( get_bundle_readme(Bundle, Readme),
	    del_file_nofail(Readme),
	    fail
	; true
	).
bundlehook_call_(clean_docs_manuals, Bundle, '') :- !,
	% TODO: use Manifest
	% TODO: this cleans all manuals at once; use lpdoc instead?
	( root_bundle(Bundle) ->
	    remove_dir(~fsR(builddir(build)/doc))
	; true
	).
%
bundlehook_call_(install, Bundle, '') :- !,
	% (install main before sub-bundles)
	% TODO: this is better for docs, but should be done atomically
	%
	% Treat bundle
	cmd_message(Bundle, "installing [no docs]", []),
	do_install_bindir(Bundle),
	do_install_storedir(Bundle),
	bundlehook_call__(install, Bundle, ''),
	cmd_message(Bundle, "installed [no docs]", []),
	% Treat sub-bundles
	sub_bundles_do_hookcmd(Bundle, install),
	% Docs
	bundleitem_do(docs(Bundle), Bundle, install),
	% Activate
	bundlehook_call(Bundle, '', bundle_activate).
bundlehook_call_(uninstall, Bundle, '') :- !,
	% (uninstall sub-bundles before main)
	% TODO: this is better for docs, but should be done atomically
	%
	% Deactivate
	bundlehook_call(Bundle, '', bundle_deactivate),
	% Docs
	bundleitem_do(docs(Bundle), Bundle, uninstall),
	% Treat sub-bundles
        revsub_bundles_do_hookcmd(Bundle, uninstall),
	% Treat bundle
	cmd_message(Bundle, "uninstalling [no docs]", []),
	bundlehook_call__(uninstall, Bundle, ''),
	do_uninstall_storedir(Bundle),
	do_uninstall_bindir(Bundle),
	cmd_message(Bundle, "uninstalled [no docs]", []).
bundlehook_call_(install_docs, Bundle, '') :- !, % (no hooks)
	% (install main before sub-bundles)
	bundleitem_do(docs(Bundle), Bundle, install),
	sub_bundles_do_hookcmd(Bundle, install_docs).
bundlehook_call_(uninstall_docs, Bundle, '') :- !, % (no hooks)
	% (uninstall sub-bundles before main)
	revsub_bundles_do_hookcmd(Bundle, uninstall_docs),
	bundleitem_do(docs(Bundle), Bundle, uninstall).
bundlehook_call_(bundle_activate, Bundle, '') :- !,
	install_bundlereg(Bundle),
	bundlehook_call(Bundle, '', register).
bundlehook_call_(bundle_deactivate, Bundle, '') :- !,
	bundlehook_call(Bundle, '', unregister),
	uninstall_bundlereg(Bundle).
%
% pbundle generation
bundlehook_call_(gen_pbundle(Kind), Bundle, '') :- !,
	gen_pbundle_hook(Kind, Bundle, []).
%
% TODO: Used from ciaobot
bundlehook_call_(gen_bundle_commit_info, Bundle, '') :- !,
	ensure_builddir,
	gen_bundle_commit_info(Bundle).
% Show bundle info
bundlehook_call_(info, Bundle, '') :- !,
	bundle_info(Bundle).
%
bundlehook_call_(Cmd, Bundle, '') :-
	cmd_rec(Cmd),
	!,
	sub_bundles_do_hookcmd(Bundle, Cmd),
	bundlehook_call__(Cmd, Bundle, '').
bundlehook_call_(Cmd, Bundle, Part) :- !,
	bundlehook_call__(Cmd, Bundle, Part).

bundlehook_call__(Cmd, Bundle, Part) :-
	BundleHooksMod = ~atom_concat(Bundle, '.hooks'),
	( m_bundlehook_decl(BundleHooksMod, Part, Cmd) ->
	    m_bundlehook_do(BundleHooksMod, Part, Cmd)
	; default_cmd(Cmd, Bundle, Part)
	).

% Default command action, when no hook is provided (true, fail, etc.)
default_cmd(prebuild_nodocs, _, _) :- !.
default_cmd(prebuild_docs, _, _) :- !.
default_cmd(build_nodocs, _, _) :- !. % TODO: build bin,libs,etc.
default_cmd(build_bin, _, _) :- !. % TODO: build cmds if there is a def
default_cmd(build_libraries, _, _) :- !. % TODO: build libs if there is a def
default_cmd(build_docs_readmes, _, _) :- !.
default_cmd(build_docs_manuals, _, _) :- !.
default_cmd(clean_docs_readmes, _, _) :- !.
default_cmd(clean_docs_manuals, _, _) :- !.
default_cmd(runbenchmarks, _, _) :- !. % TODO: look in tests?
default_cmd(runtests, _, _) :- !. % TODO: test sources
default_cmd(install, _, _) :- !. % TODO: install libs, cmds -- also decide which alias paths are public
default_cmd(uninstall, _, _) :- !. % TODO: uninstall libs, cmds -- also decide which alias paths are public
default_cmd(register, _, _) :- !.
default_cmd(unregister, _, _) :- !.
default_cmd(Cmd, Bundle, Part) :-
	( Cmd = item_prebuild_nodocs
	; Cmd = item_build_nodocs
	; Cmd = item_clean_norec
	; Cmd = item_install
	; Cmd = item_uninstall
	; Cmd = item_register
	; Cmd = item_unregister
	; fail
	),
	!,
	% In the interface, but not defined for this bundle
	functor(Cmd, F, N),
	throw(error(bundlehook_undefined(Bundle,Part,F/N), bundlehook_call/1)).
default_cmd(Cmd, Bundle, Part) :-
	( Cmd = custom_run(_)
	; fail
	),
	!,
	% In the interface, but not defined for this bundle
	functor(Cmd, F, N),
	throw(error(bundlehook_undefined(Bundle,Part,F/N), bundlehook_call/1)).
default_cmd(Cmd, Bundle, Part) :-
	% Not in the interface
	functor(Cmd, F, N),
	throw(error(bundlehook_undeclared(Bundle,Part,F/N), bundlehook_call/1)).

% Invoke bundle_do on sub-bundles of ParentBundle (dependencies first order)
sub_bundles_do_hookcmd(ParentBundle, Cmd) :- !, % (all sub-bundles)
	( % (failure-driven loop)
	  enum_sub_bundles(ParentBundle, P),
	    builder_hookcmd(P, '', Cmd),
	    fail
	; true
	).

% Invoke bundle_do on sub-bundles of ParentBundle (dependencies last order)
revsub_bundles_do_hookcmd(ParentBundle, Cmd) :- !, % (all sub-bundles)
	( % (failure-driven loop)
	  enumrev_sub_bundles(ParentBundle, P),
	    builder_hookcmd(P, '', Cmd),
	    fail
	; true
	).

% TODO: need other orders: parent last, parent last reverse order

% Commands that apply recursively to sub-bundles (parent first)
cmd_rec(runbenchmarks).
cmd_rec(runtests).

% Commands that apply recursively to sub-bundles (parent first, in reverse order)
cmd_revrec(_) :- fail.

% ============================================================================
% TODO: include 'groups'
% TODO: add dependencies

cmd_revlist(uninstall). % this command uses reverse order 

:- export(bundletree/1).
:- regtype bundletree(X) # "@var{X} is the symbolic description of the
   contents of a bundle".
% bundletree is the input of the generic install/uninstall operations
bundletree(_).

is_list([]).
is_list([_|_]). % wrong... but faster

% ----

item_cmd_message(prebuild_nodocs, "building [prebuild no docs]").
item_cmd_message(build_nodocs, "building [no docs]").
item_cmd_message(install, "installing").
item_cmd_message(uninstall, "uninstalling").
item_cmd_message(register, "registering").
item_cmd_message(unregister, "unregistering").

:- export(bundleitem_do/3).
bundleitem_do(X, Bundle, Cmd) :- is_list(X), !,
	( cmd_revlist(Cmd) ->
	    bundleitem_revlist_do(X, Bundle, Cmd)
	; bundleitem_list_do(X, Bundle, Cmd)
	).
bundleitem_do(item_group(Name, X), Bundle, Cmd) :- !,
	item_cmd_message(Cmd, CmdMsg0),
	cmd_message(Bundle, "(~s) ~s", [Name, CmdMsg0]),
	bundleitem_do(X, Bundle, Cmd).
%
bundleitem_do(only_global_ins(X), Bundle, Cmd) :- !,
	% consider X only if installation kind is global
	( ~instype = global ->
	    bundleitem_do(X, Bundle, Cmd)
	; true
	).
bundleitem_do(dir(Path), Bundle, Cmd) :- !, bundleitem_do(dir(Path, []), Bundle, Cmd).
bundleitem_do(dir(Path, Props), _Bundle, install) :- !,
	storedir_install(dir(~fsR(Path))), % perms?
	( member(files_from(SrcDir), Props) ->
	    % copy contents from Dir
	    storedir_install(dir_rec(SrcDir, ~fsR(Path)))
	; member(files_from(SrcDir, Pattern), Props) ->
	    % copy contents from Dir (as specified by Pattern)
	    ( % (failure-driven loop)
		member(File, ~glob(~fsR(SrcDir), Pattern)),
		storedir_install(file_noexec(~fsR(SrcDir/(File)), ~fsR(Path/(File)))),
	        fail
	    ; true
	    )
	; true
	).
bundleitem_do(dir(Path, Props), _Bundle, uninstall) :- !,
	( ~instype = global -> true ; throw(uninstall_requires_global) ),
	( member(del_rec, Props) ->
	    % on uninstall, remove contents recursively
	    % TODO: remove also the directory?
	    storedir_uninstall(dir_rec(~fsR(Path))) % TODO: use bundle
	; member(do_not_del, Props) ->
	    % do not delete on uninstall
	    true
	; storedir_uninstall(dir(~fsR(Path)))
	).
bundleitem_do(lib(Bundle, DirName), _Bundle, install) :- !, % (only instype=global) % TODO: use _Bundle
	% Install the module collection under DirName (along compiled files)
	cmd_message(Bundle, "installing '~w' libraries", [DirName]),
	( DirName = '.' ->
	    % TODO: simplify
	    storedir_install(dir_rec(bundle_src(Bundle), ~instciao_bundledir(Bundle)))
	; storedir_install(dir_rec(DirName, ~fsR(~instciao_bundledir(Bundle)/(DirName))))
	).
bundleitem_do(lib(Bundle, DirName), _Bundle, uninstall) :- !, % (only instype=global)
	% Uninstall the previously installed module collection DirName
	( DirName = '.' ->
	    % TODO: simplify
	    storedir_uninstall(dir_rec(~instciao_bundledir(Bundle)))
	; storedir_uninstall(dir_rec(~fsR(~instciao_bundledir(Bundle)/(DirName))))
	).
bundleitem_do(src(Bundle, DirName), _Bundle, install) :- !, % (only instype=global) % TODO: use _Bundle
	% Install the module collection under DirName (just source, e.g., for examples)
	cmd_message(Bundle, "installing '~w' source", [DirName]),
	storedir_install(src_dir_rec(DirName, ~fsR(~instciao_bundledir(Bundle)/(DirName)))).
bundleitem_do(src(Bundle, DirName), _Bundle, uninstall) :- !, % (only instype=global)
	% Uninstall the previously installed source-only module collection DirName
	storedir_uninstall(src_dir_rec(~fsR(~instciao_bundledir(Bundle)/(DirName)))).
bundleitem_do(docs(Bundle), _Bundle, install) :- !, % (for instype=global and instype=local) % TODO: use _Bundle
	bundle_install_docs(Bundle).
bundleitem_do(docs(Bundle), _Bundle, uninstall) :- !, % (for instype=global and instype=local)
	bundle_uninstall_docs(Bundle).
bundleitem_do(cmds_list(Bundle, Path, List), _Bundle, build_nodocs) :- !, % TODO: use _Bundle
	build_cmds_list(Bundle, Path, List).
bundleitem_do(cmds_list(Bundle, _, List), _Bundle, install) :- !, % TODO: use _Bundle
	storedir_install(cmds_list(Bundle, List)).
bundleitem_do(cmds_list(Bundle, _, List), _Bundle, uninstall) :- !,
 	storedir_uninstall(cmds_list(Bundle, List)).
bundleitem_do(lib_file_list(Bundle, Path, List), _Bundle, Cmd) :- !, % TODO: use _Bundle
	lib_file_list_do(List, Bundle, Path, Cmd).
bundleitem_do(bin_copy_and_link(K, Bundle, File, Props), _Bundle, install) :- !, % TODO: use _Bundle
	storedir_install(copy_and_link(K, Bundle, File)),
	( member(link_as(Link), Props) ->
	    storedir_install(link_as(K, Bundle, File, Link))
	; true
	).
bundleitem_do(bin_copy_and_link(K, Bundle, File, Props), _Bundle, uninstall) :- !,
	( member(link_as(Link), Props) ->
	    storedir_uninstall(link(K, Link))
	; true
	),
	storedir_uninstall(copy_and_link(K, Bundle, File)).
bundleitem_do(file(Path), _Bundle, uninstall) :- !,
	storedir_uninstall(file(Path)).
% Engine
bundleitem_do(eng(EngMainMod, EngOpts), _Bundle, build_nodocs) :- !,
	eng_build(EngMainMod, EngOpts),
	% Activate
 	eng_active_bld(EngMainMod).
bundleitem_do(eng(EngMainMod, _EngOpts), _Bundle, clean_norec) :- !,
	eng_clean(EngMainMod).
bundleitem_do(eng(EngMainMod, _EngOpts), Bundle, install) :- !,
	( ~instype = global -> true ; throw(install_eng_requires_global) ), % TODO: should not be needed
	storedir_install(eng_contents(Bundle, EngMainMod)),
	storedir_install(eng_active(Bundle, EngMainMod)).
bundleitem_do(eng(EngMainMod, _EngOpts), Bundle, uninstall) :- !,
	( ~instype = global -> true ; throw(uninstall_eng_requires_global) ), % TODO: should not be needed
	storedir_uninstall(eng_active(Bundle, EngMainMod)),
	storedir_uninstall(eng_contents(Bundle, EngMainMod)).
% Engine header stubs for executables
bundleitem_do(eng_exec_header(EngMainMod), _Bundle, build_nodocs) :- !,
	build_eng_exec_header(EngMainMod).
bundleitem_do(eng_exec_header(EngMainMod), _Bundle, clean_norec) :- !,
	clean_eng_exec_header(EngMainMod).
%
bundleitem_do(X, Bundle, Cmd) :-
	atom(X), % TODO: replace by item(_)
	path_concat(Bundle, X, Target),
	builder_pred(Target, item_def(Y)), !,
	bundleitem_do(Y, Bundle, Cmd).
bundleitem_do(X, Bundle, Cmd) :-
	atom(X), % TODO: replace by item(_)
	!,
	path_concat(Bundle, X, Target),
	( Cmd = prebuild_nodocs -> Cmd2 = item_prebuild_nodocs
	; Cmd = build_nodocs -> Cmd2 = item_build_nodocs
	; Cmd = clean_norec -> Cmd2 = item_clean_norec
	; Cmd = install -> Cmd2 = item_install
	; Cmd = uninstall -> Cmd2 = item_uninstall
	; Cmd = register -> Cmd2 = item_register
	; Cmd = unregister -> Cmd2 = item_unregister
	; fail
	),
	builder_cmd1(Cmd2, Target, []).
bundleitem_do(X, _Bundle, _Cmd) :-
	throw(error(unknown(X), bundleitem_do/3)).

% TODO: we would need dependencies here
% (uninstall is done in reverse order)
bundleitem_revlist_do(Xs, Bundle, Cmd) :-
	bundleitem_list_do(~reverse(Xs), Bundle, Cmd).

bundleitem_list_do([], _Bundle, _Cmd).
bundleitem_list_do([X|Xs], Bundle, Cmd) :- bundleitem_do(X, Bundle, Cmd), bundleitem_list_do(Xs, Bundle, Cmd).

lib_file_list_do([], _Bundle, _Path, _Cmd).
lib_file_list_do([X|Xs], Bundle, Path, Cmd) :-
	lib_file_item_do(X, Bundle, Path, Cmd),
	lib_file_list_do(Xs, Bundle, Path, Cmd).

lib_file_item_do(File-Props, Bundle, Path, install) :- !,
	( ~instype = global -> true ; throw(install_requires_global) ), % TODO: should not be needed
	lib_file_props(File, Props, Path, Props2),
	storedir_install(lib_file_copy_and_link(Props2, Bundle, Path, File)).
lib_file_item_do(File-Props, Bundle, Path, uninstall) :- !,
	( ~instype = global -> true ; throw(uninstall_requires_global) ), % TODO: should not be needed
	lib_file_props(File, Props, Path, Props2),
	storedir_uninstall(lib_file_copy_and_link(Props2, Bundle, File)).

lib_file_props(File, Props, Path, Props2) :-
	( member(copy_and_link, Props) ->
	    Props2 = [at_bundle, storedir_link]
	; member(to_abspath(To), Props) ->
	    Props2 = [to_abspath(To), storedir_link]
	; throw(unknown_props_lib_file(File, Props, Path))
	).
	
:- use_module(library(lists), [reverse/2]).

% ---------------------------------------------------------------------------
% create/delete directory where bundles are installed

do_install_storedir(Bundle) :- ~instype = global, !,
	% TODO: perms?
	storedir_install(dir(~instciao_storedir)),
	bundleitem_do(dir(~instciao_bundledir(Bundle), [del_rec]), Bundle, install).
do_install_storedir(_Bundle).

do_uninstall_storedir(Bundle) :- ~instype = global, !,
	bundleitem_do(dir(~instciao_bundledir(Bundle), [del_rec]), Bundle, uninstall),
	% delete if empty
	storedir_uninstall(dir_if_empty(~instciao_storedir)).
do_uninstall_storedir(_Bundle).

do_install_bindir(_Bundle) :-
	% TODO: perms?
	storedir_install(dir(~instciao_bindir)).

do_uninstall_bindir(_Bundle) :- ~instype = global, !,
	% Keep ~instciao_bindir (e.g., it may be /usr/bin)
	true.
do_uninstall_bindir(_Bundle) :-
	% delete if empty
	storedir_uninstall(dir_if_empty(~instciao_bindir)).

% ---------------------------------------------------------------------------
% Bundle registry installation (after bundle scan!)
% TODO: rename those predicates

:- use_module(ciaobld(bundle_scan),
	[create_bundlereg/2, remove_bundlereg/2,
	 ensure_global_bundle_reg_dir/0]).

%   % TODO: use bundle_reg_dir/2
%   % TODO: really? installation rewrites the bundlereg of each installed bundle
%   dir(~instciao_bundledir(core)/lib/bundlereg__auto, [do_not_del]),
%   %

install_bundlereg(Bundle) :-
	( ~instype = global ->
	    BundleDir = ~fsR(bundle_src(Bundle)),
	    ensure_global_bundle_reg_dir,
	    create_bundlereg(BundleDir, global)
	; true % (done in bundle scan)
	).

uninstall_bundlereg(Bundle) :-
	( ~instype = global ->
	    ensure_global_bundle_reg_dir,
	    remove_bundlereg(Bundle, global)
	; true % (nothing needed for local installations)
	).

% ===========================================================================

:- doc(subsection, "Documentation of a Bundle").

:- use_module(library(bundle/doc_flags), [docformat/1, docformatdir/2]).

:- use_module(library(pathnames), [path_concat/3, path_split/3]).
:- use_module(library(system), [file_exists/1]).
:- use_module(library(system_extra), [mkpath/1]).
:- use_module(library(source_tree),
	[copy_file_or_dir/2, remove_file_or_dir/1]).

:- use_module(library(aggregates), [findall/3]).

:- use_module(ciaobld(ciaoc_aux),
	[invoke_lpdoc/2, invoke_lpdoc/1]).

% Creation of README files (from .lpdoc to ascii)
% Output is moved to the bundle root directory.
build_docs_readmes(Bundle) :- with_docs(yes), !,
	( % (failure-driven loop)
	  builder_pred(Bundle, readme_path(Readme)),
	    ( Readme = as(SrcPath, OutName) ->
	        true
	    ; SrcPath = Readme,
	      OutName = Name
	    ),
	    path_split(SrcPath, _, Name),
	    BundleDir = ~fsR(bundle_src(Bundle)),
	    path_concat(BundleDir, SrcPath, SrcPath2),
	    path_split(SrcPath2, FilePath, _),
	    Ascii = ~atom_concat(Name, '.ascii'),
	    invoke_lpdoc(['-d', ~atom_concat('filepath=', FilePath),
	                  '-d', 'autogen_warning=yes',
	                  '-d', 'doc_mainopts=no_versioned_output',
	                  '-c', Ascii]),
	    DocSrc = ~fsR(builddir_doc(build)/Ascii),
	    copy_file_or_dir(DocSrc, OutName),
	    fail
	; true
	).
build_docs_readmes(_Bundle).

:- export(get_bundle_readme/2).
% TODO: duplicated in lpdoc_aux
% Output for bundle README files
get_bundle_readme(Bundle, R) :-
	builder_pred(Bundle, readme_path(Readme)),
	( Readme = as(_, Final) -> true
	; path_split(Readme, _, Final)
	),
	R = ~fsR(bundle_src(Bundle)/Final).

:- use_module(library(bundle/bundle_info), [bundle_version/2, bundle_patch/2]).

:- export(bundle_manual_base/2).
% Base name for manuals of Bundle
bundle_manual_base(Bundle) := R :-
	builder_pred(Bundle, manual_dir(Manual)),
	( Manual = as(_Dir, Base) -> true
	; Dir = Manual,
	  path_split(Dir, _, Base)
	),
	R = ~atom_concat([Base, '-', ~bundle_version(Bundle), '.', ~bundle_patch(Bundle)]).

% Creates the manuals
build_docs_manuals(Bundle) :- with_docs(yes), !,
	( % (failure-driven loop)
	  builder_pred(Bundle, manual_dir(Manual)),
	    ( Manual = as(SrcDir, _) ->
	        true
	    ; SrcDir = Manual
	    ),
	    BundleDir = ~fsR(bundle_src(Bundle)),
	    path_concat(BundleDir, SrcDir, R0),
	    path_concat(R0, 'SETTINGS', Settings),
	    invoke_lpdoc(Settings, [all]),
	    fail
	; true
	).
build_docs_manuals(_Bundle).

bundle_install_docs(Bundle) :- with_docs(yes), !,
	cmd_message(Bundle, "installing [docs]", []),
	( % (failure-driven loop)
	  ManualBase = ~bundle_manual_base(Bundle),
	  docformat(DocFormat),
	    docformatdir(DocFormat, TargetDir0),
	    TargetDir = ~rootprefixed(TargetDir0),
	    FileName = ~atom_concat([ManualBase, '.', DocFormat]),
	    Source = ~fsR(builddir_doc(build)/(FileName)),
	    ( file_exists(Source) ->
		Target = ~path_concat(TargetDir, FileName),
		( Source == Target ->
		    % TODO: Wrong note message (we are not really installing then!)
		    note(['Skipping copy of ', Target])
		; mkpath(TargetDir), % TODO: perms and owner?
		  copy_file_or_dir(Source, TargetDir)
		),
		bundle_install_docs_format_hook(DocFormat, Target)
	    ; warning(['File ', Source, ' not generated yet. Skipping copy'])
	    ),
	    fail
	; true
	),
	cmd_message(Bundle, "installed [docs]", []).
bundle_install_docs(_Bundle).

% TODO: skip it if documentation was not installed
bundle_uninstall_docs(Bundle) :- with_docs(yes), !,
	cmd_message(Bundle, "uninstalling [docs]", []),
	( % (failure-driven loop)
	  ManualBase = ~bundle_manual_base(Bundle),
	  docformat(DocFormat),
	    docformatdir(DocFormat, TargetDir0),
	    TargetDir = ~rootprefixed(TargetDir0),
	    FileName = ~atom_concat([ManualBase, '.', DocFormat]),
	    Target = ~path_concat(TargetDir, FileName),
	    ( bundle_uninstall_docs_format_hook(DocFormat, Target),
	      remove_file_or_dir(Target) ->
	        true
	    ; warning(['Could not uninstall documentation in ', 
	               DocFormat, ' format for ',
		       Bundle, ' (was it generated?)'])
	    ),
	    fail
	; true
	),
	cmd_message(Bundle, "uninstalled [docs]", []).
bundle_uninstall_docs(_Bundle).

% These predicates install the 'info' files in info dir.

:- use_module(ciaobld(info_installer)).

bundle_install_docs_format_hook(info, Target) :- !,
	dirfile_install_info(~fsR(builddir_doc(build)), Target).
bundle_install_docs_format_hook(_, _).

bundle_uninstall_docs_format_hook(info, Target) :- !,
	dirfile_uninstall_info(~fsR(builddir_doc(build)), Target).
bundle_uninstall_docs_format_hook(_, _).

% ===========================================================================

:- doc(subsection, "Bundle Management").

:- use_module(library(process), [process_call/3]).
:- use_module(library(http_get), [http_get/2]).
:- use_module(library(system), [mktemp_in_tmp/2]).

bundle_get(BundleAlias) :-
	bundle_fetch(BundleAlias, Bundle),
	builder_cmd(rescan_bundles, ~root_bundle, []), % TODO: implement single-bundle rescan
	reload_bundleregs, % (reload, bundles has been scanned)
	builder_cmd(build, Bundle, []).

% Fetch source code of bundle specified in BundleAlias
bundle_fetch(BundleAlias, Bundle) :-
	% Compute bundle dir and check status
	path_split(BundleAlias, _, Bundle),
	root_bundle_source_dir(RootDir),
	path_concat(RootDir, Bundle, BundleDir),
	( file_exists(BundleDir) ->
	    cmd_message(Bundle, "already exists, skipping fetch", []),
	    % TODO: Add a mark or compute a checksum so that we can check
	    % that this directory is not a user directory
	    normal_message("(to upgrade, please remove ~w)", [BundleDir])
	; bundle_fetch_(BundleAlias, Bundle, BundleDir)
	).

bundle_fetch_(BundleAlias, Bundle, BundleDir) :-
	% Fetch source
	bundle_src_url(BundleAlias, URL),
	cmd_message(Bundle, "fetching source from ~w", [URL]),
	mktemp_in_tmp('ciao-fetch-XXXXXX', File),
	mkpath(BundleDir),
	catch(http_get(URL, file(File)), _E, fail),
	% Uncompress
	process_call(path(tar), [
          '-x', '--strip-components', '1',
	  '-C', BundleDir,
	  '-f', File], [status(0)]),
	!.
bundle_fetch_(BundleAlias, _, _) :-
	format(user_error, "ERROR: Bundle fetch failed for `~w'.~n", [BundleAlias]),
	halt(1).

% TODO: only github.com is currently supported
bundle_src_url(BundleAlias, URL) :-
	atom_concat('github.com/', BundleAlias, _),
	!,
	atom_concat(['https://', BundleAlias, '/archive/master.tar.gz'], URL).
bundle_src_url(BundleAlias, _URL) :-
	format(user_error, "ERROR: Unrecognized bundle alias path `~w'.~n", [BundleAlias]),
	halt(1).
