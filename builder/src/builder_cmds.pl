:- module(_, [], [fsyntax, hiord, assertions, regtypes, isomodes]).

:- doc(title,  "Builder Commands").
:- doc(author, "Jose F. Morales").

:- use_module(library(aggregates), [findall/3]).
:- use_module(library(pathnames), [path_split_list/2]).
:- use_module(library(terms), [atom_concat/2]).

:- use_module(library(bundle/bundle_params),
	[set_bundle_param_value/2,
	 bundle_param_value/2]).
%
:- use_module(library(bundle/bundle_paths),
	[bundle_path/3, bundle_path/4, bundle_metasrc/3]).
:- use_module(library(bundle/bundle_info), [
	root_bundle/1,
	enum_sub_bundles/2,
	enumrev_sub_bundles/2
	]).
:- use_module(engine(internals), [
	reload_bundleregs/0,
	'$bundle_id'/1]).

:- use_module(ciaobld(builder_aux), [
	root_bundle_source_dir/1,
	rootprefixed/2,
	ensure_builddir/2,
        storedir_install/1,
        storedir_uninstall/1,
	eng_active_bld/1
	]).
:- use_module(ciaobld(bundle_get), [bundle_fetch/2, bundle_rm/1]).
:- use_module(ciaobld(ciaoc_aux),
	[promote_bootstrap/1,
	 %
	 build_eng_exec_header/1,
	 clean_eng_exec_header/1,
	 %
	 build_libs/2,
	 build_cmds_list/3,
	 builddir_clean/2,
	 clean_bundlereg/1,
	 clean_tree/1,
	 clean_mod/1
	]).
:- use_module(ciaobld(eng_maker),
	[eng_build/1,
	 eng_clean/1
	]).
:- use_module(library(system_extra), [del_file_nofail/1]).
:- use_module(ciaobld(config_common), [
    instype/1,
    instciao_bindir/1,
    instciao_storedir/1,
    instciao_bundledir/2,
    with_docs/1
]).
:- use_module(ciaobld(messages_aux), [cmd_message/3, normal_message/2]).
:- use_module(library(messages), [error_message/2]).

% ===========================================================================
:- doc(section, "Builder commands").

:- doc(bug, "See cleaning targets for @apl{lpdoc} and fix it. See other
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

:- export(builder_cleanup/0).
:- pred builder_cleanup # "Cleanup the builder state".

builder_cleanup :-
	retractall_fact(builder_cmd_done(_, _, _)).

:- export(builder_cmd/3).
:- pred builder_cmd(Cmd, Target, Opts) # "Perform command @var{Cmd} on
   target @var{Target} with options @var{Opts}".

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
	builder_cmd(full_install, Target, Opts2).
builder_cmd_(global_install, Target, Opts) :- !,
	Opts2 = [flag(ciao:instype, 'global')|Opts],
	builder_cmd(full_install, Target, Opts2).
builder_cmd_(local_install_paranoid, Target, Opts) :- !,
	Opts2 = [flag(ciao:instype, 'local'),
		 flag(ciao:unused_pred_warnings, 'yes')|Opts],
	builder_cmd(full_install, Target, Opts2).
builder_cmd_(full_install, Target, Opts) :- !,
	check_builder_update,
	% (get ciao path from Target)
	( root_bundle(Target) ->
	    root_bundle_source_dir(CiaoSrc),
	    Path = CiaoSrc
	; % TODO: implement full_install for other bundles?
	  throw(error_msg("Cannot full_install `~w'.", [Target]))
	),
	scan_bundles_at_path(Path, no),
	builder_cmd(config_noscan, Target, Opts),
	builder_cmd(build, Target, []),
	builder_cmd(install, Target, []).
%
builder_cmd_(boot_promote, Target, _Opts) :- !,
	do_boot_promote(Target).
%
builder_cmd_(configure, Target, Opts) :- !, % TODO: make it '$no_bundle' cmd?
	( member(flag(ciao:list_flags, true), Opts) ->
	    set_params(Opts), % TODO: needed?
	    builder_cmd(config_list_flags, Target, Opts)
	; member(flag(ciao:describe_flag, _Opt), Opts) ->
	    set_params(Opts), % TODO: needed, pass arg instead
	    builder_cmd(config_describe_flag, Target, Opts)
	; member(flag(ciao:set_flag_flag, _Opt), Opts) ->
	    set_params(Opts), % TODO: needed, pass arg instead
	    builder_cmd(config_set_flag, Target, Opts)
	; member(flag(ciao:get_flag_flag, _Opt), Opts) ->
	    set_params(Opts), % TODO: needed, pass arg instead
	    builder_cmd(config_get_flag, Target, Opts)
	; % (get ciao path from Target)
	  check_builder_update,
	  ( root_bundle(Target) ->
	      root_bundle_source_dir(CiaoSrc),
	      Path = CiaoSrc
	  ; throw(error_msg("Cannot configure `~w'.", [Target]))
	  ),
	  scan_bundles_at_path(Path, no),
	  builder_cmd(config_noscan, Target, Opts),
	  post_config_message(Target)
	).
%
builder_cmd_(config_noscan, Target, Opts) :- !,
	split_target(Target, Bundle, Part),
	( Part = '' -> true
	; throw(error_msg("Cannot configure separate bundle items (please configure `~w' instead).", [Bundle]))
	),
	cmd_message(Target, "configuring", []),
	set_params(Opts),
	do_config_noscan(Bundle),
	cmd_message(Target, "configured", []).
%
builder_cmd_(rescan_bundles(Path), '$no_bundle', _Opts) :- !,
	scan_bundles_at_path(Path, yes).
% List bundles
builder_cmd_(list, '$no_bundle', _Opts) :- !,
	list_bundles.
% Download and install bundles
builder_cmd_(get(BundleAlias), '$no_bundle', _Opts) :- !,
	% Fetch and build the bundle specified in BundleAlias
	bundle_fetch(BundleAlias, Bundle),
	builder_cmd(build, Bundle, []).
% Remove a downloaded bundle
builder_cmd_(rm(BundleAlias), '$no_bundle', _Opts) :- !,
	bundle_rm(BundleAlias).
%
builder_cmd_(build, Target, Opts) :- !,
	% TODO: use fsmemo package to order build tasks (build_docs on any bundle depends on 'build_nodocs lpdoc')
	builder_cmd(build_nodocs, Target, Opts),
	builder_cmd(build_docs, Target, Opts).
builder_cmd_(build_nodocs, Target, Opts) :- root_bundle(Target), !,
	Bundle = Target, Part = '',
	check_bundle_has_config(build_nodocs, Bundle),
	% NOTE: 'core/ciaobase' must be built before anything else
	% TODO: add dependencies instead of fixing a build order
	% builder_cmd(build_nodocs, 'core/ciaobase', Opts), % (included in core/ build rules)
	% Treat sub-bundles
	( % (failure-driven loop)
	  Part = '', enum_sub_bundles(Bundle, P),
	  builder_cmd(build_nodocs, P, Opts),
	    fail
	; true
	).
builder_cmd_(build_nodocs, Target, Opts) :- !,
	% TODO: Treat dependent bundles first?
	split_target(Target, Bundle, Part),
	check_bundle_has_config(build_nodocs, Bundle),
	( bundle_defines_hookcmd(Bundle, Part, prebuild_nodocs) ->
	    cmd_message(Target, "building [prebuild no docs]", []),
	    builder_cmd(prebuild_nodocs, Target, Opts),
	    cmd_message(Target, "built [prebuild no docs]", [])
	; true
	),
	cmd_message(Target, "building [no docs]", []),
	% TODO: make sure that Target exists!
	set_params(Opts),
	builder_hookcmd(Bundle, Part, build_nodocs),
	cmd_message(Target, "built [no docs]", []),
	% Treat item_subs
	( builder_pred(Target, item_subs(SubPs)) -> true ; fail ),
	builder_cmd_on_set(build_nodocs, SubPs, Opts).
builder_cmd_(prebuild_docs, Target, Opts) :- !,
	% TODO: prebuild docs needs a different order in processing of
	% item_subs and sub-bundles; can it be simplified?
	split_target(Target, Bundle, Part),
	check_bundle_has_config(prebuild_docs, Bundle),
	% Treat sub-bundles
	( % (failure-driven loop)
	  Part = '', enum_sub_bundles(Bundle, P),
	  builder_cmd(prebuild_docs, P, Opts),
	    fail
	; true
	),
	% Treat item_subs (for prebuild, it must be done before main)
	( builder_pred(Target, item_subs(SubPs)) -> true ; fail ),
	builder_cmd_on_set(prebuild_docs, SubPs, Opts),
	%
	( with_docs(yes) ->
	    ( bundle_defines_hookcmd(Bundle, Part, prebuild_docs) ->
	        cmd_message(Target, "building [prebuild docs]", []),
	        ensure_builddir(Bundle, '.'), % TODO: needed?
		ensure_builddir(Bundle, 'doc'), % TODO: needed?
		set_params(Opts),
		builder_hookcmd(Bundle, Part, prebuild_docs),
		cmd_message(Target, "built [prebuild docs]", [])
	    ; % (default)
	      true
	    )
	; true
	).
builder_cmd_(build_docs, Target, Opts) :- !, % (internal, without prebuild)
	split_target(Target, Bundle, Part),
	check_bundle_has_config(build_docs, Bundle),
	%
	builder_cmd(prebuild_docs, Target, Opts),
	%
	( with_docs(yes) ->
	    cmd_message(Target, "building [build docs]", []),
	    ( bundle_defines_hookcmd(Bundle, Part, build_docs) ->
	        set_params(Opts),
		builder_hookcmd(Bundle, Part, build_docs)
	    ; builder_pred(Target, item_def(_)) ->
	        % TODO: nothing?
	        true
	    ; % (default: readmes and manuals)
              % TODO: make sure that Target exists!
	      builder_cmd(build_docs_readmes, Target, Opts),
	      builder_cmd(build_docs_manuals, Target, Opts)
	    ),
	    cmd_message(Target, "built [build docs]", [])
	; true % normal_message("documentation omitted", [])
	),
	% Treat sub-bundles
	( % (failure-driven loop)
	  Part = '', enum_sub_bundles(Bundle, P),
	  builder_cmd(build_docs, P, Opts),
	    fail
	; true
	),
	% Treat item_subs (for prebuild, it be done before main)
	( builder_pred(Target, item_subs(SubPs)) -> true ; fail ),
	builder_cmd_on_set(build_docs, SubPs, Opts).
%
% Deletes all intermediate files, but leaves configuration settings
builder_cmd_(clean, Target, Opts) :- !,
	builder_cmd(clean_docs, Target, Opts),
	builder_cmd(clean_nodocs, Target, Opts).
%
% Like 'clean', but keeps documentation targets.
% (This reverses the 'build_nodocs' and part of 'build_docs' actions)
builder_cmd_(clean_nodocs, Target, Opts) :- !,
	split_target(Target, Bundle, Part),
	check_bundle_has_config(clean_nodocs, Bundle),
	% Treat sub-bundles
	( Part = '' -> findall(P, enumrev_sub_bundles(Bundle, P), Ps) ; Ps = [] ),
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
	split_target(Target, Bundle, Part),
	check_bundle_has_config(clean_norec, Bundle),
	%
	( root_bundle(Target) ->
	    % TODO: for non-root?
	    clean_tree(~bundle_path(Bundle, 'Manifest')),
	    clean_tree(~bundle_path(Bundle, 'doc')), % TODO: ad-hoc, clean .po,.itf, etc.
            % TODO: Clean Manifest/... in each bundle too
	    % TODO: clean all builddir except configuration?
	    builddir_clean(Bundle, pbundle),
	    builddir_clean(Bundle, bin),
	    builder_cmd(clean_norec, 'core/engine', []),
	    builder_cmd(clean_norec, 'core/exec_header', [])
	; bundle_defines_hookcmd(Bundle, Part, clean_norec) ->
	    set_params(Opts),
	    builder_hookcmd(Bundle, Part, clean_norec)
	; builder_pred(Target, item_def(_)) ->
	    bundleitem_do(Part, Bundle, clean_norec)
	; '$bundle_id'(Bundle), \+ Part = '' ->
	    true % (ignore)
	; '$bundle_id'(Bundle), Part = '' ->
	    % TODO: use some specific dirs instead
	    % TODO: does not work with CIAOCACHEDIR! fix
	    clean_tree(~bundle_path(Bundle, '.'))
	; throw(error_msg("Unknown bundle '~w'.", [Target]))
	).
% Clean of a directory tree, recursively
builder_cmd_(clean_tree(Dir), '$no_bundle', _Opts) :- !,
	clean_tree(Dir).
%
% Clean the final documentation targets (READMEs and manuals)
builder_cmd_(clean_docs, Target, Opts) :- !,
	split_target(Target, Bundle, Part),
	check_bundle_has_config(clean_docs, Bundle),
	% TODO: refine targets
	( Part = '' -> findall(P, enum_sub_bundles(Bundle, P), Ps) ; Ps = [] ),
	builder_cmd_on_set(clean_docs, Ps, Opts),
	builder_cmd(clean_docs_manuals, Target, Opts),
	builder_cmd(clean_docs_readmes, Target, Opts).
%
% Like 'clean' but also removes configuration settings.
builder_cmd_(distclean, Target, Opts) :- !,
	split_target(Target, Bundle, _Part),
	check_bundle_has_config(distclean, Bundle),
	builder_cmd(clean, Target, Opts),
	%
	% Clean config (this must be the last step)
	cmd_message(Target, "cleaning [config]", []),
	builder_cmd(configclean, Target, Opts),
	( root_bundle(Target) ->
	    % TODO: non-root?
	    % TODO: make sure that no binary is left after 'clean' (outside builddir)
	    clean_bundlereg(local),
	    builddir_clean(Bundle, all)
	; true
	).
%
% Clean the bundle configuration
% TODO: split in a configclean for each (sub)bundle or workspace
% Warning! configclean is the last cleaning step. If you clean all
%          the configuration files then many scripts will not run.
builder_cmd_(configclean, Target, _Opts) :- !,
	split_target(Target, Bundle, _Part),
	check_bundle_has_config(configclean, Bundle),
	( root_bundle(Target) ->
	    % TODO: non-root?
	    builddir_clean(Bundle, config)
	; true
	).
% ----------
builder_cmd_(Cmd, Target, Opts) :-
	set_params(Opts),
	split_target(Target, Bundle, Part),
	builder_hookcmd(Bundle, Part, Cmd).

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
:- doc(section, "Scanning bundles in workspaces").

% Note: scanning bundles must be done before configuration
% Note: bundle_path/? cannot be used until bundles are scanned (may
%   fail or report outdated data)

:- use_module(ciaobld(bundle_scan), [bundle_scan/2]).

scan_bundles_at_path(Path, Rescan) :-
	% TODO: Assumes that Path is correct
	( root_bundle_source_dir(CiaoSrc),
	  Path = CiaoSrc ->
	    InsType = local
	; InsType = inpath(Path)
	),
	( Rescan = yes ->
	    clean_bundlereg(InsType) % clean previous bundlereg
	; true
	),
	bundle_scan(InsType, Path), % TODO: Allow rescanning of a single bundle
	reload_bundleregs. % (reload, bundles have been scanned)

% ---------------------------------------------------------------------------
:- doc(section, "Invoking the Configuration").

:- use_module(ciaobld(bundle_configure), [config_noscan/1]).
:- use_module(ciaobld(bundle_configure), [bundle_has_config/1]).
:- use_module(ciaobld(bundle_configure), [check_builder_update/0]).

% Invoke configuration (pre: bundles have been scanned)
do_config_noscan(Bundle) :-
	( root_bundle(Bundle) ->
	    true
	; % TODO: implement configuration for individual bundles
	  throw(error_msg("Cannot configure bundle '~w'.", [Bundle]))
	),
	BundleSet = all,
	config_noscan(BundleSet).

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
:- use_module(ciaobld(config_common), [default_eng_def/1]).

% TODO: Create a backup/ directory with a NODISTRIBUTE flag?
% TODO: This is an operation of core/ciaoc bundle
%
% NOTE: It only saves the static ciaoc (but it could save other
% bootstrapping data like documentation in the future).

do_boot_promote(Target) :-
	cmd_message(Target, "promoting bootstrap", []),
	%
	( root_bundle(Target) ->
	    Bundle = Target, _Part = ''
	; % TODO: implement configuration for individual bundles
	  throw(error_msg("Cannot promote bundle '~w'.", [Target]))
	),
	%
	check_bundle_has_config(boot_promote, Bundle),
	ask_promote_bootstrap(~default_eng_def).

ask_promote_bootstrap(Eng) :-
	normal_message("The current compiler, including automatically generated C code for the", []),
	normal_message("emulator will become the next bootstrap system.", []),
	%
	normal_message("(Warning: do promote unless you are completely sure)", []),
	normal_message("", []),
	( ask_yesno("Are you sure?"),
	  ask_yesno("Really?") ->
	    promote_bootstrap(Eng),
	    normal_message("Bootstrap compiler promoted", [])
	; normal_message("Promotion canceled", [])
	).

% ===========================================================================

% Check that there exist a configuration. It also implies that there
% exist a running ciao_builder and that bundles has been scanned.

check_bundle_has_config(Cmd, Bundle) :-
	( \+ '$bundle_id'(Bundle) ->
	    throw(error_msg("Unknown bundle '~w' (try 'rescan-bundles').", [Bundle]))
	; \+ bundle_has_config(Bundle) ->
	    throw(error_msg("Cannot do '~w' on bundle '~w' without a configuration. Please run 'configure' before.", [Cmd, Bundle]))
	; true
	).

% ============================================================================

:- include(ciaobld(bundlehooks/bundlehooks_defs)).

% ============================================================================
% Invoke a command on Bundle that needs bundle hooks. Bundle hooks for
% the bundle are loaded automatically. The bundle Manifest must be
% already registered.

:- use_module(ciaobld(builder_meta), [ensure_load_bundle_metasrc/2]).

% TODO: Do not run dynamically... ensure loaded before commands are called
% TODO: ensure_load_bundle_metasrc/2: not unloaded! (do refcount or gc of modules)

builder_hookcmd(Bundle, Part, Cmd) :-
	ensure_load_bundle_metasrc(Bundle, config_and_hooks),
	bundlehook_call(Bundle, Part, Cmd).

% The command has an explicit definition in Bundle .hooks.pl
% TODO: Synchronize with builder_hookcmd/3
bundle_defines_hookcmd(Bundle, Part, Cmd) :-
	ensure_load_bundle_metasrc(Bundle, config_and_hooks),
	BundleHooksMod = ~atom_concat(Bundle, '.hooks'),
	m_bundlehook_decl(BundleHooksMod, Part, Cmd).

% (for predicates, not commands)
% TODO: Synchronize with builder_hookcmd/3
builder_hookpred(Bundle, Part, Head) :-
	ensure_load_bundle_metasrc(Bundle, config_and_hooks),
	BundleHooksMod = ~atom_concat(Bundle, '.hooks'),
	( m_bundlehook_decl(BundleHooksMod, Part, Head) ->
	    m_bundlehook_do(BundleHooksMod, Part, Head)
	; default_pred(Head, Bundle, Part)
	).

% Default pred, when no hook is provided (true, fail, etc.)
default_pred(manual_dir(_), _, _) :- !, fail.
default_pred(readme_path(_), _, _) :- !, fail.
default_pred(bundle_def(_), _, _) :- !, fail.
default_pred(item_def(_), _, _) :- !, fail.
default_pred(item_subs(SubPs), _, _) :- !, SubPs = [].
default_pred(Head, Bundle, Part) :-
	% Everything else is not in the interface
	functor(Head, F, N),
	throw(error(bundlehook_pred_undeclared(Bundle,Part,F/N), builder_hookpred/3)).

:- use_module(ciaobld(bundle_configure),
	[config_list_flags/1,
	 config_describe_flag/1,
	 config_set_flag/2,
	 config_get_flag/2]).
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
bundlehook_call_(config_describe_flag, _Bundle, '') :- !,
	% (the flag is the value of ciao:describe_flag)
	( bundle_param_value(ciao:describe_flag, Flag) ->
	    true
	; throw(bug_in_config_describe_flag)
	),
	config_describe_flag(Flag).
bundlehook_call_(config_set_flag, _Bundle, '') :- !,
	% (the flag is the value of ciao:set_flag_flag)
	% (the value is the value of ciao:set_flag_value)
	( bundle_param_value(ciao:set_flag_flag, Flag),
	  bundle_param_value(ciao:set_flag_value, Value) ->
	    true
	; throw(bug_in_config_set_flag)
	),
	config_set_flag(Flag, Value).
bundlehook_call_(config_get_flag, _Bundle, '') :- !,
	% (the flag is the value of ciao:get_flag_flag)
	( bundle_param_value(ciao:get_flag_flag, Flag) ->
	    true
	; throw(bug_in_config_get_flag)
	),
	display(~config_get_flag(Flag)), nl.
%
bundlehook_call_(build_docs_readmes, Bundle, '') :- !,
	( with_docs(yes) ->
	    ensure_builddir(Bundle, '.'), % TODO: needed?
	    ensure_builddir(Bundle, 'doc'), % TODO: needed?
	    build_docs_readmes(Bundle)
	; true
	).
bundlehook_call_(build_docs_manuals, Bundle, '') :- !,
	( with_docs(yes) ->
	    ensure_builddir(Bundle, '.'), % TODO: needed?
	    ensure_builddir(Bundle, 'doc'), % TODO: needed?
	    build_docs_manuals(Bundle)
	; true
	).
%
bundlehook_call_(clean_docs_readmes, Bundle, '') :- !,
	( get_bundle_readme(Bundle, _Readme),
	    % (Not cleaned, assuming they are part of the sources)
	    % del_file_nofail(_Readme),
	    fail
	; true
	).
bundlehook_call_(clean_docs_manuals, Bundle, '') :- !,
	% TODO: use Manifest, use lpdoc to clean?
	% TODO: clean per workspace?
	( root_bundle(Bundle) ->
	    builddir_clean(Bundle, doc)
	; true
	).
%
bundlehook_call_(install, Bundle, Part) :- Part = '', !,
	% TODO: Support bundle_activate/register in the same way as prebuild_nodocs
	%
	% (install main before sub-bundles)
	% TODO: this is better for docs, but should be done atomically
	%
	% Treat bundle
	cmd_message(Bundle, "installing [no docs]", []),
	do_install_bindir(Bundle),
	do_install_storedir(Bundle),
	bundlehook_call__(install, Bundle, ''),
	% Treat item_subs
	( builder_pred(Bundle, item_subs(SubPs)) -> true ; fail ),
	builder_cmd_on_set(install, SubPs, []), % TODO: opts are wrong
        %( member(SubP, SubPs), bundleitem_do(SubP, Bundle, install), fail ; true ),
	%
	cmd_message(Bundle, "installed [no docs]", []),
	% Treat sub-bundles
	sub_bundles_do_hookcmd(Bundle, install),
	% Docs
	bundle_install_docs(Bundle),
	% Activate
	( Part = '' -> install_bundlereg(Bundle) ; true ),
	bundlehook_call(Bundle, '', register).
bundlehook_call_(install, Bundle, Part) :- !,
	bundlehook_call__(install, Bundle, Part),
	bundlehook_call(Bundle, Part, register).
bundlehook_call_(uninstall, Bundle, Part) :- Part = '', !,
	% (uninstall sub-bundles before main)
	% TODO: this is better for docs, but should be done atomically
	%
	% Deactivate
	bundlehook_call(Bundle, '', unregister),
	( Part = '' -> uninstall_bundlereg(Bundle) ; true ),
	% Docs
	bundle_uninstall_docs(Bundle),
	% Treat sub-bundles
        revsub_bundles_do_hookcmd(Bundle, uninstall),
	% Treat bundle
	cmd_message(Bundle, "uninstalling [no docs]", []),
	bundlehook_call__(uninstall, Bundle, ''),
	do_uninstall_storedir(Bundle),
	do_uninstall_bindir(Bundle),
	cmd_message(Bundle, "uninstalled [no docs]", []),
	% Treat item_subs
	( builder_pred(Bundle, item_subs(SubPs)) -> true ; fail ),
	builder_cmd_on_set(uninstall, SubPs, []). % TODO: opts are wrong
	%( member(SubP, SubPs), bundleitem_do(SubP, Bundle, uninstall), fail ; true ).
bundlehook_call_(uninstall, Bundle, Part) :- !,
	bundlehook_call(Bundle, Part, unregister),
	bundlehook_call__(uninstall, Bundle, Part).
bundlehook_call_(install_docs, Bundle, '') :- !, % (no hooks)
	% (install main before sub-bundles)
	bundle_install_docs(Bundle),
	sub_bundles_do_hookcmd(Bundle, install_docs).
bundlehook_call_(uninstall_docs, Bundle, '') :- !, % (no hooks)
	% (uninstall sub-bundles before main)
	revsub_bundles_do_hookcmd(Bundle, uninstall_docs),
	bundle_uninstall_docs(Bundle).
%
% pbundle generation
bundlehook_call_(gen_pbundle, Bundle, '') :- !,
	% TODO: 'src' and 'bin' Kind have WRONG names ('nothing' implies tgz and tbz)
	( bundle_param_value(ciao:kind, Kind) ->
	    true
	; throw(bug_in_gen_pbundle)
	),
	gen_pbundle_hook(Kind, Bundle, []).
%
% TODO: Used from ciaobot
bundlehook_call_(gen_bundle_commit_info, Bundle, '') :- !,
	ensure_builddir(~root_bundle, '.'),
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
default_cmd(Cmd, Bundle, Part) :- defcmd(Cmd), !,
	( Part = '', builder_pred(Bundle, bundle_def(Y)) ->
	    bundleitem_do(Y, Bundle, Cmd)
	; path_concat(Bundle, Part, Target), builder_pred(Target, item_def(Y)) ->
	    bundleitem_do(Y, Bundle, Cmd)
	; true
	).
default_cmd(prebuild_docs, _, _) :- !.
default_cmd(build_docs_readmes, _, _) :- !.
default_cmd(build_docs_manuals, _, _) :- !.
default_cmd(clean_docs_readmes, _, _) :- !.
default_cmd(clean_docs_manuals, _, _) :- !.
default_cmd(runbenchmarks, _, _) :- !. % TODO: look in tests?
default_cmd(test, _, _) :- !. % TODO: test sources
default_cmd(Cmd, Bundle, Part) :-
	( Cmd = custom_run(_, _)
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
cmd_rec(test).

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

bundleitem_do(X, Bundle, Cmd) :- ( Cmd = install ; Cmd = uninstall ), !,
	% consider X only if installation kind is global
	( ~instype = global ->
	    bundleitem_do_(X, Bundle, Cmd)
	; true
	).
bundleitem_do(X, Bundle, Cmd) :-
	bundleitem_do_(X, Bundle, Cmd).

bundleitem_do_(X, Bundle, Cmd) :- is_list(X), !,
	( cmd_revlist(Cmd) ->
	    bundleitem_revlist_do(X, Bundle, Cmd)
	; bundleitem_list_do(X, Bundle, Cmd)
	).
bundleitem_do_(item_group(Name, X), Bundle, Cmd) :- !,
	item_cmd_message(Cmd, CmdMsg0),
	( ( Cmd = register ; Cmd = unregister ) -> true % TODO: hack
	; cmd_message(Bundle, "(~s) ~s", [Name, CmdMsg0])
	),
	bundleitem_do_(X, Bundle, Cmd).
%
bundleitem_do_(switch_to_bundle(Bundle, X), _Bundle, Cmd) :- !,
	% TODO: hack for building 'ide' from 'core' (for emacs-mode); move hooks to ide and remove this
	bundleitem_do_(X, Bundle, Cmd).
bundleitem_do_(files_from(SrcDir, Path, _Props), Bundle, install) :- !,
	% Copy contents from SrcDir into Path
	storedir_install(dir(Path)),
	storedir_install(dir_rec(~bundle_path(Bundle, SrcDir), Path)).
bundleitem_do_(files_from(_SrcDir, Path, Props), _Bundle, uninstall) :- !,
	( ~instype = global -> true ; throw(uninstall_requires_global) ),
	( member(del_rec, Props) ->
	    % on uninstall, remove contents recursively
	    % TODO: remove also the directory?
	    storedir_uninstall(dir_rec(Path))
	; member(do_not_del, Props) ->
	    % do not delete on uninstall
	    true
	; storedir_uninstall(dir(Path))
	).
bundleitem_do_(files_from(_SrcDir, _Path, _Props), _Bundle, register) :- !.
bundleitem_do_(files_from(_SrcDir, _Path, _Props), _Bundle, clean_norec) :- !.
bundleitem_do_(lib(DirName), Bundle, build_nodocs) :- !,
	build_libs(Bundle, ~bundle_path(Bundle, DirName)).
bundleitem_do_(lib(DirName), Bundle, install) :- !, % (only instype=global)
	% Install the module collection under DirName (along compiled files)
	cmd_message(Bundle, "installing '~w' libraries", [DirName]),
	From = ~bundle_path(Bundle, DirName),
	To = ~inst_bundle_path(Bundle, DirName),
	storedir_install(dir_rec(From, To)).
bundleitem_do_(lib(DirName), Bundle, uninstall) :- !, % (only instype=global)
	% Uninstall the previously installed module collection DirName
	To = ~inst_bundle_path(Bundle, DirName),
	storedir_uninstall(dir_rec(To)).
bundleitem_do_(lib(_), _Bundle, register) :- !.
bundleitem_do_(lib(_), _Bundle, clean_norec) :- !.
bundleitem_do_(lib_force_build(DirName), Bundle, build_nodocs) :- !, % TODO: hack for library/clpq, library/clpr (see core bundle)
	build_libs(Bundle, ~bundle_path(Bundle, DirName)).
bundleitem_do_(lib_force_build(_), _Bundle, install) :- !. % TODO: assume installed with lib()
bundleitem_do_(lib_force_build(_), _Bundle, uninstall) :- !. % TODO: assume installed with lib()
bundleitem_do_(lib_force_build(_), _Bundle, register) :- !.
bundleitem_do_(lib_force_build(_), _Bundle, clean_norec) :- !.
bundleitem_do_(src(_DirName), _Bundle, build_nodocs) :- !. % (only for install)
bundleitem_do_(src(DirName), Bundle, install) :- !, % (only instype=global)
	% Install the module collection under DirName (just source, e.g., for examples)
	cmd_message(Bundle, "installing '~w' source", [DirName]),
	storedir_install(src_dir_rec(~bundle_path(Bundle, DirName), ~inst_bundle_path(Bundle, DirName))).
bundleitem_do_(src(DirName), Bundle, uninstall) :- !, % (only instype=global)
	% Uninstall the previously installed source-only module collection DirName
	storedir_uninstall(src_dir_rec(~inst_bundle_path(Bundle, DirName))).
bundleitem_do_(src(_DirName), _Bundle, register) :- !.
bundleitem_do_(src(_DirName), _Bundle, clean_norec) :- !.
bundleitem_do_(cmds_list(Path, List), Bundle, build_nodocs) :- !,
	build_cmds_list(Bundle, ~bundle_path(Bundle, Path), List).
bundleitem_do_(cmds_list(_, List), Bundle, install) :- !,
	storedir_install(cmds_list_(Bundle, List)).
bundleitem_do_(cmds_list(_, List), Bundle, uninstall) :- !,
 	storedir_uninstall(cmds_list_(Bundle, List)).
bundleitem_do_(cmds_list(_, _), _Bundle, register) :- !.
bundleitem_do_(cmds_list(_, _), _Bundle, clean_norec) :- !.
bundleitem_do_(lib_file_list(_Path, _List), _Bundle, register) :- !.
bundleitem_do_(lib_file_list(_Path, _List), _Bundle, clean_norec) :- !.
bundleitem_do_(lib_file_list(Path, List), Bundle, Cmd) :- !,
	lib_file_list_do(List, Bundle, ~bundle_path(Bundle, Path), Cmd).
bundleitem_do_(bin_copy_and_link(K, File, Props), Bundle, install) :- !,
	storedir_install(copy_and_link(K, Bundle, File)),
	( member(link_as(Link), Props) ->
	    storedir_install(link_as(K, Bundle, File, Link))
	; true
	).
bundleitem_do_(bin_copy_and_link(K, File, Props), Bundle, uninstall) :- !,
	( member(link_as(Link), Props) ->
	    storedir_uninstall(link(K, Link))
	; true
	),
	storedir_uninstall(copy_and_link(K, Bundle, File)).
bundleitem_do_(bin_copy_and_link(_K, _File, _Props), _Bundle, register) :- !.
bundleitem_do_(bin_copy_and_link(_K, _File, _Props), _Bundle, clean_norec) :- !.
% Engine
bundleitem_do_(eng(EngMainSpec, EngOpts), Bundle, build_nodocs) :- !,
	Eng = eng_def(Bundle, EngMainSpec, EngOpts),
	eng_build(Eng),
	% Activate
 	eng_active_bld(Eng).
bundleitem_do_(eng(EngMainSpec, EngOpts), Bundle, clean_norec) :- !,
	eng_clean(eng_def(Bundle, EngMainSpec, EngOpts)).
bundleitem_do_(eng(EngMainSpec, EngOpts), Bundle, install) :- !,
	Eng = eng_def(Bundle, EngMainSpec, EngOpts),
	( ~instype = global -> true ; throw(install_eng_requires_global) ), % TODO: should not be needed
	storedir_install(eng_contents(Eng)),
	storedir_install(eng_active(Eng)).
bundleitem_do_(eng(EngMainSpec, EngOpts), Bundle, uninstall) :- !,
	Eng = eng_def(Bundle, EngMainSpec, EngOpts),
	( ~instype = global -> true ; throw(uninstall_eng_requires_global) ), % TODO: should not be needed
	storedir_uninstall(eng_active(Eng)),
	storedir_uninstall(eng_contents(Eng)).
bundleitem_do_(eng(_EngMainSpec, _EngOpts), _Bundle, register) :- !.
% Engine header stubs for executables
bundleitem_do_(eng_exec_header(eng(EngMainSpec, EngOpts)), Bundle, build_nodocs) :- !,
	build_eng_exec_header(eng_def(Bundle, EngMainSpec, EngOpts)).
bundleitem_do_(eng_exec_header(eng(EngMainSpec, EngOpts)), Bundle, clean_norec) :- !,
	clean_eng_exec_header(eng_def(Bundle, EngMainSpec, EngOpts)).
bundleitem_do_(eng_exec_header(eng(_EngMainSpec, _EngOpts)), _Bundle, install) :- !,
	% TODO: do nothing -- is it right?
	true.
bundleitem_do_(eng_exec_header(eng(_EngMainSpec, _EngOpts)), _Bundle, register) :- !.
%
bundleitem_do_(Part, Bundle, Cmd) :-
	atom(Part), % TODO: replace by item(_)?
	!,
	builder_hookcmd(Bundle, Part, Cmd).
bundleitem_do_(X, _Bundle, Cmd) :-
	throw(error(unknown(X, Cmd), bundleitem_do/3)).

defcmd(prebuild_nodocs).
defcmd(build_nodocs).
defcmd(register).
defcmd(unregister).
defcmd(install).
defcmd(uninstall).
defcmd(clean_norec).

% TODO: we would need dependencies here
% (uninstall is done in reverse order)
bundleitem_revlist_do(Xs, Bundle, Cmd) :-
	bundleitem_list_do(~reverse(Xs), Bundle, Cmd).

bundleitem_list_do([], _Bundle, _Cmd).
bundleitem_list_do([X|Xs], Bundle, Cmd) :- bundleitem_do_(X, Bundle, Cmd), bundleitem_list_do(Xs, Bundle, Cmd).

lib_file_list_do([], _Bundle, _Path, _Cmd).
lib_file_list_do([X|Xs], Bundle, Path, Cmd) :-
	lib_file_item_do(X, Bundle, Path, Cmd),
	lib_file_list_do(Xs, Bundle, Path, Cmd).

lib_file_item_do(File-Props, Bundle, Path, install) :- !,
	( ~instype = global -> true ; throw(install_requires_global) ), % TODO: should not be needed
	lib_file_props(File, Props, Path, _Props2),
	storedir_install(lib_file_copy_and_link(Bundle, Path, File)).
lib_file_item_do(File-Props, Bundle, Path, uninstall) :- !,
	( ~instype = global -> true ; throw(uninstall_requires_global) ), % TODO: should not be needed
	lib_file_props(File, Props, Path, _Props2),
	storedir_uninstall(lib_file_copy_and_link(Bundle, Path, File)).

lib_file_props(File, Props, Path, Props2) :-
	( member(copy_and_link, Props) ->
	    Props2 = []
	; throw(unknown_props_lib_file(File, Props, Path))
	).
	
:- use_module(library(lists), [reverse/2]).

% TODO: use in more code?
inst_bundle_path(Bundle, Rel) := R :-
	R0 = ~instciao_bundledir(Bundle),
	( Rel = '.' ->
	    R = R0
	; R = ~path_concat(R0, Rel)
	).

% ---------------------------------------------------------------------------
% create/delete directory where bundles are installed

do_install_storedir(Bundle) :- ~instype = global, !,
	storedir_install(dir(~instciao_storedir)),
	storedir_install(dir(~instciao_bundledir(Bundle))).
do_install_storedir(_Bundle).

do_uninstall_storedir(Bundle) :- ~instype = global, !,
	storedir_uninstall(dir_rec(~instciao_bundledir(Bundle))),
	% delete if empty
	storedir_uninstall(dir_if_empty(~instciao_storedir)).
do_uninstall_storedir(_Bundle).

do_install_bindir(_Bundle) :- ~instype = global, !,
	storedir_install(dir(~instciao_bindir)).
do_install_bindir(_Bundle).

do_uninstall_bindir(_Bundle) :- ~instype = global, !,
	% Keep ~instciao_bindir (e.g., it may be /usr/bin)
	true.
do_uninstall_bindir(_Bundle) :-
	% delete if empty
	storedir_uninstall(dir_if_empty(~instciao_bindir)).

% ---------------------------------------------------------------------------
% Bundle registry installation (after bundle scan!)
% TODO: rename those predicates?
% (this rewrites bundlereg in the global installation area, with modified paths)

:- use_module(ciaobld(bundle_scan),
	[create_bundlereg/2, remove_bundlereg/2,
	 ensure_global_bundle_reg_dir/0,
	 rootprefix_bundle_reg_dir/2]).

install_bundlereg(Bundle) :-
	( ~instype = global ->
	    BundleDir = ~bundle_path(Bundle, '.'),
	    ensure_global_bundle_reg_dir,
	    create_bundlereg(BundleDir, global),
	    install_bundle_flags(Bundle)
	; true % (done in bundle scan)
	).

uninstall_bundlereg(Bundle) :-
	( ~instype = global ->
	    ensure_global_bundle_reg_dir,
	    uninstall_bundle_flags(Bundle),
	    remove_bundlereg(Bundle, global)
	; true % (nothing needed for local installations)
	).

% ---------------------------------------------------------------------------
% Preliminary support for installing configuration (bundle flags)
% (attached to bundle registry)
% TODO: make sure it works properly

:- use_module(library(bundle/bundle_flags),
	[bundle_flags_file/2, bundlecfg_filename/3]).

install_bundle_flags(Bundle) :-
	% (assumes ~instype = global)
	CfgFile = ~bundle_flags_file(Bundle),
	rootprefix_bundlecfg_file(global, Bundle, InsCfgFile),
	copy_file_or_dir(CfgFile, InsCfgFile).

uninstall_bundle_flags(Bundle) :-
	% (assumes ~instype = global)
	rootprefix_bundlecfg_file(global, Bundle, InsCfgFile),
	del_file_nofail(InsCfgFile).

% File is the registry file for the BundleName bundle
rootprefix_bundlecfg_file(InsType, BundleName, RegFile) :-
	rootprefix_bundle_reg_dir(InsType, BundleRegDir),
	bundlecfg_filename(BundleName, BundleRegDir, RegFile).

% ===========================================================================

:- doc(subsection, "Documentation of a Bundle").

:- use_module(library(bundle/doc_flags), [docformat/1, docformatdir/2]).

:- use_module(library(pathnames), [path_concat/3, path_split/3]).
:- use_module(library(system), [file_exists/1]).
:- use_module(library(system_extra), [mkpath/1]).
:- use_module(library(source_tree),
	[copy_file_or_dir/2, remove_file_or_dir/1]).

:- use_module(library(aggregates), [findall/3]).

:- use_module(ciaobld(ciaoc_aux), [invoke_lpdoc/1]).

% Creation of README files (from .lpdoc to ascii)
% Output is moved to the bundle root directory.
build_docs_readmes(Bundle) :- with_docs(yes), !,
	( % (failure-driven loop)
	  builder_pred(Bundle, readme_path(Readme)),
	    build_docs_readme(Bundle, Readme),
	    fail
	; true
	).
build_docs_readmes(_Bundle).

build_docs_readme(Bundle, Readme) :-
	( Readme = as(SrcPath, OutName) ->
	    true
	; SrcPath = Readme,
	  OutName = Name
	),
	path_split(SrcPath, _, Name),
	BundleDir = ~bundle_path(Bundle, '.'),
	path_concat(BundleDir, SrcPath, SrcPath1),
	atom_concat(SrcPath1, '.lpdoc', SrcPath2),
	DocDir = ~bundle_path(Bundle, builddir, 'doc'),
	invoke_lpdoc(['--autogen_warning=yes',
	              '--allow_markdown=no',
	              '--syntax_highlight=no',
	              ~atom_concat('--output_dir=', DocDir),
		      '-t', 'ascii',
	              SrcPath2]),
	Ascii = ~atom_concat(Name, '.ascii'),
	DocSrc = ~path_concat(DocDir, Ascii),
	copy_file_or_dir(DocSrc, OutName).

:- export(get_bundle_readme/2).
% TODO: duplicated in lpdoc_aux
% Output for bundle README files
get_bundle_readme(Bundle, R) :-
	builder_pred(Bundle, readme_path(Readme)),
	( Readme = as(_, Final) -> true
	; path_split(Readme, _, Final)
	),
	R = ~bundle_path(Bundle, Final).

:- use_module(library(bundle/bundle_info), [bundle_version_patch/2]).

:- export(bundle_manual_base/2).
% Base name for manuals of Bundle
bundle_manual_base(Bundle) := R :-
	builder_pred(Bundle, manual_dir(Manual)),
	( Manual = as(_Dir, Base) -> true
	; Dir = Manual,
	  path_split(Dir, _, Base)
	),
	( V = ~bundle_version_patch(Bundle) ->
	    R = ~atom_concat([Base, '-', V])
	; R = Base
	).

% Creates the manuals
build_docs_manuals(Bundle) :- with_docs(yes), !,
	( % (failure-driven loop)
	  builder_pred(Bundle, manual_dir(Manual)),
	    ( Manual = as(SrcDir, _) ->
	        true
	    ; SrcDir = Manual
	    ),
	    build_doc(Bundle, SrcDir),
	    fail
	; true
	).
build_docs_manuals(_Bundle).

bundle_install_docs(Bundle) :- with_docs(yes), !,
	cmd_message(Bundle, "installing [docs]", []),
	( % (failure-driven loop)
	  ManualBase = ~bundle_manual_base(Bundle),
	  docformat(DocFormat),
	    install_doc(Bundle, ManualBase, DocFormat),
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
	    uninstall_doc(Bundle, ManualBase, DocFormat),
	    fail
	; true
	),
	cmd_message(Bundle, "uninstalled [docs]", []).
bundle_uninstall_docs(_Bundle).

% Build the manual `SrcDir` for `Bundle`
build_doc(Bundle, SrcDir) :-
	BundleDir = ~bundle_path(Bundle, '.'),
	path_concat(BundleDir, SrcDir, R0),
	path_concat(R0, 'SETTINGS.pl', Settings),
	( file_exists(Settings) ->
	    DocDir = ~bundle_path(Bundle, builddir, 'doc'),
	    invoke_lpdoc(['--doc_mainopts=versioned_output',
%	                  '--allow_markdown=no',
%	                  '--syntax_highlight=no',
	                  ~atom_concat('--output_dir=', DocDir),
	                  '-t', 'all',
			  Settings])
	; % Allow missing manuals (e.g., for NODISTRIBUTE content)
	  warning(['Manual at ', SrcDir, ' is missing. Skipping build'])
	).

% Install manual `ManualBase` (name and version) from given `Bundle`
% and format `DocFormat`. Do nothing if manual is not generated.
% NOTE: needed even if ~instype = local (see bundle_install_docs_format_hook/3)
install_doc(Bundle, ManualBase, DocFormat) :-
	docformatdir(DocFormat, TargetDir0),
	TargetDir = ~rootprefixed(TargetDir0),
	FileName = ~atom_concat([ManualBase, '.', DocFormat]),
	DocDir = ~bundle_path(Bundle, builddir, 'doc'),
	Source = ~path_concat(DocDir, FileName),
	Target = ~path_concat(TargetDir, FileName),
	( file_exists(Source) ->
	    % Copy if needed
	    ( Source == Target -> % (typically when ~instype = local)
	        true
	    ; mkpath(TargetDir),
	      copy_file_or_dir(Source, TargetDir)
	    ),
	    % Register doc (if needed)
	    bundle_install_docs_format_hook(DocFormat, Bundle, Target)
	; true % warning(['File ', Source, ' not generated yet. Skipping copy'])
	).

% Uninstall manual `ManualBase` (name and version) from given `Bundle`
% and format `DocFormat`. Do nothing if manual is not generated.
% NOTE: needed even if ~instype = local (see bundle_uninstall_docs_format_hook/3)
uninstall_doc(Bundle, ManualBase, DocFormat) :-
	docformatdir(DocFormat, TargetDir0),
	TargetDir = ~rootprefixed(TargetDir0),
	FileName = ~atom_concat([ManualBase, '.', DocFormat]),
	DocDir = ~bundle_path(Bundle, builddir, 'doc'),
	Source = ~path_concat(DocDir, FileName),
	Target = ~path_concat(TargetDir, FileName),
	( file_exists(Target) ->
	    % Unregister doc (if needed)
	    bundle_uninstall_docs_format_hook(DocFormat, Bundle, Target),
	    % Remove if needed
	    ( Source == Target -> % (typically when ~instype = local)
	        true
	    ; remove_file_or_dir(Target)
	    )
	; true
	).

% These predicates install the 'info' files in info dir.

:- use_module(ciaobld(info_installer)).

bundle_install_docs_format_hook(info, Bundle, Target) :- !,
	DocDir = ~bundle_path(Bundle, builddir, 'doc'),
	dirfile_install_info(DocDir, Target).
bundle_install_docs_format_hook(_, _, _).

bundle_uninstall_docs_format_hook(info, Bundle, Target) :- !,
	DocDir = ~bundle_path(Bundle, builddir, 'doc'),
	dirfile_uninstall_info(DocDir, Target).
bundle_uninstall_docs_format_hook(_, _, _).

