:- module(bundle_configure, [], [assertions, basicmodes, fsyntax, hiord]).

:- doc(title, "Bundle Configuration").
:- doc(author, "The Ciao Development Team").

:- doc(module, "This module computes the values of bundle
   configuration flags (@lib{bundle_flags}) based on configuration
   parameters the configuration rules (@tt{Manifest/*.hooks.pl}
   files).

@begin{alert}
DOCUMENT SYNTAX AND SEMANTICS OF CONFIGURATION RULES
@end{alert}

   The configuration process may invoke external configuration tools
   if required.").

% TODO: Define more precisely what is the semantics of configuration
%   parameters
% TODO: export some config values as condcomp conditions

:- use_module(library(lists)).
:- use_module(library(dynamic)).
:- use_module(library(process), [process_call/3]).
:- use_module(library(messages)).
:- use_module(library(port_reify)).

:- use_module(ciaobld(builder_flags), [get_builder_flag/2]).

% ---------------------------------------------------------------------------

% Configuration flags (persistent)
:- use_module(library(bundle/bundle_flags), [
	bundle_flags_file/2,
	clean_bundle_flags/1,
	current_bundle_flag/2,
	get_bundle_flag/2,
	set_bundle_flag/2,
	save_bundle_flags/1]).

% ---------------------------------------------------------------------------
% Input for configuration

% input_flag(Name, Bundle, Value)
:- data input_flag/3.

:- pred set_config_input(Flags) # "Select the input values for configuration".
set_config_input(Flags) :-
	retractall_fact(input_flag(_, _, _)),
	set_config_input_(Flags).

set_config_input_([flag(Flag, Value)|Flags]) :- !,
	( get_input_flag(Flag, _) ->
	    true % TODO: warn if duplicated?
	; set_input_flag(Flag, Value)
	),
	set_config_input_(Flags).
set_config_input_([]).

get_input_flag(Bundle:Name, Value) :-
	current_fact(input_flag(Name, Bundle, Value)).

set_input_flag(Bundle:Name, Value) :-
	retractall_fact(input_flag(Name, Bundle, _)),
	asserta_fact(input_flag(Name, Bundle, Value)).

% ---------------------------------------------------------------------------

:- include(ciaobld(bundlehooks/bundlehooks_defs)).

:- use_module(library(lists), [append/3]).

flag_def(Bundle:Name, X) :-
	m_bundle_config_entry(Bundle, Name, Def),
	member(X, Def),
	!.

% TODO: Document bundle configuration parameters. Document
%   config_prolog_flag/3 special case.

% TODO: Split default value in a different kind of rules?
%
%  rule_set_value ---> there is a unique value
%  rule_default   ---> there are multiple values
rule_body(Bundle, Name,
	  NeededIfGoal,
	  SetValueGoal, Value,
	  DefaultValueGoal, DefaultValue) :-
	m_bundle_config_entry(Bundle, Name, Def0),
	!,
	% expand config entry for prolog_flags
	( member(config_prolog_flag(_FlagNameL, Values, Default), Def0) ->
	    ( list(Values) ->
	        NDef = [valid_values(Values)|NDef0]
	    ; NDef = NDef0
	    ),
	    NDef0 = [rule_default(Default)|NDef1],
	    NDef1 = [],
	    append(Def0, NDef, Def)
	; Def = Def0
	),
	%
	( member(needed_if(NeededIfGoal), Def) ->
	    true
	; NeededIfGoal = true
	),
	%
	( member(rule_set_value(Value), Def) ->
	    SetValueGoal = true % TODO: not used?
	; member(rule_set_value(Value, SetValueGoal), Def) ->
	    true
	; SetValueGoal = fail
	),
	%
	( member(rule_default(DefaultValue), Def) ->
	    DefaultValueGoal = true
	; member(rule_default(DefaultValue, DefaultValueGoal), Def) ->
	    true
	; DefaultValueGoal = fail
	).
	  
% ---------------------------------------------------------------------------

:- use_module(ciaobld(interactive_aux)).

% TODO: Be careful with get_vers and get_patch: when uninstalling, the patch
%   version may differ with the version that we try to uninstall.

% ---------------------------------------------------------------------------

:- use_module(library(version_strings), [version_compare/3]).
:- use_module(library(system_extra), [file_to_line/2]).
:- use_module(library(bundle/bundle_paths), [bundle_path/3]).
:- use_module(engine(internals), ['$bundle_prop'/2]).

:- export(check_builder_update/0).
:- pred check_builder_update # "Detect if the running builder needs an
   update.".

check_builder_update :-
	( running_builder_vers(RunVers),
	  builder_minvers(MinVers),
	  version_compare(<, RunVers, MinVers) ->
	    builder_need_update(RunVers, MinVers)
	; true
	).

% The version of the running builder (fail if no .bundlereg for the
% builder exists yet).
% TODO: Add to binary instead? (like lpdoc version)
running_builder_vers(Vers) :- '$bundle_prop'(builder, version(Vers)).

% The minimum compatible builder version (specified in a single file
% easier to read than Manifests).
builder_minvers(MinVers) :-
	F = ~bundle_path(builder, 'Manifest/MinBuilderVersion'),
	catch(file_to_line(F, Str), _, fail),
	atom_codes(MinVers, Str).

builder_need_update(RunVers, MinVers) :-
	throw(error_msg(
            "The running builder (~w) is not compatible with the current sources.~n"||
            "  Please upgrade your installation (builder>=~w is required).~n"||
	    "  Use 'realclean' (or 'emergency-clean' in case of problems)~n"||
            "  then rebuild the system (including configuration steps).", [RunVers, MinVers])).

% ---------------------------------------------------------------------------

:- use_module(library(system), [file_exists/1]).

:- export(bundle_has_config/1).
bundle_has_config(Bundle) :-
	FlagsFile = ~bundle_flags_file(Bundle),
	file_exists(FlagsFile).

% ---------------------------------------------------------------------------

% The full configuration of a Bundle is stored in bundlereg/ under the
% build directory, in different formats for different tools:
%
%   - Bundle.bundlecfg: internal format (saved, no sysdep)
%   - Bundle.bundlecfg_sh: version in sh format for config-sysdep.sh
%     and eng_maker:eng_config_sysdep/2
%
% (only for bundles listed in bundle_export_sh/1)
%
% See eng_maker:eng_config_sysdep/2 for the sysdep configuration
% output for engine and C code compilation.

:- use_module(library(bundle/bundle_flags),
	[restore_all_bundle_flags/0,
	 reset_bundle_flags/1]).

:- use_module(ciaobld(eng_maker), [bundle_flags_sh_file/1]).
% TODO: ensure_load_manifest/1: not unloaded! (do refcount or gc of modules)
:- use_module(ciaobld(manifest_compiler),
	[ensure_load_manifest/1, manifest_call/2]).

:- export(bundleset_configure/2).
:- pred bundleset_configure(BundleSet, Flags) # "Configure the bundles
   specified in @var{BundleSet} using their configuration rules and
   the input provided by @var{Flags} and/or user interaction (if
   @tt{interactive_config} flag is specified).".

bundleset_configure(BundleSet, Flags) :-
	% Load config rules
	( % (failure-driven loop)
	  in_bundleset(BundleSet, Bundle),
	    ensure_load_manifest(Bundle),
	    fail
	; true
	),
	% Set input flags
	set_config_input(Flags),
	% Materialize configuration values (based on user preferences)
	check_bundle_params(BundleSet), % (for user prefs)
	check_bundle_deps(BundleSet),
	eval_config_rules(BundleSet), % (can be interactive)
	% Save changes on configuration
	save_modified_flags(BundleSet).

save_modified_flags(BundleSet) :-
	save_modified_flags_sh(BundleSet),
	( % (failure-driven loop)
	  in_bundleset(BundleSet, Bundle),
	    ( needs_save(Bundle) ->
	        save_bundle_flags(Bundle) % (save .bundlecfg files)
	    ; true
	    ),
	    fail
	; true
	).

needs_save(Bundle) :- dirty_bundle(Bundle), !.
needs_save(Bundle) :- \+ bundle_has_config(Bundle). 

% ---------------------------------------------------------------------------
% Auxiliary code for eng_config_sysdep/2

% (for future calls to eng_config_sysdep/2)
save_modified_flags_sh(BundleSet) :-
	( in_bundleset(BundleSet, Bundle),
	  needs_save(Bundle), bundle_export_sh(Bundle) -> 
	    export_bundle_flags_as_sh(~bundle_flags_sh_file)
	; true
	).

% Bundles whose configuration flags are exported to sh
% TODO: only some core__ are really needed; customize
% TODO: make configurable?
bundle_export_sh(core).

:- use_module(library(stream_utils), [string_to_file/2]).
:- use_module(library(aggregates), [findall/3]).

export_bundle_flags_as_sh(FileName) :-
	Text = ~list_concat([
	    "# Warning: This file has been created automatically\n\n",
	    ~flags_to_sh_string]),
	string_to_file(Text, FileName).

flags_to_sh_string(String) :-
	findall(Line, flag_to_sh_assign(Line), L),
	list_concat(L, String).

% (nondet)
flag_to_sh_assign(Line) :-
	bundle_export_sh(Bundle),
	current_bundle_flag(Bundle:Name0, Value),
	  atom_codes(Bundle, BundleS),
	  toupper(Name0, Name),
	  atom_codes(Name, NameS),
	  atom_codes(Value, ValueS),
	  list_concat([BundleS, "__", NameS, "=\"", ValueS, "\"\n"], Line).

:- use_module(library(hiordlib), [maplist/3]).

toupper(Name, Upper) :-
	atom_codes(Name, NameC),
	maplist(touppercode, NameC, UpperC),
	atom_codes(Upper, UpperC).

touppercode(C, U) :-
	0'a =< C,
	C =< 0'z,
	!,
	U is C + 0'A - 0'a.
touppercode(C, C).

% ---------------------------------------------------------------------------
% Verify that all user provided options (from the command-line)
% corresponds to configuration settings (m_bundle_config_entry/3)

check_bundle_params(BundleSet) :-
	( in_bundleset(BundleSet, Bundle),
	  get_input_flag(Bundle:Name, _),
	    ( Bundle = boot, m_bundle_config_entry(core, Name, _ParamDef) ->
	        % (special 'boot' configuration flags -- see scan_bootstrap_opts.sh)
		true
	    ; m_bundle_config_entry(Bundle, Name, _ParamDef) ->
		true
	    ; show_message(warning, "Unknown configuration flag `~w:~w'", [Bundle, Name])
	    ),
	    fail
	; true
	).

% ---------------------------------------------------------------------------
% Bundles in a configuration bundle set

in_bundleset(set(Bundles), Bundle) :- member(Bundle, Bundles).

% ---------------------------------------------------------------------------
% Check bundle dependencies

% TODO: Mark errors, abort configuration
% TODO: Hang on cyclic dependencies

:- use_module(engine(internals), ['$bundle_id'/1]).
:- use_module(library(bundle/bundle_info), [bundle_version/2]).

check_bundle_deps(BundleSet) :-
	( in_bundleset(BundleSet, Bundle),
	    check_bundle_deps_(Bundle),
	    fail
	; true
	).

check_bundle_deps_(Bundle) :-
	% show_message(warning, "checking deps for `~w'", [Bundle]),
	( % (failure-driven loop)
          manifest_call(Bundle, dep(Dep, Props)),
	    check_bundle_deps__(Bundle, Dep, Props),
	    fail
	; true
	).

check_bundle_deps__(Bundle, Dep, Props) :-
	( '$bundle_id'(Dep) -> true
	; throw(error_msg("missing bundle `~w' (required by `~w')", [Dep, Bundle]))
	),
	( check_bundle_constraints(Props, Dep) ->
	    true
	; throw(error_msg("requirements for bundle `~w' (required by `~w') are not met: ~q", [Dep, Bundle, Props]))
	).

:- use_module(library(version_strings), [version_compare/3]).

% Check that constraints Cs on Bundle are met
check_bundle_constraints([], _Bundle).
check_bundle_constraints([C|Cs], Bundle) :-
	check_bundle_constraint(C, Bundle),
	check_bundle_constraints(Cs, Bundle).

check_bundle_constraint(C, Bundle) :-
	version_constraint(C, Ver2, Op), !,
	bundle_version(Bundle, Ver1), % fail if no version
	version_compare(Comp, Ver1, Ver2),
	( eval_op(Op, Comp) -> true
	; fail
	).
check_bundle_constraint(_C, _Bundle). % ignore others

version_constraint(version=V, V, (=)).
version_constraint(version>=V, V, (>=)).
version_constraint(version>V, V, (>)).
version_constraint(version=<V, V, (=<)).
version_constraint(version<V, V, (<)).
version_constraint(version\=V, V, (\=)).

eval_op((=), (=)).
eval_op((>=), (>)).
eval_op((>=), (=)).
eval_op((>), (>)).
eval_op((=<), (<)).
eval_op((=<), (=)).
eval_op((<), (<)).
eval_op((\=), (>)).
eval_op((\=), (<)).

% ---------------------------------------------------------------------------
% Set a configuration flag (dangerous!)

% TODO: A better alternative is to load the original input, change the
%   value and run again the configuration rules. This would catch
%   inconsistencies. On the other hand, --set-flag may be useful for a
%   '--force' command at least for debugging.

:- export(config_set_flag/2).
% Pre: flags loaded
config_set_flag(Flag, Value) :-
	Flag = Bundle:_,
	BundleSet = set([Bundle]),
	%
	init_config_fixpo(BundleSet, yes),
	once_port_reify(config_set_flag_(Flag, Value), Port),
	reset_config_fixpo(BundleSet),
	port_call(Port),
	%
	save_modified_flags(BundleSet).

% TODO: Detect after fixpoint, not before? (for nondet flags)
config_set_flag_(Flag, Value) :-
	Flag = Bundle:Name,
	( old_bundle_flag(Name, Bundle, OldValue),
	  OldValue == Value ->
	    true
	; % value changed, mark bundle as dirty
	  set_dirty_bundle(Bundle)
	),
	set_bundle_flag(Flag, Value).

set_dirty_bundle(Bundle) :-
	( dirty_bundle(Bundle) -> true
	; assertz_fact(dirty_bundle(Bundle))
	).

% ---------------------------------------------------------------------------
% Get value of a configuration flag

:- export(config_get_flag/2).
% Pre: flags loaded
config_get_flag(Flag) := Value :-
	Value = ~get_bundle_flag(Flag).

% ---------------------------------------------------------------------------
% Display the configuration options (for the command-line interface)

:- export(config_list_flags/1).
% TODO: use Bundle
config_list_flags(Bundle) :-
	display_list(['Available configuration flags for bundle `', Bundle, '\'\n\n']),
	config_list_custom_flags(Bundle),
	display_list(['\n',
		      'Use --describe-flag OPT for help about a particular flag.\n',
		      '\n']).

config_list_custom_flags(Bundle) :-
	( m_bundle_config_entry(Bundle, Name, _),
	    Flag = Bundle:Name,
	    show_flag_and_domain(Flag),
	    fail
	; true
	).

show_flag_and_domain(Flag) :-
	( flag_def(Flag, valid_values(ValidValues)) ->
	    true
	; ValidValues = 'VALUE'
	),
	Flag = Bundle:Name,
	display_list(['  ', '--', Bundle, ':', Name, '=', ValidValues, '\n']).

:- export(config_describe_flag/1).
config_describe_flag(Flag) :-
	Flag = Bundle:Name,
	( m_bundle_config_entry(Bundle, Name, _) ->
	    describe_flag(Flag)
	; display_list(['Unknown flag \'', Bundle, ':', Name, '\'\n\n'])
	).

describe_flag(Flag) :-
	show_flag_and_domain(Flag),
	nl,
	flag_help_string(Flag, Help),
	display_string(Help), nl.

% Extract help text from the detailed or brief help
% TODO: compose both?
% TODO: similar to 'pred foo/1 # <txt>' and 'doc(foo/1, <txt>)'
flag_help_string(Flag, Help) :-
	( flag_def(Flag, details(Text)) ->
	    Help = Text
	; flag_def(Flag, comment(Text)) ->
	    Help = Text
	; Help = "(No description available)"
	).

% ===========================================================================

% Determine the values for the configuration entries (automatically or
% via user interaction, depending on bundle flag
% @tt{interactive_config})
eval_config_rules(BundleSet) :-
	init_config_fixpo(BundleSet, no),
	once_port_reify(eval_config_rules_(BundleSet), Port),
	reset_config_fixpo(BundleSet),
	port_call(Port).

eval_config_rules_(BundleSet) :-
	eval_config_mode_flag,
	( % (failure-driven loop)
	  in_bundleset(BundleSet, Bundle),
	  Flag = Bundle:Name,
	  m_bundle_config_entry(Bundle, Name, _),
	    % display(eval_config_rule(Flag, [])), nl,
	    eval_config_rule(Flag, []),
	    fail
	; true
	).

% Evaluate rule for configuration mode flag
eval_config_mode_flag :-
	( get_builder_flag(interactive_config, true) ->
	    eval_config_rule(builder:configuration_mode, [])
	; true
	).

% old_bundle_flag(Name, Bundle, Value): old flag value (detect modified flags for saving)
:- data old_bundle_flag/3.
% prev_bundle_flag(Name, Bundle, Value): previous flag value (for fixpoint)
:- data prev_bundle_flag/3.
% dirty_bundle(Bundle): @var{Bundle} is dirty (flags changed)
:- data dirty_bundle/1.

init_config_fixpo(BundleSet, OneFlag) :-
	retractall_fact(dirty_bundle(_)),
	reset_config_fixpo(BundleSet), % (just in case)
	% Save old values
	restore_all_bundle_flags, % TODO: Necessary? bundle(bundle_flags) contains an initialization directive
	bundleset_bak_flags(BundleSet),
	% Clean flags if needed (for non-interactive)
	( OneFlag = no ->
	    ( get_builder_flag(interactive_config, true) ->
	        % use saved config values
	        true
	    ; bundleset_clean_flags(BundleSet)
	    )
	; % TODO: reset only one flag? allow interactive_config?
	  true
	),
	% Copy current values (for fixpoint) to prev, and clean
	( OneFlag = no ->
	    bundleset_copy_flags(BundleSet),
	    bundleset_clean_flags(BundleSet)
	; % TODO: otherwise all flags are erased
          true
	).

% Save current bundle flags in old_bundle_flag/3 data
bundleset_bak_flags(BundleSet) :-
	( % (failure-driven loop)
	  in_bundleset(BundleSet, Bundle),
	  Flag = Bundle:Name,
	  current_bundle_flag(Flag, Value),
	    assertz_fact(old_bundle_flag(Name, Bundle, Value)),
	    fail
	; true
	).

% Save current bundle flags in prev_bundle_flag/3 data
bundleset_copy_flags(BundleSet) :-
	( % (failure-driven loop)
	  in_bundleset(BundleSet, Bundle),
	  Flag = Bundle:Name,
	  current_bundle_flag(Flag, Value),
	    assertz_fact(prev_bundle_flag(Name, Bundle, Value)),
	    fail
	; true
	).

bundleset_clean_flags(BundleSet) :-
	( % (failure-driven loop)
	  in_bundleset(BundleSet, Bundle),
	    clean_bundle_flags(Bundle),
	    fail
	; true
	).

reset_config_fixpo(BundleSet) :-
	( % (failure-driven loop)
	  in_bundleset(BundleSet, Bundle),
	    retractall_fact(old_bundle_flag(_, Bundle, _)),
	    retractall_fact(prev_bundle_flag(_, Bundle, _)),
	    fail
	; true
	).

% TODO: fix 'rule_default' and 'rule_set_value':
%  - allow 'undefined'?
%  - rule_default(_) is used for two different purposes (default values and
%    possible values)
%  - rule_set_value(_) does not allow interactive (i.e., only one possible value)
%  - rule_default(_) allow interactive (i.e., many possible values)

% Evaluate config rule for config value Name at Bundle.
eval_config_rule(Flag, _Seen) :-
	current_bundle_flag(Flag, _),
	!,
	% Already computed
	true.
eval_config_rule(Flag, Seen) :-
	member(Flag, Seen),
	!,
	% Loop in rule evaluation (not allowed)
	% TODO: This should be an error
	throw(loop_in_bundle_config_rule(Flag, Seen)).
eval_config_rule(Flag, Seen) :-
	Flag = Bundle:Name,
	( m_bundle_config_entry(Bundle, Name, _) ->
	    true
	; throw(undefined_bundle_config_parameter(Bundle, Name))
	),
	% Get rule body
	rule_body(Bundle, Name,
	          NeededIfGoal,
		  SetValueGoal, Value,
		  DefaultValueGoal, DefaultValue),
	% Get config value domain
	( flag_def(Flag, valid_values(ValidValues)) ->
	    true
	; true
	),
	% Get configuration value:
	%
	% (1) from a bundle parameter
	% TODO: use as input instead?
	( get_input_flag(Flag, Value) ->
	    % TODO: allow it only for 'interactive'?
	    Explanation = parameter
	% (2) unique possible value, from SetValueGoal
	; eval_config_deps(NeededIfGoal, Flag, Seen, Bundle),
	  eval_config_deps(SetValueGoal, Flag, Seen, Bundle),
	  eval_config_goal(SetValueGoal, Bundle) ->
	    Explanation = propagated % (no other possible choice)
	% (3) choose one option (greedy or with user interaction)	
	; eval_config_deps(NeededIfGoal, Flag, Seen, Bundle),
	  ( eval_config_deps(DefaultValueGoal, Flag, Seen, Bundle),
	    eval_config_goal(DefaultValueGoal, Bundle) ->
	      ExplanationDefault = default % (default value)
	  ; ExplanationDefault = none,
	    show_message(warning, "No possible value for `~w', which is a required parameter.", [Flag]),
	    fail
	  ),
	  % TODO: Default sometimes means a possible value
	  ( % \+ flag_def(Flag, noprevious),
	    prev_bundle_flag(Name, Bundle, PrevValue) ->
	      ExplanationPrev = previous % (previous value)
	  ; ExplanationPrev = none,
	    PrevValue = DefaultValue
	  ),
	  % TODO: split in a different set of rules?
	  % TODO: explain only if not customizing?
	  % Explain default value
	  ( flag_def(Flag, default_comment(Message)) ->
	      display_default(Flag, Message, DefaultValue)
	  ; true
	  ),
	  % Explain a particular default value
	  ( nonvar(DefaultValue),
	    flag_def(Flag, default_value_comment(DefaultValue, MessageVal)) ->
	      warning($$(MessageVal))
	  ; true
	  ),
	  % Choose value
	  ( interactive_flag(Flag) ->
	      % ask for a value, interactively from the user
	      flag_help_string(Flag, Help),
	      ask_option_value(Help, Name, ValidValues, DefaultValue, PrevValue, Value)
	  ; % choose default value
	    Value = DefaultValue
	  ),
	  ( Value == DefaultValue ->
	      Explanation = using_default(ExplanationDefault)
	  ; Explanation = customized
	  )
	),
	!,
	% TODO: do proper type check and definition
	check_bundle_flag_domain(Flag, Value, ValidValues),
	% Set value
	%display(config_value(Bundle, Name, Value, Explanation)), nl,
	config_set_flag_(Flag, Value),
	% Show selected value
	show_bundle_flag(Flag, Value).

% The flag can be configured interactively
interactive_flag(_) :-
	% none if not interactive
	\+ get_builder_flag(interactive_config, 'true'),
	!,
	fail.
interactive_flag(builder:configuration_mode) :- % (always ask on interactive)
	!.
interactive_flag(Flag) :-
	( flag_def(Flag, interactive(ConfigModes))
	; flag_def(Flag, interactive), ConfigModes = ['basic', 'advanced']
	),
	current_bundle_flag(builder:configuration_mode, ConfigMode),
	member(ConfigMode, ConfigModes),
	!.

eval_config_goal(true, _) :- !.
eval_config_goal(fail, _) :- !, fail.
eval_config_goal(flag(_), _) :- !. % (ignored, evaluated in eval_config_deps)
eval_config_goal((A,B), Bundle) :- !,
	eval_config_goal(A, Bundle),
	eval_config_goal(B, Bundle).
eval_config_goal(Goal, Bundle) :- !,
	m_bundle_config_call(Bundle, Goal).

eval_config_deps(true, _, _, _) :- !.
eval_config_deps(fail, _, _, _) :- !, fail.
eval_config_deps(X, HeadFlag, Seen, Bundle) :- X = flag(_), !,
	eval_config_lit(X, HeadFlag, Seen, Bundle).
eval_config_deps((A,B), HeadFlag, Seen, Bundle) :- !,
	eval_config_deps(A, HeadFlag, Seen, Bundle),
	eval_config_deps(B, HeadFlag, Seen, Bundle).
eval_config_deps(_, _HeadFlag, _Seen, _Bundle) :- !.

eval_config_lit(flag(Flag0), HeadFlag, Seen, ThisBundle) :-
	decomp_flag(Flag0, ThisBundle, Flag, Value),
	eval_config_rule(Flag, [HeadFlag|Seen]),
	( current_bundle_flag(Flag, Value0) ->
	    true
	; fail
	),
	Value = Value0.

% Split flag from value in flag(_), bundle-qualifying if not qualified.
decomp_flag(BundleNameVal, ThisBundle, Bundle:Name, Value) :- !,
	( BundleNameVal = Bundle:NameVal0 ->
	    NameVal = NameVal0
	; NameVal = BundleNameVal,
	  Bundle = ThisBundle
	),
	NameVal =.. [Name, Value].

% TODO: do proper type checking
check_bundle_flag_domain(Flag, Value, ValidValues) :-
	( var(ValidValues) ->
	    true
	; ground(Value), member(Value, ValidValues) ->
	    true
	; throw(error_msg("invalid value \'~w\' for ~w. Valid values are: ~w", [Value, Flag, ValidValues]))
	).

show_bundle_flag(Flag, Value) :-
	% TODO: Do not show values that are not configurable by the user?
	% This shows the value for the configuration option
	flag_show_string(Flag, ShowName),
	display_option(ShowName, Value).

% ---------------------------------------------------------------------------
% Pretty print bundle flags

flag_show_string(Flag, ShowName) :-
	( flag_def(Flag, comment(Comment)) ->
	    true
	; Comment = ""
	),
	flag_show_string_(Flag, Comment, ShowName).

flag_show_string_(Flag, Comment, ShowName) :-
	Flag = Bundle:Name,
	atom_codes(Bundle, BundleS),
	atom_codes(Name, NameS),
	( Comment = "" ->
	    list_concat([BundleS, ":", NameS], ShowName)
	; list_concat([Comment/*, BundleS, ":", NameS*/], ShowName)
	).

% TODO: messages can still be confusing
display_default(Flag, Comment, Value) :-
	flag_show_string_(Flag, Comment, ShowName),
	display('   [ '),
	display_string(ShowName),
	display('? '),
	display(Value),
	display(' ]\n').

display_option(ShowName, Value) :-
	display('   '),
	display_string(ShowName),
	display('... '),
	display_list([Value, '\n']).

% ---------------------------------------------------------------------------
% Set of set_prolog_flags from bundle config values (non-default values)

% TODO: Does it belong to this module or to bundle_flags?
% TODO: Bundle is missing

:- use_module(library(aggregates), [findall/3]).

:- export(set_prolog_flags_from_bundle_flags/1).
set_prolog_flags_from_bundle_flags(TFlagCmds) :-
	findall(
	    set_prolog_flag(FlagName, CurrValue),
	    (
		config_flag_entry(FlagName, _Values, Default, CurrValue),
		(CurrValue \== Default)
	    ),
	    TFlagCmds).

% TODO: Store in saved configuration instead?
config_flag_entry(Flag, Values, Default, CurrValue) :-
	m_bundle_config_entry(Bundle, Name, Def0),
	member(config_prolog_flag(Flag, Values, Default), Def0),
	CurrValue = ~get_bundle_flag(Bundle:Name).


