:- module(bundle_configure, [], [assertions, basicmodes, fsyntax, hiord]).

:- doc(title, "Bundle Configuration").
:- doc(author, "Ciao Development Team").

:- doc(module, "This module computes the values of bundle
   configuration flags (@lib{bundle_flags}) based on bundle parameters
   (@lib{bundle_params}) and the configuration rules
   (@tt{Manifest/*.config.pl} files).

@begin{alert}
DOCUMENT SYNTAX AND SEMANTICS OF CONFIGURATION RULES
@end{alert}

   The configuration process may invoke external configuration tools
   if required.").

:- use_module(library(lists)).
:- use_module(library(dynamic)).
:- use_module(library(process), [process_call/3]).
:- use_module(library(messages)).

% ---------------------------------------------------------------------------

% Configuration parameters
:- use_module(library(bundle/bundle_params), [bundle_param_value/2]).

% Configuration flags (persistent)
:- use_module(library(bundle/bundle_flags), [
	clean_bundle_flags/0,
	current_bundle_flag/2,
	set_bundle_flag/2,
	save_bundle_flags/0]).

% ---------------------------------------------------------------------------

:- include(ciaobld(bundleconfig/bundleconfig_defs)).

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
param_body(Bundle, Name,
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
	    NDef0 = [rule_default(Default)
		     % interactive("Set the given Prolog Flag.", [extended])
		     |NDef1],
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

:- use_module(library(bundle/paths_extra), [fsR/2]).
:- use_module(ciaobld(interactive_aux)).

% Be careful with get_vers and get_patch: when uninstalling, the patch
% version may differ with the version that we try to uninstall.

% ----------------------------------------------------------------------------

get_config_level(Flag, ConfigLevel) :-
	bundle_param_value(ciao:interactive_config, 'true'),
	LevelFlag = ciao:interactive_level,
	( Flag = LevelFlag ->
	    NConfigLevel = '3' % (for self-configure this option)
	; current_bundle_flag(LevelFlag, NConfigLevel)
	),
	!,
	configlevel(NConfigLevel, ConfigLevel).
get_config_level(_Flag, default).

% TODO: document, this seems to be the level for *interactive* configuration
configlevel('1', default). % TODO: This is not an interactive configuration!!!
configlevel('2', minimum).
configlevel('3', extended).

% ---------------------------------------------------------------------------

% The full configuration is stored in ~fsR(builddir(BldId))/bundlereg,
% in different formats for different tools:
%
%   - ciao.bundlecfg: internal format (saved, no sysdep)
%   - ciao.bundlecfg_sh: version in sh format for config-sysdep.sh
%     and ciaoc_aux:eng_config_sysdep/2
%
% See ciaoc_aux:eng_config_sysdep/2 for the sysdep configuration
% output for engine and C code compilation.

:- use_module(ciaobld(config_common), [default_eng/1]).
:- use_module(ciaobld(ciaoc_aux), [bundle_flags_sh_file/1]).

:- export(config_noscan/0).
:- pred config_noscan # "Configure the system. If
   @tt{ciao:interactive_config} flag is specified, the process is
   interactive. Otherwise, values for configuration are detected
   automatically.".

config_noscan :-
	% First part: materialize configuration values (based on user
	% preferences)
	check_bundle_params, % (for user prefs)
	eval_config_rules, % (can be interactive)
	save_bundle_flags, % (save 'ciao.bundlecfg')
	% (for future calls to eng_config_sysdep/2)
	export_bundle_flags_as_sh(~bundle_flags_sh_file).

% ---------------------------------------------------------------------------
% Auxiliary code for eng_config_sysdep/2

:- use_module(library(file_utils), [string_to_file/2]).
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
	current_bundle_flag(Bundle:Name0, Value),
	  atom_codes(Bundle, BundleS),
	  toupper(Name0, Name),
	  atom_codes(Name, NameS),
	  atom_codes(Value, ValueS),
	  list_concat([BundleS, "__", NameS, "=\"", ValueS, "\"\n"], Line).

:- use_module(library(hiordlib), [map/3]).

toupper(Name, Upper) :-
	atom_codes(Name, NameC),
	map(NameC, touppercode, UpperC),
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

check_bundle_params :-
	( bundle_param_value(Bundle:Name, _),
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
% Set a configuration flag (dangerous!)

% TODO: A better alternative is to load the original input, change the
%   value and run again the configuration rules. This would catch
%   inconsistencies. On the other hand, --set-flag may be useful for a
%   '--force' command at least for debugging.

:- export(config_set_flag/1).
config_set_flag(_Bundle) :-
	% (flags loaded)
	( bundle_param_value(ciao:set_flag_flag, Flag),
	  bundle_param_value(ciao:set_flag_value, Value) ->
	    true
	; throw(bug_in_config_set_flag)
	),
	set_bundle_flag(Flag, Value),
	%
	% TODO: save only the required _Bundle
	save_bundle_flags, % (save 'ciao.bundlecfg')
	% (for future calls to eng_config_sysdep/2)
	export_bundle_flags_as_sh(~bundle_flags_sh_file).

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
	    \+ flag_def(Flag, hidden),
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
config_describe_flag(Bundle) :-
	( bundle_param_value(ciao:describe_flag, Flag) ->
	    true
	; throw(bug_in_config_describe_flag)
	),
	Flag = Bundle:Name,
	( m_bundle_config_entry(Bundle, Name, _),
	  \+ flag_def(Flag, hidden) ->
	    describe_flag(Flag)
	; display_list(['Unknown flag \'', Bundle, ':', Name, '\'\n\n'])
	).

describe_flag(Flag) :-
	show_flag_and_domain(Flag),
	nl,
	flag_help_string(Flag, Help),
	display_string(Help), nl.

% Extract help text from the interactive message or comment.
flag_help_string(Flag, Help) :-
	( flag_def(Flag, interactive(_ConfigLevels, Help0)) ->
	    Help = Help0
	; flag_def(Flag, comment(Comment)) ->
	    Help = Comment
	; Help = "(No description available)"
	).

% ===========================================================================

% TODO: Define more precisely what is the semantics of configuration
%   parameters TODO: allow structures in own_make targets (fix spawn)
% TODO: access config values in condcomp conditions (or export them to
%   condcomp declarations)

% Determine the values for the configuration entries (automatically or
% via user interaction, depending on bundle flag
% @tt{ciao:interactive_config})
eval_config_rules :-
	reset_config_fixpo, % (just in case)
	catch(eval_config_rules_, E0, E=yes(E0)),
	reset_config_fixpo,
	( nonvar(E), E = yes(E0) -> throw(E0) ; true ).

eval_config_rules_ :-
	local_copy_bundle_flags,
	clean_bundle_flags,
	eval_config_level,
	( % (failure-driven loop)
	  m_bundle_config_entry(Bundle, Name, _),
	    Flag = Bundle:Name,
	    eval_config_rule(Flag, []),
	    fail
	; true
	).

% Evaluate rule for configuration level
eval_config_level :-
	eval_config_rule(ciao:interactive_level, []).

% prev_bundle_flag(Name, Bundle, Value).
:- data prev_bundle_flag/3.

% Save current bundle flags in prev_bundle_flag/3 data
local_copy_bundle_flags :-
	( % (failure-driven loop)
	  current_bundle_flag(Flag, Value),
	    Flag = Bundle:Name,
	    assertz_fact(prev_bundle_flag(Name, Bundle, Value)),
	    fail
	; true
	).

reset_config_fixpo :-
	retractall_fact(prev_bundle_flag(_, _, _)).

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
	param_body(Bundle, Name,
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
	( bundle_param_value(Flag, Value) ->
	    % TODO: Add option in bundle_flag for this?
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
	  ( \+ flag_def(Flag, noprevious),
	    prev_bundle_flag(Name, Bundle, PrevValue) ->
	      ExplanationPrev = previous % (previous value)
	  ; ExplanationPrev = none,
	    PrevValue = DefaultValue
	  ),
	  % TODO: split in a different set of rules?
	  % TODO: explain only if not customizing?
	  % Explain default value
	  ( flag_def(Flag, default_comment(Message)) ->
	      display_default(Message, DefaultValue)
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
	check_bundle_param_domain(Flag, Value, ValidValues),
	% Set value
	%display(config_value(Bundle, Name, Value, Explanation)), nl,
	set_bundle_flag(Flag, Value),
	% Show selected value
	( flag_def(Flag, hidden),
	  \+ interactive_flag(Flag) ->
	    % Do not show 'hidden' if we are not interactive
	    true
	; show_bundle_flag(Flag, Value)
	).

% The parameter can be configured interactively
interactive_flag(Flag) :-
	( flag_def(Flag, interactive(ConfigLevels))
	; flag_def(Flag, interactive(ConfigLevels, _))
	),
	get_config_level(Flag, ConfigLevel),
	member(ConfigLevel, ConfigLevels),
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

% TODO: do proper type checking, do not halt/1
check_bundle_param_domain(Flag, Value, ValidValues) :-
	( var(ValidValues) ->
	    true
	; ground(Value), member(Value, ValidValues) ->
	    true
	; display_list(
              ['Error: invalid value \'', Value, '\' for ',
	       Flag, '. Valid values are: ', ValidValues]),
	  halt(1)
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
	    ShowName = Comment
	; Flag = Bundle:Name,
	  atom_codes(Bundle, BundleS),
	  atom_codes(Name, NameS),
	  list_concat([BundleS, ":", NameS], ShowName)
	).

display_default(ShowName, Value) :-
	display('[ '),
	display_string(ShowName),
	display(': '),
	display(Value),
	display(' ]\n').

display_option(ShowName, Value) :-
	length(ShowName, N),
	N2 is 30 - N,
	display('   '),
	display_string(ShowName),
	display(':'),
	( N2 < 1 -> true
	; N2 = 1 -> display(' ')
	; N3 is N2 - 1,
	  display(' '), repeated_display('.', N3)
	),
	display_list([' ', Value, '\n']).

repeated_display(_,    0) :- !.
repeated_display(Term, Times) :-
	Times > 0,
	Times2 is Times - 1,
	display(Term),
	repeated_display(Term, Times2).

% ============================================================================

% TODO: Does it belong to this module?

:- use_module(library(bundle/bundle_flags), [get_bundle_flag/2]).
:- use_module(ciaobld(third_party_install), [third_party_path/2]).

:- use_module(library(pathnames), [path_concat/3]).
:- use_module(library(system), [file_exists/1, find_executable/2]).

% (Support for GNU pkg-config based libraries)

:- export(foreign_config_var/3).
% The configuration for foreign library @var{Foreign} has value
% @var{Value} in variable @var{Var}.
foreign_config_var(Foreign, Var, Value) :-
	foreign_config_tool_path(_, Foreign, CfgToolPath),
	process_call(CfgToolPath, [~atom_concat('--', Var)],
	       [stdout(line(Value)), status(0)]).

% TODO: cache path
foreign_config_tool_path(Bundle, Foreign, CfgToolPath) :-
	m_bundle_foreign_config_tool(Bundle, Foreign, CfgTool),
	( % TODO: hack! make it nicer by unifying names of third_party,
	  % foreign libraries, and AUTO_INSTALL_* params (make it a functor)
	  atom_concat('auto_install_', Foreign, AutoInstallOpt),
	  % (do not throw exception if does not exists)
	  current_bundle_flag(Bundle:AutoInstallOpt, 'yes') ->
	    % Look in third-party bin
	    third_party_path(bindir, ThirdPartyBinDir),
	    path_concat(ThirdPartyBinDir, CfgTool, CfgToolPath),
	    file_exists(CfgToolPath)
	; find_executable(CfgTool, CfgToolPath)
	).

:- export(foreign_config_version/2).
foreign_config_version(Foreign, Version) :-
	foreign_config_var(Foreign, 'version', Str),
	foreign_config_parse_version(Str, Version).

% from "Major.Minor" string to [Major,Minor]
foreign_config_parse_version(Str, L) :-
	( append(StrH, "." || StrT, Str) ->
	    L = [H|T],
	    number_codes(H, StrH),
	    foreign_config_parse_version(StrT, T)
	; L = [H],
	  number_codes(H, Str)
	).

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


