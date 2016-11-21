:- module(_, [], [ciaobld(bundleconfig)]).

:- doc(title, "Configuration rules for Ciao").
:- doc(author, "Ciao Development Team").

:- use_module(library(pathnames), [path_concat/3]).
:- use_module(library(bundle/bundle_paths), [bundle_path/3, bundle_path/4]).

% ---------------------------------------------------------------------------

:- doc(section, "General build/installation options").

:- bundle_flag(configuration_mode, [
    comment("Configuration mode"),
    details(
      % .....................................................................
      "Entering the interactive configuration.\n"||
      "You will now be asked some questions related to the configuration.\n"||
      "Hit [Enter] to accept the default values shown in square brackets.\n"||
      "Press C-c to abort the interactive configuration.\n\n"||
      %
      "Please select the configuration mode:\n\n"||
      "    basic    --  Configure just a minimum set of options.\n"||
      "    advanced --  Configure an extended set of options."),
    valid_values(['basic', 'advanced']),
    %
    rule_default('basic'),
    %
    interactive
]).

:- bundle_flag(verbose_build, [
    comment("Verbose builder"),
    details(
      % .....................................................................
      "More verbose builder messages."),
    valid_values(['yes', 'no']),
    %
    rule_default('no'),
    %
    interactive([advanced])
]).

:- bundle_flag(with_docs, [
    comment("Generate documentation"),
    details(
      % .....................................................................
      "Generate documentation."),
    valid_values(['yes', 'no']),
    %
    rule_default('yes'),
    %
    interactive([advanced])
]).

:- bundle_flag(gen_asr, [
    comment("Generate .asr files during compilation"),
    valid_values([yes, no]),
    %
    rule_default(yes)
]).

% ---------------------------------------------------------------------------

:- doc(section, "Installation type and built-in directories").

:- bundle_flag(instype, [
    comment("Installation type"),
    details(
      % .....................................................................
      "Select the type of installation:\n\n"||
      "    local  -- The system will be compiled in, and run from the \n"||
      "              sources (this is specially useful for developers)."||
      "    global -- Install the system in a separate location from the\n"||
      "              sources and set up things to use the installed version.\n"||
      "              The system will not require the sources to run, and \n"||
      "              they can be erased after installation.\n"),
    valid_values(['local', 'global']),
    %
    rule_default('local'),
    %
    interactive
]).

% TODO: make use of default value (simplify scripts)
% TODO: rename 'all' and 'user'? (e.g. global and local too?)
% TODO: include here other access methods like menus, and desktop icons 

:- bundle_flag(registration_type, [
    comment("Registration type"),
    details(
      % .....................................................................
      "Registration type:\n\n"||
      "    all  --  Make the system available to all users. Typically you\n"||
      "             you will need to complete the installation as root.\n"||
      "    user --  Make the system available only for the current user\n"||
      "             (configure it in the user\'s home directory)."),
    valid_values(['all', 'user']),
    %
    rule_default(SysregType, (
      flag(instype(InsType)),
      def_registration_type(InsType, SysregType))),
    %
    interactive
]).

def_registration_type(global, all).
def_registration_type(local, user).

% (Not configurable setting, necessary for build_engine.sh)
:- bundle_flag(ciaosrc, [
    comment("Source directory for Ciao"),
    rule_set_value(Value, bundle_path(ciao, '.', Value))
]).

% (Not configurable setting, necessary for build_engine.sh)
:- bundle_flag(defaultlibdir, [
    comment("Default directory for Ciao libraries"),
    rule_set_value(Value, (
      flag(install_libdir(_)), % TODO: hidden dependency (due to bundle_path/3)
      flag(instype(InsType)),
      get_defaultlibdir(InsType, Value)))
]).

get_defaultlibdir(local) := ~bundle_path(core, '.').
get_defaultlibdir(global) := ~instciao_bundledir(core).

:- use_module(ciaobld(config_common), [instciao_bundledir/2]).

% ---------------------------------------------------------------------------

:- doc(section, "Paths and permissions for global installation").
% (binaries, libraries, documentation, etc.)

% TODO: make sure that those are never used in local install (e.g., make them optional?)

:- use_module(library(system), [get_home/1]).

:- bundle_flag(install_prefix, [
    comment("Install prefix"),
    details(
      % .....................................................................
      "Specify the directory to perform the installation."),
    rule_set_value(Value, (
      flag(instype(InsType)),
      InsType == 'local', build_dir(Value))), % TODO: use something else (not valid in 'local')
    rule_default(DefValue, (
      flag(instype(InsType)),
      get_prefix(InsType, DefValue))),
    %
    interactive
]).

:- use_module(library(bundle/bundle_info), [root_bundle/1]).
build_dir := ~bundle_path(~root_bundle, builddir, '.').

get_prefix(global, '/usr/local').

:- bundle_flag(install_bindir, [
    comment("Installation directory for executables"),
    rule_set_value(Value, (
      flag(install_prefix(Prefix)),
      path_concat(Prefix, 'bin', Value)))
]).

:- bundle_flag(install_libdir, [
    comment("Installation directory for libraries"),
    rule_set_value(Value, (
      flag(install_prefix(Prefix)),
      path_concat(Prefix, 'lib', Value)))
]).

:- bundle_flag(execmode, [
    comment("Permissions for installed execs/dirs"),
    rule_default('775'),
    %
    interactive([advanced])
]).

:- bundle_flag(datamode, [
    comment("Permissions for installed data files"),
    rule_default('664'),
    %
    interactive([advanced])
]).

% TODO: Ignored by many of the installation code (thus, not working)
:- bundle_flag(installgroup, [
    comment("Custom group for installed files"),
    details(
      % .....................................................................
      "Group for the installed files (empty means use default)"),
    rule_default(''),
    %
    interactive([advanced])
]).

% ---------------------------------------------------------------------------

:- doc(section, "Detect optional external commands").
% Some custom non-configurable settings.

:- use_module(library(system), [find_executable/2]).
:- use_module(library(lists), [append/3]).
:- use_module(library(messages), [error_message/1]).

:- discontiguous cmd_op/2.

% Locate Cmd or any of its variants (defined in
% @pred{cmd_op/2}).
get_cmd_op(Cmd, CmdPath) :-
	( cmd_op(Cmd, CmdName),
	  find_executable(CmdName, Path0) ->
	    CmdPath = Path0
	; error_message("Cannot find any version of '" ||
		        (~append(~atom_codes(Cmd), "' command in the path."))),
	  fail
	).

% Detect GNU make
:- bundle_flag(gmake_cmd, [
    comment("Path of GNU make command"),
    rule_set_value(Value, get_cmd_op(gmake, Value))
]).

cmd_op(gmake) := gmake|make.

% Detect GNU tar
:- bundle_flag(gtar_cmd, [
    comment("Path of GNU tar command"),
    rule_set_value(Value, get_cmd_op(gtar, Value))
]).

cmd_op(gtar) := gnutar|gtar|tar.

% Detect Apache Ant
:- bundle_flag(ant_cmd, [
    comment("Path of Apache Ant command"),
    rule_set_value(Value, get_cmd_op(ant, Value))
]).

cmd_op(ant) := ant.



