:- module(_, [], [ciaobld(bundlehooks)]).

:- doc(title, "Bundle Hooks for Builder").

% ===========================================================================
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

:- doc(section, "Installation and registration type").

:- bundle_flag(instype, [
    comment("Installation type"),
    details(
      % .....................................................................
      "Select the type of installation:\n\n"||
      "    local  -- The system will be compiled in, and run from the \n"||
      "              sources (this is specially useful for developers).\n"||
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

% ---------------------------------------------------------------------------

:- doc(section, "Paths and permissions for global installation").
% (binaries, libraries, documentation, etc.)

:- use_module(library(pathnames), [path_concat/3]).
:- use_module(library(bundle/bundle_paths), [bundle_path/4]).

% Prefix for installation

:- bundle_flag(prefix, [
    comment("Prefix for installation directories"),
    details(
      % .....................................................................
      "Specify the directory to perform the installation."),
    rule_set_value(Value, (
      flag(instype(InsType)),
      InsType == 'local', Value = '')), % (none)
    rule_default(DefValue, (
      flag(instype(InsType)),
      get_prefix(InsType, DefValue))),
    %
    interactive
]).

get_prefix(global, '/usr/local').

concat_if_not_null('', _, '') :- !.
concat_if_not_null(Prefix, Rel, Path) :- path_concat(Prefix, Rel, Path).

% Base for CIAOROOT (installation goes another directory with the
% system version will be stored)

:- bundle_flag(install_ciaoroot_base, [
    comment("Base for system installation"),
    details(
      % .....................................................................
      "Base directory for installing (multiple) Ciao versions"),
    rule_set_value(Value, (
      flag(prefix(Prefix)),
      concat_if_not_null(Prefix, 'ciao', Value)))
]).

% Standard Unix-like default directories (for 'activation')

:- bundle_flag(bindir, [
    comment("Installation directory for executables"),
    rule_set_value(Value, (
      flag(prefix(Prefix)),
      concat_if_not_null(Prefix, 'bin', Value)))
]).

:- bundle_flag(mandir, [
    comment("Installation directory for 'man' pages"),
    rule_set_value(Value, (
      flag(prefix(Prefix)),
      concat_if_not_null(Prefix, 'share/man', Value)))
]).

:- bundle_flag(infodir, [
    comment("Installation directory for 'info' files"),
    rule_default(''),
    rule_set_value(Value, (
      flag(prefix(Prefix)),
      concat_if_not_null(Prefix, 'share/info', Value)))
]).

% Permissions and groups

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
