% (included file)

:- doc(section, "ciaoc compiler").

% ---------------------------------------------------------------------------
% Configuration flags

% (also available as reconfigurable prolog_flags)
% TODO: declaration in modules should be extracted from this configuration
:- bundle_flag(compress_exec, [
    config_prolog_flag(compress_exec,[yes,no],no)
]).
:- bundle_flag(itf_format, [
    config_prolog_flag(itf_format,[f,r],f)
]).
:- bundle_flag(compress_lib, [
    comment("Enable compressed bytecode"),
    details(
      % .....................................................................
      "If you wish to compile the Ciao libraries with their bytecode\n"||
      "compressed then set the following variable to \"yes\". Libraries\n"||
      "generated this way will be smaller at the cost of a slightly slower\n"||
      "usage, both in their load as when used to create an executable."),
    config_prolog_flag(compress_lib,[yes,no],no),
    interactive([advanced])
]).
:- bundle_flag(runtime_checks, [
    comment("Enable (optional) runtime checks"),
    details(
      % .....................................................................
      "If you wish to compile the Ciao libraries with runtime checks enabled\n"||
      "then set the following variable to \"yes\". This of course reduces\n"||
      "performance."),
    config_prolog_flag(runtime_checks,[yes,no],no),
    interactive([advanced])
]).
:- bundle_flag(rtchecks_level, [
    config_prolog_flag(rtchecks_level,[inner,exports],inner)
]).
:- bundle_flag(rtchecks_trust, [
    config_prolog_flag(rtchecks_trust,[yes,no],yes)
]).
:- bundle_flag(rtchecks_entry, [
    config_prolog_flag(rtchecks_entry,[yes,no],yes)
]).
:- bundle_flag(rtchecks_exit, [
    config_prolog_flag(rtchecks_exit,[yes,no],yes)
]).
:- bundle_flag(rtchecks_test, [
    config_prolog_flag(rtchecks_test,[yes,no],no)
]).
:- bundle_flag(rtchecks_asrloc, [
    config_prolog_flag(rtchecks_asrloc,[yes,no],yes)
]).
:- bundle_flag(rtchecks_predloc, [
    config_prolog_flag(rtchecks_predloc,[yes,no],yes)
]).
:- bundle_flag(rtchecks_callloc, [
    config_prolog_flag(rtchecks_callloc,[no,literal,predicate],predicate)
]).
:- bundle_flag(rtchecks_namefmt, [
    config_prolog_flag(rtchecks_namefmt,[short,long],long)
]).

% ---------------------------------------------------------------------------
% The command

'$builder_hook'(ciaoc:cmd('ciaoc', [main='ciaoc/ciaoc', bootstrap_ciaoc, static])).
