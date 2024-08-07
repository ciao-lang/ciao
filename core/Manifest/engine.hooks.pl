% (included file)

:- doc(section, "Engine").

% ===========================================================================
% Engine and C compilation options

% (next flags are needed by config-sysdep.sh)

:- bundle_flag(custom_cc, [
    comment("Custom C compiler"),
    rule_default(''),
    %
    interactive([advanced])
]).
:- bundle_flag(custom_ld, [
    comment("Custom C linker"),
    rule_default(''),
    %
    interactive([advanced])
]).
:- bundle_flag(extra_cflags, [
    comment("Additional C compiler flags"),
    rule_default(''),
    %
    interactive([advanced])
]).
:- bundle_flag(extra_ldflags, [
    comment("Specify additional C linker flags"),
    rule_default(''),
    %
    interactive([advanced])
]).

:- bundle_flag(os, [
    comment("Target OS"),
    rule_default(Value, sysconf_os(Value)),
    interactive([advanced])
]).
:- bundle_flag(arch, [
    comment("Target architecture"),
    rule_default(Value, (
      flag(core:m32(M32)),
      flag(core:m64(M64)),
      sysconf_arch(M32, M64, Value))),
    interactive([advanced])
]).
:- bundle_flag(m32, [
    comment("Force 32-bit architecture"),
    valid_values(['yes', 'no']),
    %
    rule_default('no'),
    %
    interactive([advanced])
]).
:- bundle_flag(m64, [
    comment("Force 64-bit architecture"),
    valid_values(['yes', 'no']),
    %
    rule_default('no'),
    %
    interactive([advanced])
]).

:- use_module(ciaobld(car_maker), [sysconf_os/1, sysconf_arch/3]).

% ---------------------------------------------------------------------------

% (also needed by config-sysdep.sh)
:- bundle_flag(use_threads, [
    comment("Enable threads in engine"),
    details(
      % .....................................................................
      "If you wish to compile an engine with threads capability\n"||
      "(concurrency), set the following variable to \"yes\".  Otherwise, set\n"||
      "it to \"no\".  If the architecture does not support threads (or\n"||
      "thread support has not yet been added to Ciao for this\n"||
      "architecture), this will be automatically disabled at compile time.\n"||
      "Concurrency support does not cause any appreciable runtime overhead\n"||
      "for non-concurrent programs, so it is safe to leave it as \"yes\"."),
    valid_values(['yes', 'no']),
    %
    rule_default('yes'),
    %
    interactive([advanced])
]).

% ---------------------------------------------------------------------------

% (also needed by config-sysdep.sh)
:- bundle_flag(and_parallel_execution, [
    comment("Enable and-parallel execution"),
    details(
      % .....................................................................
      "Set the following variable to \"yes\" if you wish to compile an\n"||
      "engine with support for and-parallel execution of goals in\n"||
      "(Herbrand-)independent fashion or to \"visandor\" if you wish also\n"||
      "support for VisAndOr's events. Choose one of:\n\n"||
      "        yes             -- Support for and-parallel execution.\n"||
      "        visandor        -- Support for and-parallel execution and\n"||
      "                           VisAndOr's events.\n"||
      "        no              -- No support."),
    valid_values(['yes', 'visandor', 'no']),
    %
    rule_default('no'),
    %
    interactive([advanced])
]).

% ---------------------------------------------------------------------------

% (also needed by config-sysdep.sh)
:- bundle_flag(par_back, [
    comment("Enable parallel backtracking"),
    details(
      % .....................................................................
      "Set the following variable to \"yes\" if you wish to compile an\n"||
      "engine with support for parallel backtracking execution of goals.\n"||
      "This feature is experimental and may not be available in all releases."),
    valid_values(['yes', 'no']),
    %
    rule_default('no'),
    %
    interactive([advanced])
]).

% ---------------------------------------------------------------------------

% NOTE: Support for tabled execution is both in the engine (thus in
%   'core' bundle) and in contributed libraryes ('contrib' bundle)

% TODO: should be: with-...?
% (also needed by config-sysdep.sh)
:- bundle_flag(tabled_execution, [
    comment("Enable tabled execution"),
    details(
      % .....................................................................
      "Set the following variable to \"yes\" if you wish to compile an engine\n"||
      "with support for tabled execution of goals."),
    valid_values(['yes', 'no']),
    %
    rule_default('yes'),
    %
    interactive([advanced])
]).

% ---------------------------------------------------------------------------

% (also needed by config-sysdep.sh)
:- bundle_flag(optim_level, [
    comment("Optimization level"),
    details(
      % .....................................................................
      "Optimization level used when compiling the bytecode emulator. Choose\n"||
      "one of:\n"||
      "\n"||
      "   optimized       -- Turn on optimization flags\n"||
      "   normal          -- Normal emulator (non-optimized code)\n"||
      "\n"||
      "For normal use, we recommend leaving it as \"optimized\".  But if you\n"||
      "suspect that your C compiler performs buggy optimizations (which\n"||
      "should not be the case), turn optimization off.  This may happen more\n"||
      "easily in concurrent applicacions: if you write any thread-based\n"||
      "program and unexpected results appear, try recompiling Ciao without\n"||
      "optimization options first."),
    valid_values(['optimized', 'normal']),
    %
    rule_default('optimized'),
    %
    interactive([advanced])
]).

% ---------------------------------------------------------------------------

:- bundle_flag(debug_level, [
    comment("Engine debug level"),
    details(
      % .....................................................................
      "Level of debugging built into the engine (for developers):\n"||
      "\n"||
      "   nodebug         -- Do not include debug information or messages\n"||
      "   debug           -- Emulator with C level debugging info available\n"||
      "                      plus extended C compilation warnings\n"||
      "   profile         -- Include profiling options for the emulator\n"||
      "   profile-debug   -- Include profiling and debug options for the\n"||
      "                      emulator\n"||
      "   paranoid-debug  -- Emulator with C level debugging info available\n"||
      "                      plus paranoid C compilation warnings."),
    valid_values(['nodebug', 'debug', 'profile', 'profile-debug', 'paranoid-debug']),
    %
    rule_default('nodebug'),
    %
    interactive([advanced])
]).

% ===========================================================================
% Build

'$builder_hook'(engine:eng('engine/ciaoengine', [])).

% NOTE: experimental (see options)
% DO NOT BUILD BY DEFAULT!
'$builder_hook'(static_engine:eng('engine/ciaoengine', [
  % TODO: Uses bootstrap ciaoc (see car_maker.pl) -- allow configuration here
  add_stat_libs, % link statically against C system libraries
  static_mods([library(random),
           library(sockets),
           library(sha1),
           library(concurrency)]), % link statically against foreign code
  % TODO: extract from static_mods
  static_cfgs([at_bundle(ciao_gsl, 'gsl')]) % see gsl.hooks.pl at 'ciao_gsl' bundle
])).
