% (included file)

:- doc(section, "Engine and C Compilation Options").

% ---------------------------------------------------------------------------
% (next flags also needed by config-sysdep.sh)

:- bundle_flag(custom_cc, [
    comment("Custom C compiler"),
    rule_default(''),
    %
    interactive([extended])
]).
:- bundle_flag(custom_ld, [
    comment("Custom C linker"),
    rule_default(''),
    %
    interactive([extended])
]).
:- bundle_flag(extra_cflags, [
    comment("Additional C compiler flags"),
    rule_default(''),
    %
    interactive([extended])
]).
:- bundle_flag(extra_ldflags, [
    comment("Specify additional C linker flags"),
    rule_default(''),
    %
    interactive([extended])
]).

:- bundle_flag(os, [
    comment("Target OS"),
    rule_default(Value, sysconf_os(Value)),
    interactive([extended])
]).
:- bundle_flag(arch, [
    comment("Target architecture"),
    rule_default(Value, (
      flag(core:m32(M32)),
      flag(core:m64(M64)),
      sysconf_arch(M32, M64, Value))),
    interactive([extended])
]).
:- bundle_flag(m32, [
    comment("Force 32-bit architecture"),
    valid_values(['yes', 'no']),
    %
    rule_default('no'),
    %
    interactive([extended])
]).
:- bundle_flag(m64, [
    comment("Force 64-bit architecture"),
    valid_values(['yes', 'no']),
    %
    rule_default('no'),
    %
    interactive([extended])
]).

sysconf_os(OS) :-
	get_sysconf(['--os'], OS).

% (See scan_bootstrap_opts.sh)

sysconf_arch(M32, M64, Arch) :-
	get_sysconf(['--arch'], Arch0),
	( M32 = yes -> arch32(Arch0, Arch)
	; M64 = yes -> arch64(Arch0, Arch)
	; Arch = Arch0
	).

arch32('Sparc64', 'Sparc64m32') :- !.
arch32('x86_64', 'x86_64m32') :- !.
arch32('ppc64', 'ppc64m32') :- !.
arch32(Arch, Arch). % assume 32-bit

arch64('Sparc64', 'Sparc64') :- !.
arch64('x86_64', 'x86_64') :- !.
arch64('ppc64', 'ppc64') :- !.
arch64(_, empty). % force error % TODO: emit error instead?

ciao_sysconf_sh := ~fsR(bundle_src(builder)/sh_src/'config-sysdep'/'ciao_sysconf').

get_sysconf(Args, Val) :-
	process_call(~ciao_sysconf_sh, Args,
	             [stderr(stdout), stdout(string(Val0)), status(_)]),
	atom_codes(Val, Val0).

% ---------------------------------------------------------------------------

% (also needed by config-sysdep.sh)
:- bundle_flag(use_threads, [
    comment("Enable threads in engine"),
    valid_values(['yes', 'no']),
    %
    rule_default('yes'),
    %
    interactive([extended],
      % .....................................................................
      "If you wish to compile an engine with threads capability\n"||
      "(concurrency), set the following variable to \"yes\".  Otherwise, set\n"||
      "it to \"no\".  If the architecture does not support threads (or\n"||
      "thread support has not yet been added to Ciao for this\n"||
      "architecture), this will be automatically disabled at compile time.\n"||
      "Concurrency support does not cause any appreciable runtime overhead\n"||
      "for non-concurrent programs, so it is safe to leave it as \"yes\".")
]).

% ---------------------------------------------------------------------------

% (also needed by config-sysdep.sh)
:- bundle_flag(and_parallel_execution, [
    comment("Enable and-parallel execution"),
    valid_values(['yes', 'visandor', 'no']),
    %
    rule_default('no'),
    %
    interactive([extended],
      % .....................................................................
      "Set the following variable to \"yes\" if you wish to compile an\n"||
      "engine with support for and-parallel execution of goals in\n"||
      "(Herbrand-)independent fashion or to \"visandor\" if you wish also\n"||
      "support for VisAndOr's events. Choose one of:\n\n"||
      "        yes             -- Support for and-parallel execution.\n"||
      "        visandor        -- Support for and-parallel execution and\n"||
      "                           VisAndOr's events.\n"||
      "        no              -- No support.")
]).

% ---------------------------------------------------------------------------

% (also needed by config-sysdep.sh)
:- bundle_flag(par_back, [
    comment("Enable parallel backtracking"),
    valid_values(['yes', 'no']),
    %
    rule_default('no'),
    %
    interactive([extended],
      % .....................................................................
      "Set the following variable to \"yes\" if you wish to compile an\n"||
      "engine with support for parallel backtracking execution of goals.\n"||
      "This feature is experimental and may not be available in all releases.")
]).

% ---------------------------------------------------------------------------

% NOTE: Support for tabled execution is both in the engine (thus in
%   'core' bundle) and in contributed libraryes ('contrib' bundle)

% TODO: should be: with-...?
% (also needed by config-sysdep.sh)
:- bundle_flag(tabled_execution, [
    comment("Enable tabled execution"),
    valid_values(['yes', 'no']),
    %
    rule_default('yes'),
    %
    interactive([extended],
      % .....................................................................
      "Set the following variable to \"yes\" if you wish to compile an engine\n"||
      "with support for tabled execution of goals.")
]).

% ---------------------------------------------------------------------------

% (also needed by config-sysdep.sh)
:- bundle_flag(optim_level, [
    comment("Optimization level"),
    valid_values(['optimized', 'normal']),
    %
    rule_default('optimized'),
    %
    interactive([extended],
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
      "optimization options first.")
]).

% ---------------------------------------------------------------------------

% TODO: I'd say... mostly obsolete
:- bundle_flag(cross_compiler_host, [
    comment("Host for cross compilation"),
    rule_default('none'),
    %
    interactive([extended],
      % .....................................................................
      "If you will cross-compile the engine later, please enter the user\n"||
      "name and address of the target machine to extract run-time\n"||
      "characteristics from -- e.g., \"root@my.other.host.com\".  If you\n"||
      "are not going to crosscompile, leave the default value.\n"||
      "Cross-compiling is at the moment done with \"make build crossengine\"\n"||
      "once the regular compilation is completed.")
]).

% ---------------------------------------------------------------------------

:- bundle_flag(debug_level, [
    comment("Engine debug level"),
    valid_values(['nodebug', 'debug', 'profile', 'profile-debug',
    	'paranoid-debug']),
    %
    rule_default('nodebug'),
    %
    interactive([extended],
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
      "                      plus paranoid C compilation warnings.")
]).

