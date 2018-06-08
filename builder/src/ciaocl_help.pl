:- module(_, [], [assertions, isomodes, regtypes, dcg, fsyntax]).

:- doc(title, "Ciao Command-line Help").
:- doc(author, "The Ciao Development Team").
:- doc(author, "Jose F. Morales").

:- doc(module, "Formatted help for the @apl{ciao} and
   @apl{ciao-boot.sh} (@apl{ciao-boot.bat}) command line tool").

% ---------------------------------------------------------------------------

% (exports show_help/2 and show_help_cmd/2)
:- include(library(cmdline/cmdline_help)).

% ---------------------------------------------------------------------------

:- use_module(library(system), [using_windows/0]).

top_cmd_name(Cmd, "<cmd> [<args>]") :- top_cmd_name_(Cmd).

top_cmd_name_('ciao') :- help_mode(_, normal), !.
top_cmd_name_('ciao-boot.bat') :- using_windows, !.
top_cmd_name_('ciao-boot.sh').

top_cmd_details(_) :- fail.

% ---------------------------------------------------------------------------

grp_def(help, "Help") :- advanced.
%
cmd_grp(help, help).
cmd_usage(help, "", [
    %1_______________________________________________
    "Show help on most commonly used commands"
]).
cmd_usage(help, "[<cmd>]", [
    %1_______________________________________________
    "Show help on the specified command"
]).
%
cmd_grp(help_all, help).
cmd_usage(help_all, "", [
    %1_______________________________________________
    "Show help on all available commands"
]).

grp_def(bootstrap, "Bootstrapping") :- advanced, help_mode(_, boot).
%
cmd_grp(boot_build, bootstrap).
cmd_usage(boot_build, "", [
    %1_______________________________________________
    "Build the bootstrap (if it does not exist)"
]).
%
cmd_grp(boot_rebuild, bootstrap).
cmd_usage(boot_rebuild, "", [
    %1_______________________________________________
    "Force rebuild of the bootstrap"
]).
%
cmd_grp(boot_clean, bootstrap).
cmd_usage(boot_clean, "", [
    %1_______________________________________________
    "Clean the bootstrap"
]).

grp_def(quickstart, "Quick start commands") :-
        help_mode(_, boot).
%
cmd_grp(local_install, quickstart).
cmd_usage(local_install, "[<opts>] [<targets>] [<flags>]", [
    %1_______________________________________________
    "Configure, build, and install locally (in your home directory)"
]).
%
cmd_grp(global_install, quickstart).
cmd_usage(global_install, "[<opts>] [<targets>] [<flags>]", [
    %1_______________________________________________
    "Configure, build, and install globally (system wide)"
]).
cmd_details(global_install, [
    %2........................________________________________________________
    "See 'configure' for accepted configuration flags."
]).
%
cmd_grp('--', quickstart).
%
cmd_grp(local_install_paranoid, quickstart).
% TODO: rewrite as 'compile and lint'?
cmd_usage(local_install_paranoid, "[<opts>] [<targets>] [<flags>]", [
    %1_______________________________________________
    "Like 'local-install' but enable static checking",
    "of some possible code defects (like unused",
    "predicates, modules, etc.)."
]) :- advanced.

grp_def(promotion, "Bootstrap promotion (for system developers)") :-
        advanced.
%
cmd_grp(boot_promote, promotion).
cmd_usage(boot_promote, "", [
    %1_______________________________________________
    "Promote the current compiler (bytecode) and ",
    "engine (auto-generated C files) for bootstrap"
]).
cmd_details(boot_promote, [
    %2........................________________________________________________
    "NOTE: This is a dangerous operation!",
    "      Use 'build core.engine' and 'build core.ciaoc' (or a full 'build')",
    "      to prepare the current compiler and engine C files."
]).

grp_def(management, "Bundle management").
%
cmd_grp(rescan_bundles, management).
% TODO: configure help is also in bundle_configure.pl
cmd_usage(rescan_bundles, "[<targets>]", [
    %1_______________________________________________
    "Rescan bundles (given by targets, which can be",
    "workspace paths, bundle paths, or registered",
    "bundles)."
]) :- advanced.
%
cmd_grp(list, management).
cmd_usage(list, "", [
    %1_______________________________________________
    "List all available bundles"
]).
%
cmd_grp(info, management).
cmd_usage(info, "[<bundle>]", [
    %1_______________________________________________
    "Info on specified bundle"
]).
%
cmd_grp(fetch, management).
cmd_usage(fetch, "[<bundle aliases>]", [
    %1_______________________________________________
    "Download the specified bundles"
]).
% TODO: not uninstalled!
cmd_grp(rm, management).
cmd_usage(rm, "[<bundle aliases>]", [ % TODO: Allow bundle alias in more commands
    %1_______________________________________________
    "Remove the specified downloaded bundles",
    "(use before get to force update)"
]).
%
cmd_grp(get, management).
cmd_usage(get, "[<bundle aliases>] [<flags>]", [
    %1_______________________________________________
    "Fetch, configure, build, and install"
]).

grp_def(configure, "Configuration").
%
cmd_grp(configure, configure).
% TODO: configure help is also in bundle_configure.pl
cmd_usage(configure, "[<opts>] [<targets>] [<flags>] ", [
    %1_______________________________________________
    "Configure using the specified flags",
    "(not needed when using default settings)"
]).
cmd_details(configure, Text) :-
	Text = [
          %2........................________________________________________________
          % "(discarding any previous selection)",
	  "The following arguments for configure are accepted:",
	  "",
	  "  --interactive           Interactive configuration",
	  "                          (existing previous configuration is consulted)",
	  "  --list-flags            List the bundle configuration flags",
	  "  --describe-flag <flag>  Describe the configuration flag",
	  % TODO: Use this one with care (it does not use rules)
	  "  --get-flag <flag>       Show value of the configuration flag",
	  "  --set-flag <flag>=<value> Force flag value (dangerous!)"|Text0],
        ( help_mode(_, boot) ->
            Text0 = [
              %2........................________________________________________________
              "",
              "These core flags are recognized and used for bootstrap:",
              "",
              "  --core:os=OS                Target OS",
              "  --core:arch=ARCH            Target architecture",
              "  --core:m32=[yes|no]         Force 32-bit architecture",
              "  --core:m64=[yes|no]         Force 64-bit architecture",
              "  --core:custom-cc=CC         Select a custom C compiler",
              "  --core:custom-ld=LD         Select a custom C linker (optional, uses",
              "                              custom C compiler if not provided)",
              "  --core:extra-cflags=FLAGS   Use additional flags for C compiler",
              "  --core:extra-ldflags=FLAGS  Use additional flags for C linker",
              "  --core:debug-level          Debug level for C code",
              "",
              "Use same options with --boot:_ qualifier to affect only the bootstrap."
            ]
        ; Text0 = []
        ).
%
cmd_grp('--', configure).
%
cmd_grp(configclean, configure).
% TODO: really use BUNDLE as a parameter
cmd_usage(configclean, "[<targets>]", [
    %1_______________________________________________
    "Clean the configuration"
]) :- advanced.

common_opts_details([
    %2........................________________________________________________
    "  --grade=Grade          Select grade (default: bin, docs)",
    "  --bin                  Alias for --grade=bin (binaries)",
    "  --docs                 Alias for --grade=docs (documentation)",
    "  -r                     Treat dependencies (same workspace)",
    "  -x                     Treat dependencies (all workspaces)"
]).

grp_def(build_grp, "Build").
%
cmd_grp(build, build_grp).
cmd_usage(build, "[<opts>] [<targets>]", [
    %1_______________________________________________
    "Build bundles"
]).
%
grp_details(build_grp, [
    %2........................________________________________________________
    "Build compiles modules (libraries), binaries, and documentation",
    "of the specified bundles. The process is incremental and recompiles",
    "sources affected by modifications. In some rare cases (like changes in",
    "configuration or untracked dependencies from foreign code and libraries)",
    "you may need to invoke 'clean' before 'build'.",
    "",
    "You can use 'info' to inspect dependencies and subtargets.",
    "",
    %2........................________________________________________________
    "Build accepts the options:",
    ""|
    ~common_opts_details
]).

grp_def(clean_grp, "Cleaning").
%
% NOTE: clean --docs is NOT equivalent to 'clean' for docs; it just
% remove the final targets, not the temporary files.
cmd_grp(clean, clean_grp).
cmd_usage(clean, "[<opts>] [<targets>]", [
    %1_______________________________________________
    "Clean intermediate files (keeps configuration)"
]).
%
cmd_grp(distclean, clean_grp).
cmd_usage(distclean, "[<targets>]", [
    %1_______________________________________________
    "Like 'clean' but also removes configuration"
]).
%
cmd_grp(realclean, clean_grp).
cmd_usage(realclean, "", [
    %1_______________________________________________
    "Like 'distclean' but also clean the bootstrap"
]) :- advanced, help_mode(_, boot).
%
cmd_grp('--', clean_grp).
%
cmd_grp(clean_tree, clean_grp).
cmd_usage(clean_tree, "<dir>", [
    %1_______________________________________________
    "Clean the specified directory tree"
]) :- advanced.
%
cmd_grp('--', clean_grp).
%
% (only for bootstrap)
cmd_grp(emergency_clean, clean_grp).
cmd_usage(emergency_clean, "", [
    %1_______________________________________________
    "Emergency clean (for unstable source trees)"
]) :- advanced, help_mode(_, boot).
cmd_details(emergency_clean, [
    %2........................________________________________________________
    "NOTE: Use this option when the bootstrap is unable to compile properly and",
    "none of the cleaning commands work."
]).
%
grp_details(clean_grp, [
    %2........................________________________________________________
    "Clean accepts the options:",
    ""|
    ~common_opts_details
]).

grp_def(install_grp, "Installation").
%
cmd_grp(install, install_grp).
cmd_usage(install, "[<opts>] [<targets>]", [
    %1_______________________________________________
    "Install libraries, binaries, and documentation"
]).
%
cmd_grp(uninstall, install_grp).
cmd_usage(uninstall, "[<opts>] [<targets>]", [
    %1_______________________________________________
    "Uninstall libraries, binaries, and documentation"
]).
%
grp_details(install_grp, [
    %2........................________________________________________________
    "Install and uninstall accepts the options:",
    "",
    "  --destdir=DESTDIR      Prepend DESTDIR to each (un)install target",
    "                         (for staged installations)",
    ""|
    ~common_opts_details
]).

grp_def(test_grp, "Test automation and benchmarking").
%
cmd_grp(test, test_grp).
% TODO: Split into unit tests and the more advanced testing/benchmarking driver
cmd_usage(test, "[<targets>]", [
    %1_______________________________________________
    "Run all tests (unit tests, integration, etc.)"
]).
%
cmd_grp(bench, test_grp).
cmd_usage(bench, "[<targets>]", [
    %1_______________________________________________
    "Execute all benchmarks"
]) :- advanced.

grp_def(packaging, "Packaging for distribution") :- advanced.
%
cmd_grp(gen_pbundle, packaging). % TODO: trick, does not exist
cmd_usage(gen_pbundle, "[--kind=Kind] [<targets>]", [
    %1_______________________________________________
    "Generates a distribution of the specified Kind",
    "(see gen_pbundle_hook/3)"
]).
%
cmd_grp(gen_commit_info, packaging).
cmd_usage(gen_commit_info, "[--git-repo-dir=Dir] [<targets>]", [
    %1_______________________________________________
    "[Not documented, internal]"
]).

% TODO: detailed help for those commands is treated in a different way
grp_def(tools, "Tools") :- \+ help_mode(_, boot).
%
cmd_grp(toplevel, tools).
cmd_usage(toplevel, "", [
    %1_______________________________________________
    "Executes a toplevel (ciaosh) [default]"
]).
%
cmd_grp(doc, tools).
cmd_usage(doc, "[<bundle>]", [
    %1_______________________________________________
    "Show the manual for the given bundle"
]).
%
cmd_grp(comp, tools).
cmd_usage(comp, "", [
    %1_______________________________________________
    "Executes the compiler (ciaoc)"
]).
%
% TODO: extend to run commands (cmds/1, cmds/2), looking up in workspaces
cmd_grp(run, tools).
cmd_usage(run, "", [
    %1_______________________________________________
    "Runs a Ciao script (ciao-shell)"
]).

