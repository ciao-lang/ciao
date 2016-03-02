:- module(_, [], [assertions, isomodes, regtypes, dcg]).

:- doc(title, "Ciao Command-line Help").
:- doc(author, "Ciao Development Team").
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
cmd_usage(local_install, "[<opts>]", [
    %1_______________________________________________
    "Configure, build, and install locally (in your home directory)"
]).
%
cmd_grp(global_install, quickstart).
cmd_usage(global_install, "[<opts>]", [
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
cmd_usage(local_install_paranoid, "[<opts>]", [
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
    "      Use 'build core/engine' and 'build core/ciaoc' (or a full 'build')",
    "      to prepare the current compiler and engine C files."
]).

grp_def(management, "Bundle management").
%
cmd_grp(rescan_bundles, management).
% TODO: configure help is also in bundle_configure.pl
cmd_usage(rescan_bundles, "[<path>]", [
    %1_______________________________________________
    "Rescan bundles at workspace"
]).
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
cmd_grp(get, management).
cmd_usage(get, "[<bundle alias>]", [
    %1_______________________________________________
    "Download and install the specified bundles"
]).
cmd_grp(rm, management).
cmd_usage(rm, "[<bundle alias>]", [ % TODO: Allow bundle alias in more commands
    %1_______________________________________________
    "Remove a downloaded bundle",
    "(use before get to force update)"
]).

grp_def(configure, "Configuration (before build)").
%
cmd_grp(configure, configure).
% TODO: add BUNDLE as a parameter
% TODO: configure help is also in bundle_configure.pl
cmd_usage(configure, "[<bundle>] [<args>] [<opts>]", [
    %1_______________________________________________
    "Configure using the specified flags",
    "(discarding any previous selection)"
]).
cmd_details(configure, Text) :-
	Text = [
          %2........................________________________________________________
	  "The following additional arguments for configure are accepted:",
	  "",
	  "  --interactive           Configure the system interactively",
	  "                          (existing previous configuration is consulted)",
	  "  --list-flags            List the bundle configuration flags",
	  "  --describe-flag <flag>  Describe the configuration flag",
	  % TODO: Use this one with care (it does not use rules)
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
cmd_usage(configclean, "[<bundle>]", [
    %1_______________________________________________
    "Clean the configuration"
]) :- advanced.

grp_def(build_grp, "Build").
%
cmd_grp(build, build_grp).
cmd_usage(build, "[<bundle>]", [
    %1_______________________________________________
    "Build a bundle (including documentation)"
]).
%
cmd_grp(prebuild_nodocs, build_grp).
cmd_usage(prebuild_nodocs, "[<bundle>]", [
    %1_______________________________________________
    "Prepare source for build_nodocs"
]) :- advanced.
%
cmd_grp(build_nodocs, build_grp).
cmd_usage(build_nodocs, "[<bundle>]", [
    %1_______________________________________________
    "Build commands and libraries (includes prebuild)"
]) :- advanced.
%
cmd_grp(build_bin, build_grp).
cmd_usage(build_bin, "[<bundle>]", [
    %1_______________________________________________
    "Only build commands (executable binaries)"
]) :- advanced.
%
cmd_grp(build_libraries, build_grp).
cmd_usage(build_libraries, "[<bundle>]", [
    %1_______________________________________________
    "Only build libraries (modules in alias paths)"
]) :- advanced.
%
cmd_grp(prebuild_docs, build_grp).
cmd_usage(prebuild_docs, "[<bundle>]", [
    %1_______________________________________________
    "Prepare source for build_docs"
]) :- advanced.
%
cmd_grp(build_docs, build_grp).
cmd_usage(build_docs, "[<bundle>]", [
    %1_______________________________________________
    "Only build documentation (includes prebuild)"
]) :- advanced.
%
cmd_grp(build_docs_readmes, build_grp).
cmd_usage(build_docs_readmes, "[<bundle>]", [
    %1_______________________________________________
    "Only build documentation READMEs"
]) :- advanced.
%
grp_details(build_grp, [
    %2........................________________________________________________
    "Build compiles all the modules (libraries), binaries, and documentation",
    "of the specified bundle. The process is incremental and should only",
    "recompile sources with modifications.",
    "",
    "Additionally, 'build_nodocs' omits documentation, while",
    "'build_docs' builds just the documentation. In some rare cases",
    "(like changes in configuration or untracked dependencies from foreign",
    "code and libraries) you may need to invoke 'clean' before 'build'.",
    %2........................________________________________________________
    "",
    "For convenience, the following subtargets are available:",
    "",
    % TODO: Do not enumerate bundles; add an option to list_bundles to show subbundles!
    "  core/engine            The engine",
    "  core/ciaoc             The 'ciaoc' compiler",
    "  core/shell             The 'ciaosh' toplevel (or shell)",
    "  core/emacs_mode        The Emacs-based IDE for Ciao",
    "  core/java              Java interface",
    "  contrib/profiler       Profiler",
    "  ciaopp/ilciao          Java resource analysis", % TODO: Does not work in 'build'!
    "",
    "Additionally, other valid targets are:",
    % TODO: this should not be needed
    "",
    "  ciaobase               Engine, compiler, and toplevel" % TODO: does not work in 'clean'!
]).

grp_def(clean_grp, "Cleaning").
%
cmd_grp(clean, clean_grp).
cmd_usage(clean, "[<bundle>]", [
    %1_______________________________________________
    "Clean intermediate files (keeps configuration)"
]).
%
cmd_grp(distclean, clean_grp).
cmd_usage(distclean, "[<bundle>]", [
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
cmd_grp(clean_nodocs, clean_grp).
% NOTE: clean_docs is NOT equivalent to 'clean' for docs; it just
% remove the final targets, not the temporary files.
cmd_usage(clean_nodocs, "[<bundle>]", [
    %1_______________________________________________
    "Like 'clean', but keep documentation targets"
]) :- advanced.
%
cmd_grp(clean_docs, clean_grp).
cmd_usage(clean_docs, "[<bundle>]", [
    %1_______________________________________________
    "Delete documentation targets (temporary files are",
    "removed in 'clean_nodocs')"
]) :- advanced.
%
cmd_grp(clean_docs_readmes, clean_grp).
cmd_usage(clean_docs_readmes, "[<bundle>]", [
    %1_______________________________________________
    "Delete documentation (README) targets"
]) :- advanced.
%
cmd_grp(clean_docs_manuals, clean_grp).
cmd_usage(clean_docs_manuals, "[<bundle>]", [
    %1_______________________________________________
    "Delete documentation (manuals) targets"
]) :- advanced.
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

grp_def(install_grp, "Installation").
%
cmd_grp(install, install_grp).
cmd_usage(install, "[<bundle>]", [
    %1_______________________________________________
    "Install libraries, binaries, and documentation"
]).
%
cmd_grp(uninstall, install_grp).
cmd_usage(uninstall, "[<bundle>]", [
    %1_______________________________________________
    "Uninstall libraries, binaries, and documentation"
]).
%
grp_details(install_grp, [
    %2........................________________________________________________
    "Install and uninstall accepts the flag:",
    "",
    "  --destdir=DESTDIR      Prepend DESTDIR to each (un)install target",
    "                         (for staged installations)"
]).

grp_def(test_grp, "Test automation and benchmarking").
%
cmd_grp(runtests, test_grp).
% TODO: Split into unit tests and the more advanced testing/benchmarking driver
cmd_usage(runtests, "[<bundle>]", [
    %1_______________________________________________
    "Execute all tests (regression, unit tests, etc.)"
]).
%
cmd_grp(runbenchmarks, test_grp).
cmd_usage(runbenchmarks, "[<bundle>]", [
    %1_______________________________________________
    "Execute all benchmarks available in the system"
]) :- advanced.

grp_def(packaging, "Packaging for distribution") :- advanced.
%
cmd_grp(gen_pbundle__TYPE, packaging). % TODO: trick, does not exist
cmd_grp(gen_pbundle__win32, packaging).
cmd_grp(gen_pbundle__rpm, packaging).
cmd_grp(gen_pbundle__src, packaging).
cmd_grp(gen_pbundle__tgz, packaging).
cmd_grp(gen_pbundle__tbz, packaging).
cmd_grp(gen_pbundle__bin, packaging).
cmd_grp(gen_pbundle__bin_tgz, packaging).
cmd_grp(gen_pbundle__bin_tbz, packaging).
cmd_grp(gen_pbundle__noa, packaging).
cmd_grp(gen_pbundle__noa_tgz, packaging).
cmd_grp(gen_pbundle__noa_tbz, packaging).
cmd_grp(gen_pbundle__pkg, packaging).
cmd_grp(gen_pbundle__macport, packaging).
cmd_grp(gen_pbundle__app, packaging).
cmd_usage(gen_pbundle__TYPE, "", [
    %1_______________________________________________
    "Generates a distribution of the specified TYPE",
    "(see gen_pbundle_hook/3)"
]).
%
cmd_grp(gen_pbundle__descfile, packaging).
cmd_usage(gen_pbundle__descfile, "", [
    %1_______________________________________________
    "Generate the pbundle_meta file"
]).
%
cmd_grp(gen_bundle_commit_info, packaging).
cmd_usage(gen_bundle_commit_info, "", [
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
cmd_grp(run, tools).
cmd_usage(run, "", [
    %1_______________________________________________
    "Runs a Ciao script (ciao-shell)"
]).
%
cmd_grp(comp, tools).
cmd_usage(comp, "", [
    %1_______________________________________________
    "Executes the compiler (ciaoc)"
]).
%
cmd_grp(doc, tools).
cmd_usage(doc, "", [
    %1_______________________________________________
    "Executes the documentation generator (lpdoc)"
]).
%
cmd_grp(pp, tools).
cmd_usage(pp, "", [
    %1_______________________________________________
    "Executes the preprocessor (ciaopp)"
]).

