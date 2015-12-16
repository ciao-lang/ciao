:- module(_, [], [fsyntax, assertions, regtypes, dcg]).

:- doc(title, "Ciao Command-line Help").
:- doc(author, "Ciao Development Team").
:- doc(author, "Jose F. Morales").

:- doc(module, "Formatted help for the @apl{ciao} and
   @apl{ciao-boot.sh} (@apl{ciao-boot.bat}) command line tool").

:- use_module(library(lists), [length/2]).
:- use_module(library(strings), [write_string/1]).
:- use_module(library(system), [using_windows/0]).

:- doc(bug, "Transform predicates into facts (later, extract from assertions)").
:- doc(bug, "Merge with optparse package").

:- export(show_help/2).
:- pred show_help(Mode, Boot) # "Show help for @tt{ciao}. @var{Mode}
   is @tt{summary}, @tt{all}, or @tt{details}. @var{Boot} is @tt{normal} or
   @tt{boot}".

show_help(Mode, Boot) :- show_help_cmd_('', Mode, Boot).

:- export(show_help_cmd/2).
:- pred show_help_cmd(Cmd, Boot) # "Show detailed help on the
   specified command @var{Cmd} (shows help of the command group)".

show_help_cmd(Cmd, Boot) :-
	show_help_cmd_(Cmd, details, Boot).

show_help_cmd_(Cmd, Mode, Boot) :-
	set_help_mode(Mode, Boot),
	help_str(Cmd, Str, []),
	write_string(Str).

% Format the help message as a string
help_str('') --> !,
	banner_help_str,
	%
	grp_help_str(help),
	grp_help_str(bootstrap),
	grp_help_str(quickstart),
	grp_help_str(promotion),
	%
	grp_help_str(configure),
	grp_help_str(build_grp),
	grp_help_str(clean_grp),
	grp_help_str(install_grp),
	grp_help_str(test_grp),
	grp_help_str(packaging),
	%
	grp_help_str(tools),
	ending_help_str.
help_str(Cmd) -->
	{ cmd_grp(Cmd, Grp) },
	!,
	grp_help_str(Grp).
help_str(_) -->
	"No help for command\n".

ciaocmd -->
	( { help_mode(_, normal) } ->
	    "ciao"
	; { using_windows } -> 
	    "ciao-boot.bat"
	; "ciao-boot.sh" 
	).

% Banner for help
banner_help_str -->
	"Usage: ", ciaocmd, " <cmd> [<args>]\n",
	"\n",
	( { help_mode(summary, normal) } ->
            "The most commonly used commands are:\n\n"
	; []
	).

ending_help_str --> { help_mode(summary, _) }, !,
	"\n",
	"Use \"", ciaocmd, " help-all\" for a list of all the available commands.\n",
	"Use \"", ciaocmd, " help <cmd>\" for more information about a command.\n",
	"\n".
ending_help_str --> { help_mode(all, _) }, !,
	"Use \"", ciaocmd, " help <cmd>\" for more information about a command.\n",
	"\n".
ending_help_str --> [].

% Group of a command
% TODO: Some commands should be unified using options, this is a
%   temporary hack.

cmd_grp(help, help).
cmd_grp(help_all, help).
%
cmd_grp(boot_build, bootstrap).
cmd_grp(boot_rebuild, bootstrap).
cmd_grp(boot_clean, bootstrap).
%
cmd_grp(local_install, quickstart).
cmd_grp(global_install, quickstart).
cmd_grp(local_install_paranoid, quickstart).
%
cmd_grp(boot_promote, promotion).
%
cmd_grp(configure, configure).
cmd_grp(rescan_bundles, configure). % TODO: move somewhere else?
cmd_grp(list_bundles, configure). % TODO: move somewhere else?
cmd_grp(configclean, configure).
%
cmd_grp(build, build_grp).
cmd_grp(prebuild_nodocs, build_grp).
cmd_grp(build_nodocs, build_grp).
cmd_grp(build_bin, build_grp).
cmd_grp(build_libraries, build_grp).
cmd_grp(prebuild_docs, build_grp).
cmd_grp(build_docs, build_grp).
cmd_grp(build_docs_readmes, build_grp).
cmd_grp(install, install_grp).
cmd_grp(uninstall, install_grp).
%cmd_grp(register, install_grp).
%cmd_grp(unregister, install_grp).
cmd_grp(clean, clean_grp).
cmd_grp(distclean, clean_grp).
cmd_grp(realclean, clean_grp).
cmd_grp(clean_nodocs, clean_grp).
cmd_grp(clean_docs, clean_grp).
cmd_grp(clean_docs_readmes, clean_grp).
cmd_grp(clean_docs_manuals, clean_grp).
cmd_grp(clean_tree, clean_grp).
cmd_grp(emergency_clean, clean_grp).
%
cmd_grp(runbenchmarks, test_grp).
cmd_grp(runtests, test_grp).
%
cmd_grp(gen_bundle_commit_info, packaging).
cmd_grp(gen_pbundle__win32, packaging).
cmd_grp(gen_pbundle__descfile, packaging).
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

all_or_details(all).
all_or_details(details).

% Help for help
grp_help_str(help) --> { help_mode(~all_or_details, _) }, !,
	help_section("Help"),
	showcmd(help, "", [
          %1_______________________________________________
	  "Show help on most commonly used commands"
        ]),
	showcmd(help_all, "", [
          %1_______________________________________________
	  "Show help on all available commands"
        ]),
	showcmd(help, "[<cmd>]", [
          %1_______________________________________________
	  "Show help on the specified command"
        ]),
	showcmdsep.
grp_help_str(help) --> [].

grp_help_str(bootstrap) --> { help_mode(~all_or_details, boot) }, !,
	help_section("Bootstrapping"),
	showcmd(boot_build, "", [
          %1_______________________________________________
	  "Build the bootstrap (if it does not exist)"
        ]),
	showcmd(boot_rebuild, "", [
          %1_______________________________________________
          "Force rebuild of the bootstrap"
        ]),
	showcmd(boot_clean, "", [
          %1_______________________________________________
	  "Clean the bootstrap"
	]),
	showcmdsep.
grp_help_str(bootstrap) --> [].

grp_help_str(quickstart) --> { help_mode(_, boot) }, !,
	help_section("Quick start commands"),
	showcmd(local_install, "[<opts>]", [
          %1_______________________________________________
          "Configure, build, and install locally (in your home directory)"
        ]),
	showcmd(global_install, "[<opts>]", [
          %1_______________________________________________
          "Configure, build, and install globally (system wide)"
        ]),
        showcmdtext([
          %2........................________________________________________________
          "",
          "See 'configure' for accepted configuration flags."
        ]),
	showcmdsep,
	% TODO: rewrite as 'compile and lint'?
	( { help_mode(~all_or_details, _) } ->
	    help_section("Quick start commands (advanced)"),
	    showcmd(local_install_paranoid, "[<opts>]", [
	      %1_______________________________________________
	      "Like 'local-install' but enable static checking",
	      "of some possible code defects (like unused",
	      "predicates, modules, etc.)."
	    ]),
	    showcmdsep
	; []
	).
grp_help_str(quickstart) --> [].

grp_help_str(promotion) --> { help_mode(~all_or_details, _) }, !,
	help_section("Bootstrap promotion (for system developers)"),
	showcmd(boot_promote, "", [
          %1_______________________________________________
          "Promote the current compiler (bytecode) and ",
          "engine (auto-generated C files) for bootstrap"
	]),
	showcmdtext([
          %2........................________________________________________________
          "",
	  "NOTE: This is a dangerous operation!",
          "      Use 'build core/engine' and 'build core/ciaoc' (or a full 'build')",
	  "      to prepare the current compiler and engine C files."
        ]),
	showcmdsep.
grp_help_str(promotion) --> [].
			       
grp_help_str(configure) -->
	% TODO: add BUNDLE as a parameter
	% TODO: configure help is also in bundle_configure.pl
	help_section("Configuration (before build)"),
	showcmd(configure, "[<bundle>] [<args>] [<opts>]", [
          %1_______________________________________________
	  "Configure using the specified flags",
	  "(discarding any previous selection)"
        ]),
	showcmdtext([
          %2........................________________________________________________
          "",
	  "The following additional arguments for configure are accepted:",
	  "",
	  "  --interactive           Configure the system interactively",
	  "                          (existing previous configuration is consulted)",
	  "  --list-flags            List the bundle configuration flags",
	  "  --describe-flag <flag>  Describe the configuration flag",
	  % TODO: Use this one with care (it does not use rules)
	  "  --set-flag <flag>=<value> Force flag value (dangerous!)"
        ]),
	showcmdsep,
	showcmd(list_bundles, "", [
          %1_______________________________________________
          "List all available bundles"
        ]),
	showcmdsep,
        % TODO: really use BUNDLE as a parameter
	grp_help_str(configure_extra),
	%
	grp_help_str(bootstrap_opts).

grp_help_str(configure_extra) --> { help_mode(~all_or_details, _) }, !,
	showcmd(rescan_bundles, "[<bundle>]", [
          %1_______________________________________________
          "Rescan the (sub)bundles (when source changes)"
        ]),
	showcmdsep,
	showcmd(configclean, "[<bundle>]", [
          %1_______________________________________________
          "Clean the configuration"
        ]),
	showcmdsep.
grp_help_str(configure_extra) --> [].

grp_help_str(bootstrap_opts) --> { help_mode(details, boot) }, !,
	% help_section("Options for bootstrapping")
        showcmdtext([
          %2........................________________________________________________
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
        ]),
	showcmdsep.
grp_help_str(bootstrap_opts) --> [].

grp_help_str(build_grp) -->
	help_section("Build"),
	showcmd(build, "[<bundle>]", [
          %1_______________________________________________
	  "Build a bundle (including documentation)"
	]),
	( { help_mode(~all_or_details, _) } ->
	    showcmd(prebuild_nodocs, "[<bundle>]", [
              %1_______________________________________________
              "Prepare source for build_nodocs"
            ])
	; []
	),
	( { help_mode(~all_or_details, _) } ->
	    showcmd(build_nodocs, "[<bundle>]", [
              %1_______________________________________________
              "Build commands and libraries (includes prebuild)"
            ])
	; []
	),
	( { help_mode(~all_or_details, _) } ->
	    showcmd(build_bin, "[<bundle>]", [
              %1_______________________________________________
              "Only build commands (executable binaries)"
            ])
	; []
	),
	( { help_mode(~all_or_details, _) } ->
	    showcmd(build_library, "[<bundle>]", [
              %1_______________________________________________
              "Only build libraries (modules in alias paths)"
            ])
	; []
	),
	( { help_mode(~all_or_details, _) } ->
	    showcmd(prebuild_docs, "[<bundle>]", [
              %1_______________________________________________
              "Prepare source for build_docs"
            ])
	; []
	),
	( { help_mode(~all_or_details, _) } ->
	    showcmd(build_docs, "[<bundle>]", [
              %1_______________________________________________
              "Only build documentation (includes prebuild)"
            ])
	; []
	),
	( { help_mode(~all_or_details, _) } ->
	    showcmd(build_docs_readmes, "[<bundle>]", [
              %1_______________________________________________
	      "Only build documentation READMEs"
            ])
	; []
	),
	showcmdtext([
          %2........................________________________________________________
          "",
    	  "Build compiles all the modules (libraries), binaries, and documentation",
          "of the specified bundle. The process is incremental and should only",
          "recompile sources with modifications.",
	  "",
    	  "Additionally, 'build_nodocs' omits documentation, while",
	  "'build_docs' builds just the documentation. In some rare cases",
	  "(like changes in configuration or untracked dependencies from foreign",
	  "code and libraries) you may need to invoke 'clean' before 'build'."
	]),
	( { help_mode(details, _) } ->
	    help_bundle_parts
	; []
	),
	showcmdsep.
%
grp_help_str(install_grp) -->
	help_section("Installation"),
	showcmd(install, "[<bundle>]", [
          %1_______________________________________________
          "Install libraries, binaries, and documentation"
        ]),
	showcmd(uninstall, "[<bundle>]", [
          %1_______________________________________________
          "Uninstall libraries, binaries, and documentation"
        ]),
	showcmdtext([
          %2........................________________________________________________
	  "",
	  "Install and uninstall accepts the flag:",
	  "",
	  "  --destdir=DESTDIR      Prepend DESTDIR to each (un)install target",
	  "                         (for staged installations)"
	]),
	showcmdsep.
	% showcmd(register, "[<bundle>]", [
        %   %1_______________________________________________
        %   "Register in the system (locally or globally)"
        % ]),
	% showcmd(unregister, "[<bundle>]", [
        %   %1_______________________________________________
        %   "Unregister from the system"
        % ]).
%
grp_help_str(clean_grp) -->
	help_section("Cleaning"),
	showcmd(clean, "[<bundle>]", [
          %1_______________________________________________
          "Clean intermediate files (keeps configuration)"
        ]),
	showcmd(distclean, "[<bundle>]", [
          %1_______________________________________________
	  "Like 'clean' but also removes configuration"
        ]),
	( { help_mode(~all_or_details, boot) } ->
	    showcmd(realclean, "", [
              %1_______________________________________________
              "Like 'distclean' but also clean the bootstrap"
            ])
	; []
	),
	( { help_mode(details, _) } ->
	    help_bundle_parts
	; []
	),
	showcmdsep,
	% NOTE: clean_docs is NOT equivalent to 'clean' for docs; it just
	% remove the final targets, not the temporary files.
	( { help_mode(~all_or_details, _) } ->
	    showcmd(clean_nodocs, "[<bundle>]", [
              %1_______________________________________________
              "Like 'clean', but keep documentation targets"
            ]),
	    showcmd(clean_docs, "[<bundle>]", [
              %1_______________________________________________
              "Delete documentation targets (temporary files are",
	      "removed in 'clean_nodocs')"
            ]),
	    showcmd(clean_docs_readmes, "[<bundle>]", [
              %1_______________________________________________
              "Delete documentation (README) targets"
            ]),
	    showcmd(clean_docs_manuals, "[<bundle>]", [
              %1_______________________________________________
	      "Delete documentation (manuals) targets"
            ]),
	    showcmdsep
	; []
	),
	%
	( { help_mode(~all_or_details, _) } ->
	    showcmd(clean_tree, "<dir>", [
              %1_______________________________________________
	      "Clean the specified directory tree"
            ]),
	    showcmdsep
	; []
	),
	% (only for bootstrap)
	( { help_mode(~all_or_details, boot) } ->
	    showcmd(emergency_clean, "", [
              %1_______________________________________________
              "Emergency clean (for unstable source trees)"
            ]),
	    showcmdtext([
              %2........................________________________________________________
	      "",
	      "NOTE: Use this option when the bootstrap is unable to compile properly and",
	      "none of the cleaning commands work."
            ]),
	    showcmdsep
	; []
	).

grp_help_str(tools) --> { help_mode(_, boot) }, !, [].
grp_help_str(tools) -->
	( { help_mode(~all_or_details, _) } -> help_section("Tools") ; [] ),
	showcmd(toplevel, "", [
          %1_______________________________________________
	  "Executes a toplevel (ciaosh) [default]"
        ]),
	showcmd(run, "", [
          %1_______________________________________________
	  "Runs a Ciao script (ciao-shell)"
        ]),
	showcmd(comp, "", [
          %1_______________________________________________
	  "Executes the compiler (ciaoc)"
        ]),
	showcmd(doc, "", [
          %1_______________________________________________
	  "Executes the documentation generator (lpdoc)"
        ]),
	showcmd(pp, "", [
          %1_______________________________________________
	  "Executes the preprocessor (ciaopp)"
        ]),
	showcmdsep.

grp_help_str(test_grp) -->
	% TODO: Split into unit tests and the more advanced testing/benchmarking driver
	help_section("Test automation and benchmarking"),
	showcmd(runtests, "[<bundle>]", [
          %1_______________________________________________
          "Execute all tests (regression, unit tests, etc.)"
	]),
	( { help_mode(~all_or_details, _) } ->
	    showcmd(runbenchmarks, "[<bundle>]", [
              %1_______________________________________________
              "Execute all benchmarks available in the system"
            ])
	; []
	),
	showcmdsep.
grp_help_str(test_grp) --> [].

grp_help_str(packaging) --> { help_mode(~all_or_details, _) }, !,
	help_section("Packaging for distribution"),
	showcmd(gen_pbundle__TYPE, "", [
          %1_______________________________________________
          "Generates a distribution of the specified TYPE",
	  "(see gen_pbundle_hook/3)"
        ]),
	showcmd(gen_pbundle__descfile, "", [
          %1_______________________________________________
          "Generate the pbundle_meta file"
        ]),
	showcmd(gen_bundle_commit_info, "", [
          %1_______________________________________________
          "[Not documented, internal]"
        ]),
	showcmdsep.
grp_help_str(packaging) --> [].

help_bundle_parts -->
	% (build)
	showcmdtext([	    
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

% ---------------------------------------------------------------------------
:- doc(section, "Help modifiers").

:- data help_mode_/2.

set_help_mode(Mode, Boot) :-
	retractall_fact(help_mode_(_, _)),
	assertz_fact(help_mode_(Mode, Boot)).

help_mode(Mode, Boot) :- help_mode_(Mode, Boot).

% ---------------------------------------------------------------------------
:- doc(section, "Formatting").

dashify(X, Y) :-
	atom_codes(X, Xs),
	dashify_(Xs, Ys),
	atom_codes(Y, Ys).

dashify_([], []) :- !.
dashify_([0'_|Xs], [0'-|Ys]) :- !, dashify_(Xs, Ys).
dashify_([X|Xs], [X|Ys]) :- dashify_(Xs, Ys).

string([]) --> [].
string([X|Xs]) --> [X], string(Xs).

% TODO: Use ANSI colors?
%
% % # Setup ANSI color if the output is a TTY and the terminal does support them
% % 
% % #if tty -s <&1 && ( [ x"${TERM}" = x"xterm" ] || [ x"${EMACS}" = x"t" ] ) ; then
% % if tty -s <&1 && [ x"${TERM}" = x"xterm" ] ; then
% %     BOLD_B="\033[1m"
% %     BOLD_E="\033[0m"
% %     UNDER_B="\033[4m"
% %     UNDER_E="\033[0m"
% % else
% %     BOLD_B=""
% %     BOLD_E=""
% %     UNDER_B=""
% %     UNDER_E=""
% % fi
% % 
% % # Echo in 'bold' style
% % bold_echo() {
% %     echo "${BOLD_B}${1}${BOLD_E}"
% % }
% % # Echo in 'under' style
% % under_echo() {
% %     echo "${UNDER_B}${1}${UNDER_E}"
% % }

bold_echo(X) --> string(X).

under_echo(X) --> string(X).

blanks(N) --> { N =< 0 }, !, [].
blanks(N) --> " ", { N1 is N - 1 }, blanks(N1).

marginsize(2).
col1size(27).

% Name of the command from the command-line (replace '_' by '-')
showcmd(Cmd, _Args, Desc) -->
	{ \+ help_mode(details, _) },
	!,
	showcmd_(Cmd, "", Desc).
showcmd(Cmd, Args, Desc) -->
	showcmd_(Cmd, Args, Desc).

showcmd_(Cmd, Args, Desc) -->
	% Margin
	{ marginsize(Margin) },
	blanks(Margin),
	% Command name
	{ dashify(Cmd, Cmd1) },
	{ atom_codes(Cmd1, Cmd2) },
	bold_echo(Cmd2),
	% Command arguments (optional)
	{ Args = "" -> Args2 = ""
	; Args2 = " "||Args
	},
	string(Args2),
	% Compute size for alignment
	{ length(Cmd2, La) },
	{ length(Args2, Le) },
	{ L is La + Le + Margin },
	{ col1size(Col) },
	{ L < Col -> Pad is Col - L ; Pad = 0 },
	( { Pad = 0 } -> "\n", blanks(Col)
	; blanks(Pad)
	),
	aligntext(Desc, Col).

% Additional description of a command
showcmdtext(_) --> { \+ help_mode(details, _) }, !,
	[].
showcmdtext(Text) -->
	% Margin (twice)
	{ marginsize(Margin), Margin2 is 2 * Margin },
	blanks(Margin2),
	aligntext(Text, Margin2).

% Display an aligned text. The text is given as a list of lists.
% (assume that the alignment padding for the first line is already
% printed)
aligntext([Row|Rows], Col) --> !,
	string(Row), "\n",
	( { Rows = [] } -> []
	; % More rows, align and print
	  blanks(Col),
	  aligntext(Rows, Col)
	).
aligntext([], _Col) --> [].

help_section(_) --> { help_mode(summary, _) }, !, [].
help_section(Text) -->
	under_echo(Text),
	":\n",
	"\n".

showcmdsep --> { help_mode(summary, _) }, !, [].
showcmdsep -->
	"\n".

