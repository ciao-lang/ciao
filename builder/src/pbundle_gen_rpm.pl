:- module(pbundle_gen_rpm, [], [fsyntax, assertions]).
:- doc(title, "RPM-based distributions").
:- doc(subtitle, "An automated RPM binary package generator for Ciao").

:- doc(author, "Jos@'{e} Luis Gonz@'{a}lez").
:- doc(author, "Edison Mera").
:- doc(author, "Jos@'{e} F. Morales").
:- doc(author, "The Ciao Development Team").

:- doc(ack, "This work builds on the work of Manuel Carro, Emilio
   Gallego and Edison Mera. Thank you also to Manuel Hermenegildo and
   Germ@'{a}n Puebla for their invaluable support.").

:- doc(copyright,"Copyright @copyright{} 2006--2007 Jos@'{e} Luis
   Gonz@'{a}lez/The Ciao Development Team.").

% ===========================================================================

:- use_module(ciaobld(messages_aux), [normal_message/2]).

:- use_module(library(aggregates)).
:- use_module(library(lists)).
:- use_module(library(llists)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(process), [process_call/3]).
:- use_module(library(system), [copy_file/3, working_directory/2]).
:- use_module(library(system_extra),
	[copy_files/3, del_file_nofail/1, del_files_nofail/1]).
:- use_module(library(stream_utils)).
:- use_module(library(version_strings), [version_split_patch/3]).

:- use_module(engine(internals), ['$bundle_prop'/2]).
:- use_module(library(pathnames), [path_concat/3]).
:- use_module(library(bundle/bundle_info), [bundle_version/2]).
:- use_module(library(bundle/bundle_paths), [bundle_path/3]).
:- use_module(ciaobld(bundle_hash), [bundle_commit_info/3]).
:- use_module(ciaobld(pbundle_generator)).
:- use_module(ciaobld(pbundle_gen_src)).
:- use_module(ciaobld(builder_aux), [wr_template/4]).
:- use_module(ciaobld(eng_defs), [eng_cfg/2]).
:- use_module(ciaobld(config_common), [default_eng_def/1]).

% (hooks for gen_pbundle)
:- include(ciaobld(pbundle_gen_hookdefs)).

:- doc(summary, "
RPM generator is a system that builds binary RPM packages
for @apl{Ciao} automatically (with no user interaction),
for as many Linux distributions as possible, and requiring minimal
packager maintenance. The system is designed to meet the following
requirements:

@begin{itemize}
@item Keep packager maintenance effort as close as possible to zero
  while @apl{Ciao} bundles (software packages) evolve from version to
  version. In most cases the system is able to build packages for a
  new @apl{Ciao} version fully automatically.
@item It allows building each @apl{Ciao} bundle separately. It is
  also possible to produce a single package with the whole
  @apl{Ciao} bundle.
@item In order to simplify @apl{Ciao} distribution, only one RPM
  package is produced for each @apl{Ciao} bundle and
  architecture. This single package should work in all Linux
  distributions. (This also saves the time and resources that would
  require building a set of packages for each Linux distribution.)
@item The system is meant to be portable. It should work (build packages)
  in any RPM Linux system with equivalent behaviour in all of them
  (except for differences in the local RPM version and system libraries).
@end{itemize}
").

:- doc(module, "
@section{Building RPM packages}

@subsection{Requirements (rpm)}

These are the main prerequisites for the build process to succeed:

@begin{itemize}
@item A user account with enough free space (at least twice that 
	necessary for a compiled local repository).
@item A local @apl{Ciao} repository with documentation already generated.
@item A working installation of @apl{Ciao}. (This is needed to generate
	the @concept{RPM specification} and handle the build process.)
@item @apl{RPM} v3 or higher.
@item @apl{rpmbuild} installed in your system. (@tt{yum install rpm-build})
@end{itemize}

The usual @apl{Ciao} software requirements also apply, but these are
declared in the @concept{RPM specification} and the build process should
complain if anything is missing.

@subsection{Instructions (rpm)}

By default, a source @apl{Ciao} distribution for your local repository
will be packaged. Building is requested with the following command:

@begin{itemize}
@item @tt{./ciao-boot.sh gen_pbundle --kind=rpm [--option=value...]}
@end{itemize}

@subsection{Options summary (rpm)}

One or more options may be added to the building command.

These control the behaviour of the building and generation processes: 

@begin{description}

@item{@tt{vendor_independent=yes}} Produces packages that should work in all
RPM-based Linux distributions. If disabled the packages are only
guaranteed to work in the same distribution (vendor) they were built on.

@end{description}

Vendor-dependent paths and filenames can also be modified as options.
This is only recommended for building vendor dependent packages
(@code{vendor_independent=no}) since default values are sensible and
changing @tt{emacs_sitestart_dir} or @tt{emacs_sitestart_filename}
messes up with vendor-independent installation/removal scripts. These
would set the default values:

@begin{itemize}
@item @tt{install_info_cmd=/sbin/install-info}
@item @tt{emacs_sitestart_dir=@{_datadir@}/emacs/site-lisp/site-start.d}
@item @tt{emacs_sitestart_filename=ciao-mode-init.el}
@end{itemize}

There is also support for changing some of Ciao's project details:

@begin{description}
@item{@tt{repo_dirname=Ciao}}
	Sets @apl{Ciao}'s directory name for its @apl{subversion} repository.
	Relative to the RPM build directory (absolute paths not allowed).
@item{@tt{repo_uri=file:///home/clip/SvnReps/Systems/CiaoDE/trunk}}
	Sets @apl{Ciao}'s checkout URI for its @apl{subversion} repository.
@item{@tt{bibrepo_dirname=CiaoDE/bibtex_clip}}
	Sets the Ciao bibliography directory name for its @apl{subversion}
	repository. Relative to the RPM build directory
	(absolute paths not allowed.)
@item{@tt{bibrepo_uri=file:///home/clip/SvnReps/bibtex/clip}}
	Sets the Ciao bibliography checkout URI for its
	@apl{subversion} repository.
@end{description}

@section{Installing RPM packages}

A RPM package can be installed with:

@begin{verbatim}
# rpm -U package(s).rpm
@end{verbatim}

In order to upgrade your currently installed version of a package or
packages to a newer one, the same command is used with the new package(s).
This replaces the old (installed) version with the new one.

@section{Packager maintenance guide (rpm)}

The system comprises the following elements:

@begin{enumerate}
@item A stub (@file{pbundle_gen_rpm.pl}) for @apl{Ciao}'s installer that
	handles the whole process.
@item A shell script (@file{RPM-Ciao.bash}) that ensures that an adequate
	RPM building environment exists (and sets it up if it doesn't)
	before running the actual build.
@item A skeleton for the @index{RPM specification} (@file{Ciao.spec.skel}).
@end{enumerate}

When the build process is run, an @concept{RPM specification} file is
generated on-the-fly from the skeleton. Then the @apl{RPM-Ciao.bash}
script builds the packages from this specification, the resulting
packages get moved to the @file{<builddir>/pbundle/} directory, and the
RPM building environment is cleaned up.

@subsection{Changes demanding maintenance (rpm)}

Any of the following changes to @apl{Ciao} requires that the @concept{RPM
specification} skeleton be updated accordingly:

@begin{itemize}
@item Major changes to the top path structure (@file{bin/}, @file{lib/ciao/},
	@file{lib/ciaopp/}, @file{lib/lpdoc/}, etc.)
@item Added, removed, or renamed binaries.
@item Renamed or relocated documentation.
@item Added, removed, or renamed (Ciao) bundles. This also affects the
      installer stub.
@item Changes to requirements for building the bundles.
@item Changes to requirements for running the bundles.
@item Changes in the configuration or build system.
@item For SVN compilation/building: changes to repository names or locations.
@end{itemize}

Finally, there is a minor risk to be considered: that SuSE changes
its distribution-specific peculiarities or some of the RPM-based
Linux distributions change the path or name for the install-info
command, the path or name for (x)emacs site-start scripts (all of which
conveniently defined on top of the specification), or the names for
packages listed as @tt{BuildRequires}.

@subsection{Further reading (rpm)}

The reference documentation for RPM is available at:

@begin{itemize}
@item @href{http://fedora.redhat.com/docs/drafts/rpm-guide-en/}{RPM Guide}
@item @href{http://www.rpm.org/max-rpm/}{Maximum RPM}
@end{itemize}

The former resource is extensive and current, the latter includes a
convenient global index.

Many specific details not covered by those documents are scattered in
RPM's own documentation (a handful of note files).
").

% ===========================================================================
:- doc(section, "pbundle Generation as a 'RPM package'").

% Options (each one option(Name,Value)) that will be used for RPM
% generation. Defaults are provided on top of Ciao.spec.skel, so we only
% need to list here the ones that the user explicitly sets. If the same option
% appears more than once, the first occurence will take precedence.

% (hook)
% Generate RPM packages. A source distribution is generated if missing.
gen_pbundle_hook(rpm, Target, _Options) :- !,
	% TODO: Allow options in _Options (option(Name, Val))
	Opts = [
	    % TODO: customize?
	    option('vendor_independent', 'yes')
	],
	gen_pbundle__rpm(Target, Opts).

% ---------------------------------------------------------------------------

% TODO: Enable this target for debugging RPM spec generation

% % @subsection{Troubleshooting (rpm)}
% % 
% % The following command is available for testing purposes:
% % 
% % @begin{verbatim}
% % ./ciao-boot.sh gen_pbundle --kind=rpm_spec
% % @end{verbatim}
% % 
% % This produces the @concept{RPM specification} for your current
% % @apl{Ciao} version, but does not build any package with it. The
% % specification is left in the top directory of your repository (named
% % @file{Ciao.spec}). You can then review and modify it at will and build
% % the packages manually (by running @apl{rpmbuild} by yourself). All
% % generator options are also available through rpmbuild's @tt{--define}
% % facility (see the specification for details). This is useful to create
% % custom packages and facilitates package generation for development
% % snapshots of @apl{Ciao} (where some binaries or documentation may be
% % temporarily unavailable or located in unusual places).
% 
% % TODO: necessary target?
% gen_pbundle__rpm_spec ...
% 	create_rpm_spec(Target).

:- doc(bug, "To speed up the process, we create the rpm from a
	precompiled bin distribution.").

:- doc(bug, "Some @apl{rpmbuild} versions are said to no longer
	support --defining a macro's value as an argument. This would
	break generation options.").

bin_pkgname(Target, F) :-
	EngCfg = ~eng_cfg(~default_eng_def),
	F = ~atom_concat([~dist_versioned_pkgname(Target), '-bin-', EngCfg]).

:- pred gen_pbundle__rpm(Target, GenerationOptions) # "
	Handle generation of RPM packages according to
	@var{GenerationOptions} (see @ref{Options summary}.)".
%	option(@var{Macro},@var{Value})).

gen_pbundle__rpm(Target, GenerationOptions) :-
	VersionedPkgName = ~dist_versioned_pkgname(Target),
	normal_message("creating RPM package for ~w", [VersionedPkgName]),
	%
	create_rpm_spec(Target),
	%
	SpecFileName = 'Ciao.spec',
	%
	OutputDirName = ~pbundle_output_dir(Target),
	create_pbundle_output_dir(Target),
	rpm_prevailingoptions(GenerationOptions, RpmbuildOptions),
	rpmbuild_setoptions(RpmbuildOptions, RpmbuildArgs),
	process_call(~bundle_path(builder, 'src/rpm/RPM-Ciao.bash'),
	       [~atom_concat(OutputDirName, '/'),
		~bin_pkgname(Target),
		SpecFileName | RpmbuildArgs], []),
	rpm_macrovalue('_arch',   Arch),
	rpm_macrovalue('_rpmdir', RpmDir),
	%
	CiaoRpmFileName = ~path_concat(~path_concat(RpmDir, Arch), ~rpm_file_name(Target, Arch)),
	%
	copy_file(CiaoRpmFileName, OutputDirName, [overwrite]),
 	del_file_nofail(~atom_concat(~path_concat(OutputDirName, VersionedPkgName), '.tar.gz')),
	del_file_nofail(CiaoRpmFileName).

% TODO: ugly
dist_move_mans(Target) :=
	~flatten(~findall(S, dist_move_man(Target, S))).

dist_move_man(Target, BundleMoveMan) :-
	dist_bundles(Target, Bundle),
	atom_codes(Bundle, BundleS),
	atom_codes(~bundle_version(Bundle), BundleVersion),
	BundleMoveMan = [
          "mv %{buildroot}%{_mandir}/"||BundleS,"-"||BundleVersion,".manl",
	    " %{buildroot}%{_mandir}/man1/"||BundleS,".1\n"].

% TODO: ugly
install_info_cmds(Target, Command) :=
	~flatten(~findall(S, install_info_cmd(Target, Command, S))).

install_info_cmd(Target, Command, Cmd) :-
	dist_bundles(Target, Bundle),
	atom_codes(Bundle, BundleS),
	atom_codes(~bundle_version(Bundle), BundleVersion),
	Cmd = [
	    "    install-info "||Command,
	    " --dir-file=%{_infodir}/dir",
            " %{_infodir}/"||BundleS,"-"||BundleVersion,".info\n"].

% TODO: ugly
dist_files(Target) := ~flatten(~findall(S, dist_file(Target, S))).

dist_file(Target, BundleFile) :-
	dist_bundles(Target, Bundle),
	atom_codes(Bundle, BundleS),
	Version = ~bundle_version(Bundle),
	version_split_patch(Version, VersionNopatch, _),
	atom_codes(Version, BundleVersion),
	atom_codes(VersionNopatch, BundlePathVersion),
	BundleFile = [
	  "%{_libdir}/"||BundleS,
          "/"||BundleS,
          "-"||BundlePathVersion,
          "\n",
          "%{ciaodocdir}/"||BundleS,
          "-"||BundleVersion,
          ".pdf\n"].

:- pred create_rpm_spec/1 # "Generate RPM specification file.".
create_rpm_spec(Target) :-
	% TODO: Ciao.spec is hardwired
        get_rpm_version_and_release(Target, Version, Release),
	working_directory(Cwd, Cwd), % TODO: sure?
	Version = ~dist_version(Target),
	version_split_patch(Version, VersionNopatch, _),
	wr_template(at(Cwd), ~bundle_path(builder, 'src/rpm'), 'Ciao.spec', [
	    'Version' = Version,
	    'Release' = Release,
	    'VersionedPkgName' = ~dist_versioned_pkgname(Target),
	    'BinPkgName' = ~bin_pkgname(Target),
	    'BundleMoveMans' = ~dist_move_mans(Target),
	    'BundleIntegrateInfoindexes' = "",
	    'BundleInstallInfoCmds' = ~install_info_cmds(Target, ""),
	    'BundleInstallInfoCmdsRemove' = ~install_info_cmds(Target, "--remove"),
	    'BundleFiles' = ~dist_files(Target),
	    'CiaoPathVersion' = VersionNopatch]).

:- pred rpm_macrovalue(Macro, Value) # "RPM @var{Macro} is
   system-defined as @var{Value}.".

% Utilities to communicate Ciao installer with RPM macro system:
rpm_macrovalue(Macro, Value) :-
	MacroExpr = ~atom_concat(['%', Macro]),
	process_call(path(rpm), ['--eval', MacroExpr], [stdout(line(String))]),
	atom_codes(Value, String),
	% (returned) Value = (requested) %Macro would mean Value not defined
	Value \= MacroExpr. % So fail if macro not defined

:- pred rpmbuild_setoptions(RpmOptions, RpmbuildArgs) # "
	@var{RpmbuildArgs} are the command-line arguments for
	@apl{rpmbuild} that will set the RPM generation options. Each
	option is set in the specification by defining the macro of
	the same name to its appropriate value (see @pred{map_rpmoptval/2}).
	".

rpmbuild_setoptions([], []).
rpmbuild_setoptions([option(Macro, Value)|L], Args) :-
	map_rpmoptval(option(Macro, Value), RpmValue),
	Args = ['--define', ~atom_concat([Macro, ' ', RpmValue])|Args0],
	rpmbuild_setoptions(L, Args0).

:- pred rpm_prevailingoptions(Options, PrevailingOptions) # "
	@var{Options} is a list of options for RPM generation, with
	possible repetitions (same option with different values).
	@var{PrevailingOptions} is the same list without repetitions,
	the first occurrence taking precedence over the following ones.".
% First occurence is chosen (instead of last) since this eases doing
% [ overwriting_option1, overwriting_option2 | Defaults ]

rpm_prevailingoptions([], []).
rpm_prevailingoptions([option(OptName, Val)|L],
	    [option(OptName, Val)|PL]) :-
	delete_non_ground(L, option(OptName, _), DL),
	rpm_prevailingoptions(DL, PL).

% If last occurence were to take precedence instead of first:
%
%rpm_prevailingoptions( [], [] ).
%rpm_prevailingoptions( [ option( OptName, _ ) | L ], [ PL ] ) :-
%	member( option(OptName, _ ), L ),
%	!
%	rpm_prevailingoptions( L, PL ).
%rpm_prevailingoptions( [ H | L ], [ H | PL ] ) :-
%	rpm_prevailingoptions( L, PL ).


% Utilities to keep common options between Ciao installer and RPM spec:

:- pred map_rpmoptval(Option, RpmValue) 
# "@var{RpmValue} is the value to use in the RPM specification for
   @var{Option}. It may just be @var{Option}'s straight value or a
   mapped representation if the RPM specification needs so.".

map_rpmoptval(option(Opt, Val), MappedVal) :-
	ciaorpm_opttype(Opt, Type),
	ciaorpm_mapvalue(Type, Val, MappedVal),
	!.
map_rpmoptval(option(_, Val), Val).

:- pred ciaorpm_opttype(OptName, OptType)
# "This predicate declares the kind of some options (it is not meant
   to be exhaustive, only the ones whose value gets mapped need to be
   declared). @var{OptType} is the option type for an option named
   @var{OptName}.".

ciaorpm_opttype('vendor_independent', 'rpm_expression').
% Since we only need to map rpm_expressions there's no need to declare
% (and keep track of) other options.

:- pred ciaorpm_mapvalue(OptionType, OptionValue, RpmValue) 
# "Maps values between RPM generator (Ciao) options and RPM macros.
   @var{RpmValue} is the value to use in the RPM specification for an
   option of type @var{OptionType} whose value is set to
   @var{OptionValue} in Ciao. Anything not declared here is assumed to
   be immediately usable (does not need mapping).".

ciaorpm_mapvalue('rpm_expression', 'yes', '1').
ciaorpm_mapvalue('rpm_expression', 'no',  '0').

% ===========================================================================
% Naming conventions for RPM files

rpm_file_name(Target, Arch) := Name :-
	PkgName = ~dist_pkgname(Target),
	get_rpm_version_and_release(Target, VersionNopatch, Release),
	Name = ~atom_concat([PkgName, '-', VersionNopatch, '-', Release, '.', Arch, '.rpm']).

% Extract the version and release numbers from commit desc
% (usually version and patch)
get_rpm_version_and_release(Target, VersionNopatch, Release) :-
	Version = ~dist_version(Target),
	version_split_patch(Version, VersionNopatch, Patch),
        Desc = ~bundle_commit_info(~dist_main_bundle(Target), desc),
        ( atom_concat([Version, '-', Release0], Desc) ->
            % Replace '-' in release number (necessary for RPM)
            atom_codes(Release0, Release1),
            replace_char(Release1, 0'-, 0'., Release2),
            atom_codes(Release, Release2)
        ; % just in case the previous fails (it should not)
          Release = Patch
        ).

replace_char([], _, _, []).
replace_char([A|As], A, B, [B|Bs]) :- !, replace_char(As, A, B, Bs).
replace_char([X|As], A, B, [X|Bs]) :- replace_char(As, A, B, Bs).

% ===========================================================================

:- doc(section, "Future Work (rpm)").

:- doc(bug, "Check and warn for unknown options or incorrect values
	(at this time if the user sets an invalid option it gets silently 
	ignored).").
:- doc(bug, "Better compliance with @apl{rpmlint}").
:- doc(bug, "Check whether enabling mysql / java is feasible").
:- doc(bug, "Bugs from Emilio's email (debian package)").
:- doc(bug, "Eliminate PATHS warning issued by certain Ciao binaries").
:- doc(bug, "RPM-Ciao.bash should be replaced by Prolog code in this module").
:- doc(bug, "Review package descriptions").

