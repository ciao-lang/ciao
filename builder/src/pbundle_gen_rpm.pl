:- module(pbundle_gen_rpm, [], [fsyntax, assertions]).

:- doc(title, "RPM-based distributions").
:- doc(subtitle, "An automated RPM binary package generator for Ciao").

:- doc(author, "Jos@'{e} Luis Gonz@'{a}lez").
:- doc(author, "Edison Mera").
:- doc(author, "Jos@'{e} F. Morales").
:- doc(author, "The CLIP Group").

:- doc(ack, "This work builds on the work of Manuel Carro, Emilio
   Gallego and Edison Mera. Thank you also to Manuel Hermenegildo and
   Germ@'{a}n Puebla for their invaluable support.").

:- doc(copyright,"Copyright @copyright{} 2006--2007 Jos@'{e} Luis
   Gonz@'{a}lez/The CLIP Group.").

% ===========================================================================

:- use_module(ciaobld(messages_aux), [cmd_message/3]).
:- use_module(library(bundle/bundle_params),
	[bundle_param_value/2]).

:- use_module(library(aggregates)).
:- use_module(library(lists)).
:- use_module(library(llists)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(process), [process_call/3]).
:- use_module(library(system), [copy_file/3, working_directory/2]).
:- use_module(library(system_extra),
	[copy_files/3, del_file_nofail/1, del_files_nofail/1]).
:- use_module(library(file_utils)).

:- use_module(engine(internals), ['$bundle_prop'/2]).
:- use_module(library(bundle/paths_extra), [fsR/2]).
:- use_module(library(bundle/bundle_info), [
	enum_sub_bundles/2,
	bundle_version/2, bundle_patch/2,
	bundle_version_patch/2]).
:- use_module(ciaobld(bundle_hash), [
	bundle_versioned_packname/2, bundle_commit_info/3]).
:- use_module(ciaobld(pbundle_generator)).
:- use_module(ciaobld(pbundle_gen_src)).
:- use_module(ciaobld(builder_aux), [wr_template/4]).
:- use_module(ciaobld(config_common),
	[get_eng_cfg/2,
	 bundle_to_bldid/2]).

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
@item @index{Versioned packages} can be produced for specific versions of
  the @apl{Ciao} bundles so that users can install as many versions
  of the bundles as they wish at the same time (each version from its own
  versioned package). The regular (non-versioned) package is meant for
  the main system version (usually the latest) to be upgraded.
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
@item @tt{./ciao-boot.sh gen_pbundle__rpm [--option=value...]}
@end{itemize}

@subsection{Options summary (rpm)}

One or more options may be added to the building command.

These control the behaviour of the building and generation processes: 

@begin{description}

@item{@tt{subpackages=yes}} Enables building each bundle on its own
subpackage. Without subpackages (option set to @tt{'no'}) a single
@file{Ciao} package will be produced with the whole bundle.

@item{@tt{versionp=yes}} Produces versioned packages instead of regular
ones.  These are packages that allow installing several versions of
Ciao or its bundles at the same time (see @ref{Installing
packages}). The contents for both kinds of packages are identical
except that the versioned ones lack the simple names for binaries
(e.g., they include an executable called @tt{ciao-1.13} but not one
called @tt{ciao}) and add a version suffix to the names of manual
pages.  This option @em{must} be used along with subpackages.

@item{@tt{vendor_independent=yes}} Produces packages that should work in all
RPM-based Linux distributions. If disabled the packages are only
guaranteed to work in the same distribution (vendor) they were built on.

@end{description}

Setting any of the following compiles @apl{Ciao} with the feature of
the same name enabled (as defined in @tt{./ciao-boot.sh configure}):

@begin{itemize}
@item @tt{with_gsl=yes}
@item @tt{with_mysql=yes}
@item @tt{with_java_interface=yes}
@end{itemize}

Note that none of these gets compiled in by default (if the option is
not explicitly set.)

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

If you want to install more than one version of a @apl{Ciao} bundle
at the same time, just install a regular package for the latest version
(the one that you want to be upgraded as the system's main version) and
also install a versioned package for each of the additional (older)
versions. For instance, if you want to install both ciao 1.18, 1.16 and
1.14, do:

@begin{verbatim}
# rpm -U ciao-1.18.0-XXX.rpm
# rpm -U ciao-v1.16-1.16.0-XXX.rpm
# rpm -U ciao-v1.14-1.14.1-XXX.rpm
@end{verbatim}

The main (regular) installation will be available with plain commands
(@apl{ciao}, @apl{ciaoc}) as well as versioned commands. The older
(versioned) installations will only be available through versioned
commands (e.g. @tt{ciao-1.16}, @tt{ciaoc-1.14}.)

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

Special care must be taken to guarantee that @concept{versioned
packages} remain installable along with other versioned packages of the
same bundle (as well as the regular upgradeable package), provided
they are all different versions. Special care must also be taken to
replicate changes in the separate subpackages to the main package.

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

:- pred rpm_options(Bundle, Opts) # "@var{Opts} are the options to be
	used in the RPM generation of @var{Bundle}. All these get
	defined as RPM macros of the same name.".

rpm_options(Bundle) := Options :-
	Options = ~append([
	    % These are compulsory (gen_pbundle__rpm/2 needs them to know how to behave).
	    % We set a default value if the option wasn't defined by user:
	    ~rpm_option_or_default('subpackages', 'no'),
	    ~rpm_option_or_default('versionp',    'no'),
	    ~rpm_option_or_default('vendor_independent', 'yes')|
            % All other options are optional. We only set them if defined by user:
            % TODO: INCORRECT!
	    ~findall(option(OptName, OptVal), bundle_param_value(_Bundle:OptName, OptVal))
	    ],
	    % All available subpackages are produced by default:
	    ~findall(option(SubBundle, 'yes'), enum_sub_bundles(Bundle, SubBundle))).

% ===========================================================================
:- doc(section, "pbundle Generation as a 'RPM package'").

% Options (each one option(Name,Value)) that will be used for RPM
% generation. Defaults are provided on top of Ciao.spec.skel, so we only
% need to list here the ones that the user explicitly sets. If the same option
% appears more than once, the first occurence will take precedence.

% (hook)
% Generate RPM packages. A source distribution is generated if missing.
gen_pbundle_hook(rpm, Bundle, _Options) :- !,
	gen_pbundle__rpm(Bundle, ~rpm_options(Bundle)).

% ---------------------------------------------------------------------------

% TODO: Enable this target for debugging RPM spec generation

% % @subsection{Troubleshooting (rpm)}
% % 
% % The following command is available for testing purposes:
% % 
% % @begin{verbatim}
% % ./ciao-boot.sh gen_pbundle__rpm_spec
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
% gen_pbundle__rpm_spec <- [] #
% 	"Generate a RPM specification for regular packages." :-
% 	Bundle = ~root_bundle,
% 	create_rpm_spec(Bundle).

:- pred versioned_packagename(BundleName, PackName) #
   "@var{PackName}, is the versioned RPM package name for
   @apl{Ciao}'s bundle @var{BundleName}.".

versioned_packagename(BundleName, PName) :-
	'$bundle_prop'(BundleName, packname(Packname)),
	atom_concat([Packname, '-v', ~bundle_version(BundleName)], PName).

:- doc(bug, "To speed up the process, we create the rpm from a
	precompiled bin distribution.").

:- doc(bug, "Some @apl{rpmbuild} versions are said to no longer
	support --defining a macro's value as an argument. This would
	break generation options.").

bin_packname(Bundle, F) :-
	bundle_to_bldid(Bundle, BldId),
	EngCfg = ~get_eng_cfg(BldId),
	F = ~atom_concat([~bundle_versioned_packname(Bundle), '-bin-', EngCfg]).

:- pred gen_pbundle__rpm(Bundle, GenerationOptions) # "
	Handle generation of RPM packages according to
	@var{GenerationOptions} (see @ref{Options summary}.)".
%	option(@var{Macro},@var{Value})).

gen_pbundle__rpm(Bundle, GenerationOptions) :-
	VersionedPackName = ~bundle_versioned_packname(Bundle),
	cmd_message(Bundle, "creating RPM package for ~w ...", [VersionedPackName]),
	%
	create_rpm_spec(Bundle),
	%
	SpecFileName = 'Ciao.spec',
	%
	pbundle_output_dir(OutputDirName),
	rpm_prevailingoptions(GenerationOptions, RpmbuildOptions),
	rpmbuild_setoptions(RpmbuildOptions, Bundle, RpmbuildArgs),
	process_call(~fsR(~builder_src_dir/'rpm'/'RPM-Ciao.bash'),
	       [~atom_concat(OutputDirName, '/'),
		~bin_packname(Bundle),
		SpecFileName | RpmbuildArgs], []),
	create_pbundle_output_dir,
	rpm_macrovalue('_arch',   Arch),
	rpm_macrovalue('_rpmdir', RpmDir),
 	% TODO: generalize, use fsR
	'$bundle_prop'(Bundle, packname(Packname)),
	atom_concat([RpmDir, '/', Arch, '/', ~rpm_file_name(Bundle, Packname, Arch)], CiaoRpmFileName),
	%
	findall(BundleRpmFileName, bundle_rpm_file_names(Bundle, RpmDir, Arch, RpmbuildOptions, BundleRpmFileName), RpmFileNames),
	%
	( member(option('subpackages', 'no'), RpmbuildOptions) ->
	    copy_file(CiaoRpmFileName, OutputDirName, [overwrite])
	;
	    true
	),
	( member(option('subpackages', 'yes'), RpmbuildOptions) ->
	    copy_files(RpmFileNames, OutputDirName, [overwrite])
	;
	    true
	),
 	del_file_nofail(~atom_concat(~fsR(OutputDirName/VersionedPackName), '.tar.gz')),
	del_file_nofail(CiaoRpmFileName),
	( member(option('subpackages', 'yes'), RpmbuildOptions) ->
	    del_files_nofail(RpmFileNames)
	; true
	).

% (only when subpackages=yes)
% (nondet)
bundle_rpm_file_names(Bundle, RpmDir, Arch, RpmbuildOptions, BundleRpmFileName) :-
	enum_sub_bundles(Bundle, SubBundle),
	( member(option('versionp', 'yes'), RpmbuildOptions) ->
	    versioned_packagename(SubBundle, Ciaoname) % TODO: Probably wrong
	; '$bundle_prop'(SubBundle, packname(Ciaoname))
	),
	% TODO: refactor
	atom_concat([RpmDir, '/', Arch, '/', ~rpm_file_name(SubBundle, Ciaoname, Arch)], BundleRpmFileName).

:- doc(bug, "For now, release is equal to the SVN revision number,
	that is done only to allow generating the RPM now, but when 
	version numbers are clarified, this should be reconsidered.").

bundle_versioned_names(Bundle) :=
	~flatten(~findall(S, bundle_versioned_name(Bundle, S))).

bundle_versioned_name(ParentBundle, BundleVersionName) :-
	enum_sub_bundles(ParentBundle, Bundle),
	atom_codes(Bundle, BundleS),
	atom_codes(~bundle_version(Bundle), BundleVersion),
	BundleVersionName = [
          "%define "||BundleS,"name "||
          BundleS, "-v"||BundleVersion,"\n"
        ].

bundle_name_defs(Bundle) := ~flatten(~findall(S, bundle_name_def(Bundle, S))).

bundle_name_def(ParentBundle, BundleName) :-
	enum_sub_bundles(ParentBundle, Bundle),
	atom_codes(Bundle, BundleS),
	BundleName = [
          "%define "||BundleS,"name "||
          BundleS,"\n"
        ].

bundle_move_man_versions(Bundle) :=
	~flatten(~findall(S, bundle_move_man_version(Bundle, S))).

bundle_move_man_version(ParentBundle, BundleMoveManVersion) :-
	enum_sub_bundles(ParentBundle, Bundle),
	atom_codes(Bundle,                    BundleS),
	atom_codes(~bundle_version_patch(Bundle), BundleVersion),
	BundleMoveManVersion = [
          "mv %{buildroot}%{_mandir}/"||BundleS, "-"||BundleVersion, ".manl ",
	     "%{buildroot}%{_mandir}/man1/"||BundleS, "-"||BundleVersion, ".1\n"].

bundle_move_mans(Bundle) :=
	~flatten(~findall(S, bundle_move_man(Bundle, S))).

bundle_move_man(ParentBundle, BundleMoveMan) :-
	enum_sub_bundles(ParentBundle, Bundle),
	atom_codes(Bundle,                    BundleS),
	atom_codes(~bundle_version_patch(Bundle), BundleVersion),
	BundleMoveMan = [
          "mv %{buildroot}%{_mandir}/"||BundleS,"-"||BundleVersion,".manl",
	    " %{buildroot}%{_mandir}/man1/"||BundleS,".1\n"].

bundle_install_info_cmds(Bundle, Command) :=
	~flatten(~findall(S, bundle_install_info_cmd(Bundle, Command, S))).

bundle_install_info_cmd(ParentBundle, Command, BundleInstallInfoCmd) :-
	enum_sub_bundles(ParentBundle, Bundle),
	atom_codes(Bundle,                    BundleS),
	atom_codes(~bundle_version_patch(Bundle), BundleVersion),
	BundleInstallInfoCmd = [
	    "    install-info "||Command,
	    " --dir-file=%{_infodir}/dir",
            " %{_infodir}/"||BundleS,"-"||BundleVersion,".info\n"].

bundle_files(Bundle) := ~flatten(~findall(S, bundle_file(Bundle, S))).

bundle_file(ParentBundle, BundleFile) :-
	enum_sub_bundles(ParentBundle, Bundle),
	atom_codes(Bundle,                    BundleS),
	atom_codes(~bundle_version_patch(Bundle), BundleVersion),
	atom_codes(~bundle_version(Bundle), BundlePathVersion),
	BundleFile = [
	  "%{_libdir}/"||BundleS,"/"||BundleS,"-"||BundlePathVersion,"\n",
          "%{ciaodocdir}/"||BundleS,"-"||BundleVersion,".pdf\n"].

:- pred create_rpm_spec/1 # "Generate RPM specification file.".
create_rpm_spec(Bundle) :-
	% TODO: Ciao.spec is hardwired
        get_rpm_version_and_release(Bundle, Version, Release),
	working_directory(Cwd, Cwd), % TODO: sure?
	wr_template(at(Cwd), ~builder_src_dir/'rpm', 'Ciao.spec', [
	    'Version' = Version,
	    'Release' = Release,
	    'VersionedPackName' = ~bundle_versioned_packname(Bundle),
	    'BinPackName' = ~bin_packname(Bundle),
	    'BundleMoveManVersions' = ~bundle_move_man_versions(Bundle),
	    'BundleMoveMans' = ~bundle_move_mans(Bundle),
	    'BundleIntegrateInfoindexes' = "",
	    'BundleInstallInfoCmds' = ~bundle_install_info_cmds(Bundle, ""),
	    'BundleInstallInfoCmdsRemove' = ~bundle_install_info_cmds(Bundle, "--remove"),
	    'BundleVersionedNames' = ~bundle_versioned_names(Bundle),
	    'BundleFiles' = ~bundle_files(Bundle),
	    'BundleNames' = ~bundle_name_defs(Bundle)
	|~bundle_vars(Bundle)]).

% Other keys to be filled in Ciao.spec
bundle_vars(Bundle) :=
	~findall(C,
	    ( bundle_version_var(Bundle, C)
	    ; bundle_path_version_var(Bundle, C)
	    )).

bundle_version_var(ParentBundle, Key = BundleVersion) :-
	enum_sub_bundles(ParentBundle, Bundle),
	'$bundle_prop'(Bundle, packname(PackName)),
	Key = ~atom_concat(PackName, 'Version'),
	BundleVersion = ~bundle_version_patch(Bundle).

bundle_path_version_var(ParentBundle, Key = BundlePathVersion) :-
	enum_sub_bundles(ParentBundle, Bundle),
	'$bundle_prop'(Bundle, packname(PackName)),
	Key = ~atom_concat(PackName, 'PathVersion'),
	BundlePathVersion = ~bundle_version(Bundle).

:- pred rpm_macrovalue(Macro, Value) # "RPM @var{Macro} is
   system-defined as @var{Value}.".

% Utilities to communicate Ciao installer with RPM macro system:
rpm_macrovalue(Macro, Value) :-
	MacroExpr = ~atom_concat(['%', Macro]),
	process_call(path(rpm), ['--eval', MacroExpr], [stdout(line(String))]),
	atom_codes(Value, String),
	% (returned) Value = (requested) %Macro would mean Value not defined
	Value \= MacroExpr. % So fail if macro not defined

:- pred rpmbuild_setoptions(RpmOptions, Bundle, RpmbuildArgs) # "
	@var{RpmbuildArgs} are the command-line arguments for
	@apl{rpmbuild} that will set the RPM generation options. Each
	option is set in the specification by defining the macro of
	the same name to its appropriate value (see @pred{map_rpmoptval/3}).
	".

rpmbuild_setoptions([], _Bundle, []).
rpmbuild_setoptions([option(Macro, Value)|L], Bundle, Args) :-
	map_rpmoptval(option(Macro, Value), Bundle, RpmValue),
	Args = ['--define', ~atom_concat([Macro, ' ', RpmValue])|Args0],
	rpmbuild_setoptions(L, Bundle, Args0).

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

:- pred map_rpmoptval(Option, Bundle, RpmValue) 
# "@var{RpmValue} is the value to use in the RPM specification for
   @var{Option}. It may just be @var{Option}'s straight value or a
   mapped representation if the RPM specification needs so.".

map_rpmoptval(option(Opt, Val), Bundle, MappedVal) :-
	ciaorpm_opttype(Opt, Bundle, Type),
	ciaorpm_mapvalue(Type, Val, MappedVal),
	!.
map_rpmoptval(option(_, Val), _Bundle, Val).

:- pred ciaorpm_opttype(OptName, Bundle, OptType)
# "This predicate declares the kind of some options (it is not meant
   to be exhaustive, only the ones whose value gets mapped need to be
   declared). @var{OptType} is the option type for an option named
   @var{OptName}.".

ciaorpm_opttype('subpackages',        _Bundle, 'rpm_expression').
ciaorpm_opttype('versionp',           _Bundle, 'rpm_expression').
ciaorpm_opttype('vendor_independent', _Bundle, 'rpm_expression').
ciaorpm_opttype(SubBundleName,        Bundle, 'rpm_expression') :-
	% TODO: Strange option! add something like "with_sub_bundle_BundleName"
	enum_sub_bundles(Bundle, SubBundleName). % Subpackage creation guards (see spec)
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

:- pred rpm_option_or_default(OptionName, DefaultValue, Option) #
   "@var{Option} is the RPM
   @tt{option(OptionName,Value)}. @var{DefaultValue} is used instead
   as the default fallback when @var{OptionName} was not defined by
   the user.".

rpm_option_or_default(OptName, _, option(OptName, OptValue)) :-
	bundle_param_value(_Bundle:OptName, OptValue), !.
rpm_option_or_default(OptName, DefValue, option(OptName, DefValue)).

% ===========================================================================
% Naming conventions for RPM files

rpm_file_name(Bundle, Packname, Arch) := RpmFileName :-
	get_rpm_version_and_release(Bundle, Version, Release),
	RpmFileName = ~atom_concat([Packname, '-', Version, '-', Release, '.', Arch, '.rpm']).

% Extract the version and release numbers from commit desc
% (usually version and patch)
% TODO: Move as part of bundle info!
get_rpm_version_and_release(Bundle, Version, Release) :-
        Version = ~bundle_version(Bundle),
        Desc = ~bundle_commit_info(Bundle, desc),
        ( atom_concat([Version, '-', Release0], Desc) ->
            % Replace '-' in release number (necessary for RPM)
            atom_codes(Release0, Release1),
            replace_char(Release1, 0'-, 0'., Release2),
            atom_codes(Release, Release2)
        ; % just in case the previous fails (it should not)
          Release = ~bundle_patch(Bundle)
        ).

replace_char([], _, _, []).
replace_char([A|As], A, B, [B|Bs]) :- !, replace_char(As, A, B, Bs).
replace_char([X|As], A, B, [X|Bs]) :- replace_char(As, A, B, Bs).

% ===========================================================================

:- doc(section, "Future Work (rpm)").

:- doc(bug, "Check and warn for unknown options or incorrect values
	(at this time if the user sets an invalid option it gets silently 
	ignored).").
:- doc(bug, "(subpackages=no ^ versionp=yes) should warn(incompatible)
and exit error.").
:- doc(bug, "Better compliance with @apl{rpmlint}").
:- doc(bug, "Check whether enabling mysql / java is feasible").
:- doc(bug, "Bugs from Emilio's email (debian package)").
:- doc(bug, "Eliminate PATHS warning issued by certain Ciao binaries").
:- doc(bug, "RPM-Ciao.bash should be replaced by Prolog code in this module").
:- doc(bug, "Review package descriptions").

