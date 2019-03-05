:- module(pbundle_generator, [],
     [assertions, basicmodes, nativeprops, regtypes, fsyntax, hiord]).

:- doc(title, "Packager").
:- doc(subtitle, "Generation of packaged bundles for distribution").

:- doc(module, "This module provides a mechanism for the generation of
   @concept{packaged bundle}s for distribution on different platforms
   and operating systems.").

:- use_module(engine(system_info), [get_os/1, get_arch/1]).
:- use_module(engine(runtime_control)).
:- use_module(library(aggregates)).
:- use_module(library(stream_utils)).
:- use_module(library(lists)).
:- use_module(library(llists)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(write)).
:- use_module(library(system), [file_exists/1]).
:- use_module(library(pathnames),
	[path_concat/3, path_get_relative/3, path_splitext/3]).
:- use_module(library(system_extra), [mkpath/1]).
:- use_module(library(source_tree), [current_file_find/3]).
:- use_module(engine(internals), ['$bundle_id'/1, '$bundle_prop'/2]).
:- use_module(library(bundle/bundle_info), [
	bundle_name/2,
	bundle_version/2]).
:- use_module(library(bundle/bundle_paths), [bundle_path/3, bundle_path/4]).
:- use_module(ciaobld(bundle_hash), [bundle_commit_info/3]).

:- use_module(ciaobld(messages_aux), [normal_message/2]).

% ---------------------------------------------------------------------------
% TODO: Only one distribution is valid currently (ciao_root)

:- use_module(ciaobld(builder_cmds), [bundle_share_workspace/2, root_target/1]).
:- use_module(engine(internals), [ciao_root/1]).

:- export(dist_main_bundle/2).
% Main bundle for a distribution (used to get versions, etc.)
dist_main_bundle(Target, MainBundle) :-
	root_target(Target),
	MainBundle = core. % TODO: ad-hoc

:- export(dist_pkgname/2).
% Name for distribution file
dist_pkgname(Target, PkgName) :-
	root_target(Target),
	PkgName = 'Ciao'. % TODO: ad-hoc

:- export(dist_name/2).
% Name of distribution
dist_name(Target, Name) :-
	root_target(Target),
	Name = 'ciao'. % TODO: ad-hoc

:- export(dist_workspace/2).
% Directory containing the distribution
dist_workspace(Target, Path) :-
	root_target(Target),
	ciao_root(Path). % TODO: ad-hoc

:- export(dist_versioned_pkgname/2).
% Name for distribution file, with version information (using
% COMMIT_DESC, which show descriptive names with the necessary data to
% uniquely identify stable and devel versions).
% TODO: Make it optional?
dist_versioned_pkgname(Target) := Name :-
	dist_pkgname(Target, PkgName),
	dist_main_bundle(Target, MainBundle),
	Name = ~atom_concat([PkgName, '-', ~bundle_commit_info(MainBundle, desc)]).

:- use_module(ciaobld(bundle_hash), [bundle_gen_commit_info/1]).
:- export(dist_gen_commit_info/1).
dist_gen_commit_info(Target) :-
	dist_main_bundle(Target, MainBundle),
	bundle_gen_commit_info(MainBundle).

:- export(dist_version/2).
% Version of the distribution (taken from MainBundle)
dist_version(Target) := Version :-
	dist_main_bundle(Target, MainBundle),
	Version = ~bundle_version(MainBundle).

:- export(dist_bundles/2).
% Bundles that are distributed by Target distribution
% (MainBundles and all the bundles from the same workspace, that are
% not marked with a NODISTRIBUTE)
dist_bundles(Target, Bundle) :-
	dist_main_bundle(Target, MainBundle),
	'$bundle_id'(Bundle),
	Dir = ~bundle_path(Bundle, '.'),
	% TODO: use 'directory mark' preds
	NoDist = ~path_concat(Dir, 'NODISTRIBUTE'),
	\+ file_exists(NoDist),
	%
	bundle_share_workspace(MainBundle, Bundle).

% ---------------------------------------------------------------------------

% (hooks for gen_pbundle)
:- include(ciaobld(pbundle_gen_hookdefs)).
% ---------------------------------------------------------------------------

% TODO: Add as external options
pbundle_codeitem_kind := tgz|rpm_x86|deb_x86|win|dmg.
% TODO: Simplify, extract from sub-bundles, etc.
% TODO: Missing some internal manuals, add them.
pbundle_docitem_kind := manual_html|manual_pdf.
% Sub-bundles whose documentation is distributed explicitly as a docitem
% TODO: ad-hoc, extract from Target, bundles, etc.
pbundle_manuals(Target, ciao, "Ciao Manual") :- root_target(Target).
pbundle_manuals(Target, ciaopp, "CiaoPP Manual") :- root_target(Target).
pbundle_manuals(Target, lpdoc, "LPdoc Manual") :- root_target(Target).

:- use_module(library(stream_utils), [output_to_file/2]).
:- use_module(library(version_strings), [version_split_patch/3]).

%:- export(pbundle_generate_meta/2).
% Generate the metadata file which describes the distribution
pbundle_generate_meta(Target, DescFile) :-
	pbundle_generate_meta_(Target, Desc),
	clauses_to_file(Desc, DescFile).

% :- export(pbundle_generate_meta_/2).
pbundle_generate_meta_(Target, Desc) :-
	findall(F, enum_pbundle_code_items(Target, F), Fs),
	findall(D, enum_pbundle_doc_items(Target, D), Ds),
	dist_pkgname(Target, PkgName),
	Version = ~dist_version(Target),
	version_split_patch(Version, VersionNopatch, VersionPatch),
	dist_main_bundle(Target, MainBundle),
	Desc = [% Dist information (most from MainBundle manifest or source)
                name = ~dist_name(Target),
                pkgname = PkgName,
		version = VersionNopatch,
		patch = VersionPatch,
		% Commit information
		commit_branch = ~bundle_commit_info(MainBundle, branch),
		commit_id = ~bundle_commit_info(MainBundle, id),
		commit_date = ~bundle_commit_info(MainBundle, date),
		commit_desc = ~bundle_commit_info(MainBundle, desc),
		% Items in this packaged bundle (after bundle build)
		docs = Ds,
		code = Fs].

% TODO: duplicated?
clauses_to_file(Desc, DescFile) :-
	output_to_file(clauses_to_file_(Desc), DescFile).

:- use_module(engine(runtime_control), [push_prolog_flag/2, pop_prolog_flag/1]). % TODO: find a better solution?

clauses_to_file_(Desc) :-
	push_prolog_flag(write_strings, on),
	portray_clauses(Desc),
	pop_prolog_flag(write_strings).

portray_clauses([]).
portray_clauses([X|Xs]) :-
	portray_clause(X),
	portray_clauses(Xs).

% Enumerate the pbundle items for code files
% TODO: See ciaobot_internals.sh:get_produced_pbundle_item_base
enum_pbundle_code_items(Target, Item) :-
	pbundle_codeitem_kind(CodeKind),
	pbundle_code_item(Target, CodeKind, Item).

% Enumerate the pbundle items for documentation
enum_pbundle_doc_items(Target, Item) :-
	% (nondet)
	pbundle_manuals(Target, ManualSuffix, ManualTitle), % TODO: ad-hoc
	pbundle_docitem_kind(PDocKind),
	pbundle_doc_item(Target, ManualSuffix, ManualTitle, PDocKind, Item).

% ---------------------------------------------------------------------------

% Obtain the pbundle_item description for the specified CodeKind
pbundle_code_item(Target, CodeKind, Item) :-
	pbundle_codeitem_kind_info(CodeKind, Ext0),
	PkgName = ~dist_pkgname(Target),
	dist_main_bundle(Target, MainBundle),
	Desc = ~bundle_commit_info(MainBundle, desc),
	( CodeKind = rpm_x86 ->
	    RPMDesc = ~fix_commit_desc_for_rpm(Desc),
	    PFile = ~atom_concat([PkgName, '-', RPMDesc, Ext0]),
	    PFileKind = i386_rpm
	; CodeKind = deb_x86 ->
	    RPMDesc = ~fix_commit_desc_for_rpm(Desc), % TODO: Use RPM scheme too?
	    PkgName2 = ~loweratom(PkgName),
	    atom_concat('.', Ext1, Ext0),
	    PFile = ~atom_concat([PkgName2, '_', RPMDesc, '_', Ext1]),
	    PFileKind = i386_deb
	; PFile = ~atom_concat([PkgName, '-', Desc, Ext0]),
	  ( pbundle_file_kind_ext(PFileKind, Ext),
	    atom_concat(_, Ext, PFile) ->
	      true
	  ; fail
	  )
	),
	Item = pbundle_item(PFileKind, "", PFile).

% TODO: Merge with code from ciaobot_internals.sh
% Take a commit desc TAG-N-HASH and generate TAG-N.HASH, where N.HASH
% will be the release number for RPM (no '-' is allowed there).
% TODO: Use TAG-N+HASH instead for debian? ('+' is more common)
% TODO: Use TAG+HASH-N for ubuntu?
fix_commit_desc_for_rpm(Desc) := RPMDesc :-
	atom_codes(Desc, Desc1),
	append(Tag, "-"||NHash, Desc1), append(N, "-"||Hash, NHash),
	!,
	append(N, "."||Hash, NHash2),
	append(Tag, "-"||NHash2, RPMDesc1),
	atom_codes(RPMDesc, RPMDesc1).
fix_commit_desc_for_rpm(Desc) := Desc.

% Obtain the pbundle_item description for the specified PDocKind and sub-bundle
pbundle_doc_item(Target, ManualSuffix, ManualTitle, PDocKind, Item) :-
	pbundle_doc_kind_ext(PDocKind, Ext),
	VersionedPkgName = ~dist_versioned_pkgname(Target), % TODO: weird
	atom_concat([VersionedPkgName, '_', ManualSuffix, Ext], PDoc),
	%
	Item = pbundle_item(PDocKind, ManualTitle, PDoc).

% ---------------------------------------------------------------------------

pbundle_doc_kind_ext(manual_pdf, '.pdf').
pbundle_doc_kind_ext(manual_html, '.html').

:- export(pbundle_file_kind_ext/2).
pbundle_file_kind_ext(tar_gz, '.tar.gz').
pbundle_file_kind_ext(i386_rpm, '.i386.rpm').
pbundle_file_kind_ext(i386_deb, '.i386.deb').
pbundle_file_kind_ext(windows, '.exe').
pbundle_file_kind_ext(macosx, '.dmg').

:- pred pbundle_codeitem_kind_info(Src, Ext) => atm * atm
   # "@var{Ext} is the extention of @var{Src}. @var{Src} is an allowed
      value of variable @tt{pbundle_codeitem_kind/1}".

pbundle_codeitem_kind_info(zip,     '.zip').
pbundle_codeitem_kind_info(gz,      '.gz').
pbundle_codeitem_kind_info(bz2,     '.bz2').
pbundle_codeitem_kind_info(tgz,     '.tar.gz').
pbundle_codeitem_kind_info(tbz,     '.tar.bz2').
pbundle_codeitem_kind_info(win,     '.exe').
pbundle_codeitem_kind_info(rpm_x86, '.i386.rpm').
pbundle_codeitem_kind_info(deb_x86, '.i386.deb').
pbundle_codeitem_kind_info(dmg,     '.dmg').

% ---------------------------------------------------------------------------

% TODO: merge with precomp_level in source_tree.pl
% TODO: This predicate is not used, but it should (at least in assertions)
:- export(pbundle_codeitem_type/1).
:- regtype pbundle_codeitem_type/1 # "The types of files that contains
code in a @index{packaged bundle}.".
pbundle_codeitem_type(src). % Source files
pbundle_codeitem_type(noa). % Platform independent binary files: Not Architecture
pbundle_codeitem_type(bin). % Binary files, including platform dependent files

% ---------------------------------------------------------------------------

pbundle_codeitem_type_suffix(src, '').
pbundle_codeitem_type_suffix(noa, '-noarch').
pbundle_codeitem_type_suffix(bin) := ~atom_concat(['-bin-', ~get_os, ~get_arch]).

% ---------------------------------------------------------------------------

% TODO: rename by pbundle_create_tarball?
:- export(gen_pbundle_common/3).
gen_pbundle_common(Target, PBundleType, Descs) :-
	VersionedPkgName = ~dist_versioned_pkgname(Target),
	normal_message("creating ~w archive for ~w ~w", [PBundleType, VersionedPkgName, Descs]),
	%
	SourceDir = ~dist_workspace(Target),
	findall(RelFile, get_file_list(PBundleType, Target, SourceDir, RelFile), Files),
	%
	TargetDir = ~pbundle_output_dir(Target),
	create_pbundle_output_dir(Target),
	build_pbundle_codeitems(Descs, SourceDir, TargetDir, VersionedPkgName, PBundleType, Files).

:- use_module(library(archive_files), [archive_files/4]).

build_pbundle_codeitems([], _SourceDir, _TargetDir, _VersionedPkgName, _PBundleType, _Files).
build_pbundle_codeitems([N|Ns], SourceDir, TargetDir, VersionedPkgName, PBundleType, Files) :-
	build_pbundle_codeitem(N, SourceDir, TargetDir, VersionedPkgName, PBundleType, Files),
	build_pbundle_codeitems(Ns, SourceDir, TargetDir, VersionedPkgName, PBundleType, Files).

build_pbundle_codeitem(Name, SourceDir, TargetDir, VersionedPkgName, PBundleType, Files) :-
	pbundle_codeitem_kind_info(Name, PBundleExtension),
	atom_concat([TargetDir, '/', VersionedPkgName, ~pbundle_codeitem_type_suffix(PBundleType), PBundleExtension], Archive),
	TopDir = VersionedPkgName,
	archive_files(SourceDir, Files, TopDir, Archive).

% TODO: replace PBundleType by precomp_level
% (nondet)
get_file_list(PBundleType, Target, BaseDir, RelFile) :-
	( % all distributable files (it does not enter bndls/)
	  current_file_find(distributable_precomp(PBundleType), BaseDir, File0)
	; % the BUNDLE_CATALOG mark at bndls/ % TODO: make it optional
	  CatalogMark = ~path_concat(BaseDir, 'bndls/BUNDLE_CATALOG'),
	  file_exists(CatalogMark),
	  File0 = CatalogMark
	; % all distributable bundles at bndls/
	  dist_bundles(Target, Bundle),
	  SourceDir = ~bundle_path(Bundle, '.'),
	  BndlsDir = ~path_concat(BaseDir, 'bndls'),
	  path_get_relative(BndlsDir, SourceDir, _),
	  current_file_find(distributable_precomp(PBundleType), SourceDir, File0)
	),
	\+ is_excluded(File0, PBundleType),
	path_get_relative(BaseDir, File0, RelFile).

is_excluded(File, noa) :-
	% Some noarch file of a module that contains foreign code
	% TODO: Does not work with CIAOCCACHE
	path_splitext(File, FileBase, Ext),
	( Ext = '.po'
	; Ext = '.itf'
	; Ext = '.asr'
	; Ext = '.ast'
	),
	a_filename(FileBase, FileA),
	file_exists(FileA).

:- use_module(engine(internals), [a_filename/2]).

% ---------------------------------------------------------------------------

% TODO: share implementation
loweratom(X0, X) :- atom_codes(X0, Cs0), lowercodes(Cs0, Cs), atom_codes(X, Cs).

lowercodes([], []).
lowercodes([X0|Xs0], [X|Xs]) :- lowercode(X0, X), lowercodes(Xs0, Xs).

lowercode(X0, X) :- X0 >= 0'A, X0 =< 0'Z, !, X is X0 - 0'A + 0'a.
lowercode(X, X).

% ===========================================================================

:- export(pbundle_output_dir/2).
% TODO: The definition of directory is repeated in ciaobot/SHARED
%       (PBUNDLE_BUILD_DIR). Share the definition.
pbundle_output_dir(Target) := Path :-
	dist_main_bundle(Target, MainBundle),
	Path = ~bundle_path(MainBundle, builddir, 'pbundle').

:- export(create_pbundle_output_dir/1).
:- pred create_pbundle_output_dir/1 # "Make sure that the directory
   where generated pbundle files are placed exists and has the
   NODISTRIBUTE mark.".

create_pbundle_output_dir(Target) :-
	Path = ~pbundle_output_dir(Target),
	mkpath(Path),
	% TODO: use 'directory mark' preds
	string_to_file("", ~path_concat(Path, 'NODISTRIBUTE')).

:- export(gen_pbundle_descfile/1).
% Generate the desc.tmpl file (see pbundle_meta)
gen_pbundle_descfile(Target) :-
	gen_pbundle_hook(descfile, Target, []).

% (hook)
% TODO: a real target?
gen_pbundle_hook(descfile, Target, _Options) :- !,
	create_pbundle_output_dir(Target),
	DescFile = ~path_concat(~pbundle_output_dir(Target), 'desc.tmpl'),
	pbundle_generate_meta(Target, DescFile).

