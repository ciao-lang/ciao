:- module(pbundle_generator, [],
     [assertions, basicmodes, nativeprops, regtypes, fsyntax, hiord]).

:- doc(title, "Packager").
:- doc(subtitle, "Generation of packaged bundles for distribution").

:- doc(module, "This module provides a mechanism for the generation of
   @concept{packaged bundle}s for distribution on different platforms
   and operating systems.").

:- use_module(library(aggregates)).
:- use_module(library(file_utils)).
:- use_module(library(lists)).
:- use_module(library(llists)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(write)).
:- use_module(library(system), [file_exists/1]).
:- use_module(library(pathnames),
	[path_concat/3, path_get_relative/3, path_splitext/3]).
:- use_module(library(system_extra), [mkpath/1]).
:- use_module(library(source_tree), [current_file_find/3]).
:- use_module(engine(internals), ['$bundle_prop'/2]).
:- use_module(library(bundle/bundle_info), [
	bundle_name/2,
	bundle_version/2, bundle_patch/2]).
:- use_module(library(bundle/paths_extra), [fsR/2]).
:- use_module(ciaobld(bundle_hash), [
	bundle_versioned_packname/2, bundle_commit_info/3]).

:- use_module(ciaobld(config_common), [local_bldid/1]).

:- use_module(ciaobld(messages_aux), [cmd_message/3]).

% (hooks for gen_pbundle)
:- include(ciaobld(pbundle_gen_hookdefs)).
% ---------------------------------------------------------------------------

% TODO: Add as external options
pbundle_codeitem_kind := tgz|rpm_x86|deb_x86|win|dmg.
% TODO: Simplify, extract from sub-bundles, etc.
% TODO: Missing some internal manuals, add them.
pbundle_docitem_kind := manual_html|manual_pdf.
% Sub-bundles whose documentation is distributed explicitly as a docitem
% TODO: extract from Bundle
bundle_doc_subbundles(ciao, core, ciao, "Ciao Manual").
bundle_doc_subbundles(ciao, ciaopp, ciaopp, "CiaoPP Manual").
bundle_doc_subbundles(ciao, lpdoc, lpdoc, "LPdoc Manual").

:- use_module(library(file_utils), [output_to_file/2]).

:- export(pbundle_generate_meta/2).
% Generate the metadata file which contains all the produced output of
% the bundle build process.
pbundle_generate_meta(Bundle, DescFile) :-
	pbundle_generate_meta_(Bundle, Desc),
	clauses_to_file(Desc, DescFile).

% :- export(pbundle_generate_meta_/2).
pbundle_generate_meta_(Bundle, Desc) :-
	findall(F, enum_pbundle_code_items(Bundle, F), Fs),
	findall(D, enum_pbundle_doc_items(Bundle, D), Ds),
	'$bundle_prop'(Bundle, packname(Packname)),
	Desc = [% Bundle information (from manifest or source)
                name = ~bundle_name(Bundle),
                packname = Packname,
		version = ~bundle_version(Bundle),
		patch = ~bundle_patch(Bundle),
		% Commit information
		commit_branch = ~bundle_commit_info(Bundle, branch),
		commit_id = ~bundle_commit_info(Bundle, id),
		commit_date = ~bundle_commit_info(Bundle, date),
		commit_desc = ~bundle_commit_info(Bundle, desc),
		% Items in this packaged bundle (after bundle build)
		docs = Ds,
		code = Fs].

clauses_to_file(Desc, DescFile) :-
	output_to_file(clauses_to_file_(Desc), DescFile).

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
enum_pbundle_code_items(Bundle, Item) :-
	pbundle_codeitem_kind(CodeKind),
	pbundle_code_item(Bundle, CodeKind, Item).

% Enumerate the pbundle items for documentation
enum_pbundle_doc_items(Bundle, Item) :-
	bundle_doc_subbundles(Bundle, SubBundle, SubBundleSuffix, SubBundleTitle),
	pbundle_docitem_kind(PDocKind),
	pbundle_doc_item(Bundle, SubBundle, SubBundleSuffix, SubBundleTitle, PDocKind, Item).

% ---------------------------------------------------------------------------

% Obtain the pbundle_item description for the specified CodeKind
pbundle_code_item(Bundle, CodeKind, Item) :-
	pbundle_codeitem_kind_info(CodeKind, Ext0),
	'$bundle_prop'(Bundle, packname(Packname)),
	Desc = ~bundle_commit_info(Bundle, desc),
	( CodeKind = rpm_x86 ->
	    RPMDesc = ~fix_commit_desc_for_rpm(Desc),
	    PFile = ~atom_concat([Packname, '-', RPMDesc, Ext0]),
	    PFileKind = i386_rpm
	; CodeKind = deb_x86 ->
	    RPMDesc = ~fix_commit_desc_for_rpm(Desc), % TODO: Use RPM scheme too?
	    Packname2 = ~loweratom(Packname),
	    atom_concat('.', Ext1, Ext0),
	    PFile = ~atom_concat([Packname2, '_', RPMDesc, '_', Ext1]),
	    PFileKind = i386_deb
	; PFile = ~atom_concat([Packname, '-', Desc, Ext0]),
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
pbundle_doc_item(Bundle, _SubBundle, SubBundleSuffix, SubBundleTitle, PDocKind, Item) :-
	pbundle_doc_kind_ext(PDocKind, Ext),
	VersionedPackName = ~bundle_versioned_packname(Bundle),
	atom_concat([VersionedPackName, '_', SubBundleSuffix, Ext], PDoc),
	%
	Item = pbundle_item(PDocKind, SubBundleTitle, PDoc).

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
gen_pbundle_common(Bundle, PackType, Descs) :-
	VersionedPackName = ~bundle_versioned_packname(Bundle),
	cmd_message(Bundle, "creating ~w archive for ~w ~w ...", [PackType, VersionedPackName, Descs]),
	%
	SourceDir = ~fsR(bundle_src(Bundle)),
	findall(RelFile, get_file_list(PackType, SourceDir, RelFile), Files),
	%
	create_pbundle_output_dir,
	TargetDir = ~pbundle_output_dir,
	build_pbundle_codeitems(Descs, SourceDir, TargetDir, VersionedPackName, PackType, Files).

:- use_module(library(archive_files), [archive_files/4]).

build_pbundle_codeitems([], _SourceDir, _TargetDir, _VersionedPackName, _PBundleType, _Files).
build_pbundle_codeitems([N|Ns], SourceDir, TargetDir, VersionedPackName, PBundleType, Files) :-
	build_pbundle_codeitem(N, SourceDir, TargetDir, VersionedPackName, PBundleType, Files),
	build_pbundle_codeitems(Ns, SourceDir, TargetDir, VersionedPackName, PBundleType, Files).

build_pbundle_codeitem(Name, SourceDir, TargetDir, VersionedPackName, PBundleType, Files) :-
	pbundle_codeitem_kind_info(Name, PBundleExtension),
	atom_concat([TargetDir, '/', VersionedPackName, ~pbundle_codeitem_type_suffix(PBundleType), PBundleExtension], Archive),
	TopDir = VersionedPackName,
	archive_files(SourceDir, Files, TopDir, Archive).

% TODO: replace PBundleType by precomp_level
% (nondet)
get_file_list(PBundleType, SourceDir, RelFile) :-
	current_file_find(distributable_precomp(PBundleType), SourceDir, File0),
	\+ is_excluded(File0, PBundleType),
	path_get_relative(SourceDir, File0, RelFile).

is_excluded(File, noa) :-
	% Some noarch file of a module that contains foreign code
	% TODO: Does not work with CIAOCACHEDIR
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

% The directory where @apl{ciao_builder} code is found
:- export(builder_src_dir/1).
builder_src_dir := bundle_src(builder)/'src'.

% ===========================================================================

:- export(pbundle_output_dir/1).
% TODO: The definition of directory is repeated in ciaobot/SHARED
%       (PBUNDLE_BUILD_DIR). Share the definition.
pbundle_output_dir := ~fsR(builddir(~local_bldid)/'pbundle').

:- export(create_pbundle_output_dir/0).
:- pred create_pbundle_output_dir # "Make sure that the directory
   where generated pbundle files are placed exists and has the
   NODISTRIBUTE mark.".

create_pbundle_output_dir :-
	TargetDir = ~pbundle_output_dir,
	mkpath(TargetDir),
	% TODO: use 'directory mark' preds
	string_to_file("", ~path_concat(TargetDir, 'NODISTRIBUTE')).

:- export(relciaodir/2).
relciaodir(S) := Dir :-
	R = ~fsR(bundle_src(ciao)),
	path_get_relative(R, S, Dir).

:- export(gen_pbundle_descfile/1).
% Generate the desc.tmpl file (see pbundle_meta)
gen_pbundle_descfile(Bundle) :-
	gen_pbundle_hook(descfile, Bundle, []).

% (hook)
% TODO: a real target?
gen_pbundle_hook(descfile, Bundle, _Options) :- !,
	TargetDir = ~pbundle_output_dir,
	DescFile = ~path_concat(TargetDir, 'desc.tmpl'),
	pbundle_generate_meta(Bundle, DescFile).

