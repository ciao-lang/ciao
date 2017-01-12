:- module(_, [], [fsyntax, hiord, assertions, regtypes, isomodes]).

:- doc(title,  "Extended interface to LPdoc").

:- doc(author, "Jos@'{e} F. Morales").
:- doc(author, "Ciao Deveveloper Team").

:- doc(module, "This is a wrapper around @apl{lpdoc} to build and
   install the documentation of a bundle.").

% ---------------------------------------------------------------------------

:- use_module(ciaobld(config_common), [verbose_build/1]).
:- use_module(ciaobld(config_common), [cmd_path/4]).
:- use_module(ciaobld(cpx_process), [cpx_process_call/3]).

lpdoc_exec := ~cmd_path(lpdoc, plexe, 'lpdoc').

% TODO: make lpdoc verbose message more descriptive? remove message
%   here (e.g., show basename of directory containing SETTINGS)
:- export(invoke_lpdoc/1).
invoke_lpdoc(Args) :-
	( verbose_build(yes) -> Args2 = ['-v'|Args] ; Args2 = Args ),
	cpx_process_call(~lpdoc_exec, Args2, []).

% ---------------------------------------------------------------------------

:- use_module(library(system), [file_exists/1]).
:- use_module(library(pathnames), [path_concat/3, path_split/3]).
:- use_module(library(pathnames), [path_splitext/3]).
:- use_module(library(bundle/bundle_paths), [
    bundle_path/3, bundle_path/4
]).
:- use_module(ciaobld(builder_aux), [ensure_builddir/2]).

:- export(build_docs_readme/3).
% Creation of README files (from .lpdoc to ascii)
% Output is moved to the bundle root directory.
build_docs_readme(Bundle, SrcPath, OutName) :-
	ensure_builddir(Bundle, '.'), % TODO: needed?
	ensure_builddir(Bundle, 'doc'), % TODO: add as dep to 'initial doc' bundle?
	OutAbsFile = ~bundle_path(Bundle, OutName),
	path_split(SrcPath, _, Name),
	path_splitext(Name, Name0, _),
	BundleDir = ~bundle_path(Bundle, '.'),
	path_concat(BundleDir, SrcPath, SrcPath2),
	DocDir = ~bundle_path(Bundle, builddir, 'doc'),
	invoke_lpdoc(['--autogen_warning=yes',
	              '--allow_markdown=no',
	              '--syntax_highlight=no',
	              ~atom_concat('--output_dir=', DocDir),
		      '-t', 'ascii',
	              SrcPath2]),
	Ascii = ~atom_concat(Name0, '.ascii'),
	DocSrc = ~path_concat(DocDir, Ascii),
	copy_file_or_dir(DocSrc, OutAbsFile).

:- export(build_doc/2).
% TODO: build does not read format, install does, it is inconsistent
% Build the manual `SrcDir` for `Bundle`
build_doc(Bundle, Path) :-
	ensure_builddir(Bundle, '.'), % TODO: needed?
	ensure_builddir(Bundle, 'doc'), % TODO: add as dep to 'initial doc' bundle?
	BundleDir = ~bundle_path(Bundle, '.'),
	path_concat(BundleDir, Path, AbsPath),
	( file_exists(AbsPath) ->
	    DocDir = ~bundle_path(Bundle, builddir, 'doc'),
	    invoke_lpdoc(['--doc_mainopts=versioned_output',
%	                  '--allow_markdown=no',
%	                  '--syntax_highlight=no',
	                  ~atom_concat('--output_dir=', DocDir),
	                  '-t', 'all',
			  AbsPath])
	; % Allow missing manuals (e.g., for NODISTRIBUTE content)
	  warning(['Manual ', Path, ' is missing. Skipping build']) % TODO: error?
	).

% ---------------------------------------------------------------------------

:- use_module(library(system), [file_exists/1]).
:- use_module(library(source_tree),
	[copy_file_or_dir/2, remove_file_or_dir/1]).
:- use_module(library(system_extra), [mkpath/1]).
:- use_module(ciaobld(builder_aux), [rootprefixed/2]).

:- use_module(library(bundle/doc_flags), [docformatdir/2]).

:- export(install_doc/3).
% Install manual `ManualBase` (name and version) from given `Bundle`
% and format `DocFormat`. Do nothing if manual is not generated.
% NOTE: needed even if ~instype = local (see bundle_install_docs_format_hook/3)
install_doc(Bundle, ManualBase, DocFormat) :-
	docformatdir(DocFormat, TargetDir0),
	TargetDir = ~rootprefixed(TargetDir0),
	DocExt = ~atom_concat('.', DocFormat),
	FileName = ~atom_concat(ManualBase, DocExt),
	DocDir = ~bundle_path(Bundle, builddir, 'doc'),
	Source = ~path_concat(DocDir, FileName),
	Target = ~path_concat(TargetDir, FileName),
	( file_exists(Source) ->
	    % Copy if needed
	    ( Source == Target -> % (typically when ~instype = local)
	        true
	    ; mkpath(TargetDir),
	      copy_file_or_dir(Source, TargetDir)
	    ),
	    % Register doc (if needed)
	    bundle_install_docs_format_hook(DocFormat, Bundle, Target)
	; true % warning(['File ', Source, ' not generated yet. Skipping copy'])
	).

:- export(uninstall_doc/3).
% Uninstall manual `ManualBase` (name and version) from given `Bundle`
% and format `DocFormat`. Do nothing if manual is not generated.
% NOTE: needed even if ~instype = local (see bundle_uninstall_docs_format_hook/3)
uninstall_doc(Bundle, ManualBase, DocFormat) :-
	docformatdir(DocFormat, TargetDir0),
	TargetDir = ~rootprefixed(TargetDir0),
	DocExt = ~atom_concat('.', DocFormat),
	FileName = ~atom_concat(ManualBase, DocExt),
	DocDir = ~bundle_path(Bundle, builddir, 'doc'),
	Source = ~path_concat(DocDir, FileName),
	Target = ~path_concat(TargetDir, FileName),
	( file_exists(Target) ->
	    % Unregister doc (if needed)
	    bundle_uninstall_docs_format_hook(DocFormat, Bundle, Target),
	    % Remove if needed
	    ( Source == Target -> % (typically when ~instype = local)
	        true
	    ; remove_file_or_dir(Target)
	    )
	; true
	).

% These predicates install the 'info' files in info dir.

:- use_module(ciaobld(info_installer)).

bundle_install_docs_format_hook(info, Bundle, Target) :- !,
	DocDir = ~bundle_path(Bundle, builddir, 'doc'),
	dirfile_install_info(DocDir, Target).
bundle_install_docs_format_hook(_, _, _).

bundle_uninstall_docs_format_hook(info, Bundle, Target) :- !,
	DocDir = ~bundle_path(Bundle, builddir, 'doc'),
	dirfile_uninstall_info(DocDir, Target).
bundle_uninstall_docs_format_hook(_, _, _).

% ---------------------------------------------------------------------------
% TODO: merge with help module?

:- export(show_doc/3).
% Show documentation for `ManualBase` (name and version) from given `Bundle`
% and format `DocFormat`. Do nothing if manual is not generated.
% TODO: may not work for installed bundles!
show_doc(Bundle, Path, DocFormat) :- % ManualBase
	BundleDir = ~bundle_path(Bundle, '.'),
	path_concat(BundleDir, Path, AbsPath),
	( file_exists(AbsPath) ->
	    DocDir = ~bundle_path(Bundle, builddir, 'doc'),
	    invoke_lpdoc(['--doc_mainopts=versioned_output',
	                  ~atom_concat('--output_dir=', DocDir),
	                  '-t', DocFormat,
			  '--view', AbsPath])
	; % Allow missing manuals (e.g., for NODISTRIBUTE content)
	  % TODO: locate online?
	  warning(['Manual ', Path, ' is missing.']) % TODO: error?
	).
        % TODO: for installed manuals:
	%   docformatdir(DocFormat, TargetDir),
	%   DocExt = ~atom_concat('.', DocFormat),
	%   FileName = ~atom_concat(ManualBase, DocExt),
	%   Target = ~path_concat(TargetDir, FileName),

