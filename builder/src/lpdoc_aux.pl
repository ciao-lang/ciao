:- module(_, [], [fsyntax, hiord, assertions, regtypes, isomodes]).

:- doc(title,  "Build documentation using LPdoc").
:- doc(author, "Jos@'{e} F. Morales").
:- doc(author, "The Ciao Development Team").

:- doc(module, "This is a wrapper around @apl{lpdoc} to build the
   documentation of a bundle. It uses @tt{lpdoc} as an external
   process so that no hard dependencies are introduced.").

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

:- use_module(engine(messages_basic), [message/2]).
:- use_module(library(system), [file_exists/1]).
:- use_module(library(system), [find_executable/2]).
:- use_module(library(source_tree), [copy_file_or_dir/2]).
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
	% TODO: currently a hardwired dependency for 'ascii' lpdoc backend, find a better solution
	find_executable(makeinfo,_),
	!,
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
build_docs_readme(_, _, _OutName) :-
	message(warning, ['Skipping update, check dependencies for the LPdoc ascii backend']).

:- export(build_docs_readme_html/3).
% Render README files as HTML (from .lpdoc to .html)
build_docs_readme_html(Bundle, SrcPath, OutName) :-
	ensure_builddir(Bundle, '.'), % TODO: needed?
	ensure_builddir(Bundle, 'doc'), % TODO: add as dep to 'initial doc' bundle?
	BundleDir = ~bundle_path(Bundle, '.'),
	path_concat(BundleDir, SrcPath, SrcPath2),
	DocDir = ~bundle_path(Bundle, builddir, 'doc'),
	invoke_lpdoc(['--allow_markdown=yes',
	              '--syntax_highlight=no',
	              '--html_layout=embedded',
	              ~atom_concat('--output_dir=', DocDir),
	              ~atom_concat('--output_name=', OutName),
		      '-t', 'html',
	              SrcPath2]).

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
	    invoke_lpdoc([
	        % '--doc_mainopts=versioned_output',
	        % '--allow_markdown=no',
	        % '--syntax_highlight=no',
	        ~atom_concat('--output_dir=', DocDir),
		'-t', 'all',
		AbsPath])
	; % Allow missing manuals (e.g., for NODISTRIBUTE content)
	  message(warning, ['Manual ', Path, ' is missing. Skipping build']) % TODO: error?
	).

% ---------------------------------------------------------------------------
% TODO: merge with help module?

:- export(show_doc/3).
% Show documentation for `Path` manual from given `Bundle`
% and format `DocFormat`. Do nothing if manual is not generated.
% TODO: may not work for installed bundles!
show_doc(Bundle, Path, DocFormat) :- % ManualBase
	BundleDir = ~bundle_path(Bundle, '.'),
	path_concat(BundleDir, Path, AbsPath),
	( file_exists(AbsPath) ->
	    DocDir = ~bundle_path(Bundle, builddir, 'doc'),
	    invoke_lpdoc([
                % '--doc_mainopts=versioned_output',
	        ~atom_concat('--output_dir=', DocDir),
		'-t', DocFormat,
		'--view', AbsPath])
	; % Allow missing manuals (e.g., for NODISTRIBUTE content)
	  % TODO: locate online?
	  message(warning, ['Manual ', Path, ' is missing.']) % TODO: error?
	).

% ---------------------------------------------------------------------------
% Manual paths

get_manual_name(ManualBase, DocFormat) := FileName :-
	DocExt = ~atom_concat('.', DocFormat),
	FileName = ~atom_concat(ManualBase, DocExt).

:- export(bld_manual_path/4).
% Path for manual in builddir
bld_manual_path(Bundle, ManualBase, DocFormat) := Path :-
	FileName = ~get_manual_name(ManualBase, DocFormat),
	DocDir = ~bundle_path(Bundle, builddir, 'doc'),
	Path = ~path_concat(DocDir, FileName).

% TODO: Move inst_* act_* somewhere else?

:- use_module(ciaobld(install_aux), [active_docdir/2, inst_builddir_path/2]).

:- export(inst_manual_path/4).
% Path for manual in instdir
inst_manual_path(_Bundle, ManualBase, DocFormat) := Path :-
	FileName = ~get_manual_name(ManualBase, DocFormat),
	TargetDir = ~inst_builddir_path('doc'),
	Path = ~path_concat(TargetDir, FileName).

:- export(act_manual_path/4).
% Path for manual in activation dir
act_manual_path(_Bundle, ManualBase, DocFormat) := Path :-
	FileName = ~get_manual_name(ManualBase, DocFormat),
	ActTargetDir = ~active_docdir(DocFormat),
	Path = ~path_concat(ActTargetDir, FileName).

