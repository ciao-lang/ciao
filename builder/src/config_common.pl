:- module(config_common, [], [assertions, fsyntax]).

% TODO: rename by builder_opts or flags?
:- doc(title, "Commonly used config flags").
:- doc(author, "Ciao Development Team").
:- doc(author, "Jose F. Morales").

:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(pathnames), [path_concat/3]).

:- use_module(library(bundle/bundle_paths), [bundle_path/4]).
:- use_module(library(bundle/bundle_flags), [get_bundle_flag/2]).

% ===========================================================================

:- export(verbose_build/1).
verbose_build := ~get_bundle_flag(builder:verbose_build).

% ===========================================================================

:- use_module(engine(internals), ['$bundle_id'/1]).

:- export(with_docs/1).
% Enable docs grade (lpdoc must be available)
with_docs := ~get_bundle_flag(builder:with_docs) :- !, '$bundle_id'(lpdoc).
with_docs := no.

% TODO: Those are default formats for installation; use a bundle flag instead
:- export(docformat/1).
docformat := pdf|manl|info|html. % | ps.

% ===========================================================================

% Default engine
% TODO: Generalize a-la optim_comp for arbitrary native code execs
% TODO: Generalize with options, flags, etc.
:- export(default_eng_def/1).
default_eng_def(Eng) :-
	Eng = eng_def(core, 'engine/ciaoengine', []).

% Boot engine
:- export(boot_eng_def/1).
boot_eng_def(Eng) :-
	Eng = eng_def(core, 'engine/ciaoengine', [boot]).

% ===========================================================================
% TODO: Move to ciaoc_aux?

:- export(cmd_path/4).
% Path for executable commands in the build area
% (e.g., build/bin/<CMD>)
cmd_path(Bundle, Kind, File) := Path :-
	BinDir = ~bundle_path(Bundle, builddir, 'bin'),
	Path = ~path_concat(BinDir, ~concat_ext(Kind, File)).

:- export(libcmd_path/4).
% Path for libexec commands in the build area (not intented to be run
% directly by humans, e.g., like active modules)
% (e.g., build/libexec/<CMD>)
libcmd_path(Bundle, Kind, File) := Path :-
	BinDir = ~bundle_path(Bundle, builddir, 'libexec'),
	Path = ~path_concat(BinDir, ~concat_ext(Kind, File)).

:- export(concat_ext/3).
% Obtain 'A'+'Ext' where Ext is the default extension (For the current
% architecture) for each kind:
%
%  - 'plexe': ciao executables
%  - 'exec': native executables
%  - 'shscript': shell scripts
%  - 'ext(Ext)': custom extension 'Ext'
%
concat_ext(plexe, X) := ~atom_concat(X, ~get_ciao_ext).
concat_ext(exec, X) := ~atom_concat(X, ~get_exec_ext).
concat_ext(shscript, X) := X.
concat_ext(ext(Ext), X) := ~atom_concat(X, Ext).

% ===========================================================================
% TODO: document somewhere the directories under build/

:- export(site_root_dir/1).
% Files for HTTP serving
site_root_dir := Path :-
	Bundle = core, % TODO: allow any bundle
	Path = ~bundle_path(Bundle, builddir, 'site').

:- export(data_root_dir/1).
% Files for per-bundle persistence
data_root_dir := Path :-
	Bundle = core, % TODO: allow any bundle
	Path = ~bundle_path(Bundle, builddir, 'data').
