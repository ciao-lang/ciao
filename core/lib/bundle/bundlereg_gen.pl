:- module(bundlereg_gen, [], [assertions]).

:- doc(title, "Compile bundlereg from Manifest").
:- doc(author, "Ciao Development Team").
:- doc(author, "Jose F. Morales").

:- doc(module, "Processing of @tt{Manifest.pl} files into bundle
   registry entries. Bundle registries are written using @lib{fastrw}
   so that the support code to load them is minimal.

@begin{alert}
  Make sure that this code does not end in executables by default,
  unless necessary (it should not be in the dependencies of usual user
  programs).
@end{alert}").

:- use_module(library(fastrw), [fast_write/1]).
:- use_module(library(pathnames), [path_concat/3]).
:- use_module(library(read), [read/2]).
:- use_module(library(messages), [warning_message/2]).

:- use_module(library(system), [file_exists/1]).
:- use_module(library(streams), [open_output/2, close_output/1]).

:- use_module(engine(internals), [bundlereg_version/1]).

% ---------------------------------------------------------------------------

:- export(is_bundle_dir/1).
% @var{BundleDir} is the directory for a bundle (i.e., contains a
% Manifest.pl file)
is_bundle_dir(BundleDir) :-
	locate_manifest_file(BundleDir, _).

:- export(locate_manifest_file/2).
% Given @var{BundleDir} a bundle directory, @var{ManifestFile}
% is unified with the name of the manifest file.
locate_manifest_file(BundleDir, ManifestFile) :-
	( BundleDir2 = BundleDir
	; path_concat(BundleDir, 'Manifest', BundleDir2)
	),
	path_concat(BundleDir2, 'Manifest.pl', ManifestFile),
	file_exists(ManifestFile),
	!.

% ---------------------------------------------------------------------------

:- export(gen_bundlereg/4).
% Generate a bundlereg @var{RegFile} for bundle @var{BundleName} at
% @var{BundleDir}.  The base path for alias paths is @var{AliasBase}.
gen_bundlereg(BundleDir, BundleName, AliasBase, RegFile) :-
	% Load Manifest.pl (and optionally version from version/)
	locate_manifest_file(BundleDir, ManifestFile),
	loop_read_file(ManifestFile, ManifestSents0),
	read_bundle_version(BundleDir, ManifestSents0, ManifestSents),
	%
	% Check that bundle name matches declared
	( member(bundle_name(BundleName0), ManifestSents),
	  BundleName0 == BundleName ->
	    true
	; warning_message("Mismatch in bundle name (expected ~w)", [BundleName])
	),
	%
	( member_chk(bundle_alias_paths(RelAliasPaths), BundleDir, ManifestSents) ->
	    true
	; RelAliasPaths = []
	),
	%
	abs_alias_paths(RelAliasPaths, AliasBase, AliasPaths),
	%
	open_output(RegFile, UO),
	%
	% Version of bundlereg
	bundlereg_version(V),
	fast_write(bundlereg_version(V)),
        % Alias paths
	write_alias_paths(AliasPaths, BundleName),
	% TODO: write extra info in a separate file?
	member_chk(bundle_packname(Packname), BundleDir, ManifestSents),
	fast_write(bundle_id(BundleName)),
	fast_write(bundle_prop(BundleName, packname(Packname))),
	( member(bundle_requires(Requires), ManifestSents) ->
	    fast_write(bundle_prop(BundleName, requires(Requires)))
	; true
	),
	( member(bundle_version(Version), ManifestSents) ->
	    fast_write(bundle_prop(BundleName, version(Version)))
	; true
	),
	( member(bundle_patch(Patch), ManifestSents) ->
	    fast_write(bundle_prop(BundleName, patch(Patch)))
	; true
	),
	% TODO: relocate BundleDir for InsType=global?
	fast_write(bundle_srcdir(BundleName, BundleDir)),
	%
	close_output(UO).

write_alias_paths([], _Bundle).
write_alias_paths([X|Xs], Bundle) :-
	write_alias_path(X, Bundle),
	write_alias_paths(Xs, Bundle).

write_alias_path(AliasName=AliasPath, Bundle) :- !,
	fast_write(bundle_alias_path(AliasName, Bundle, AliasPath)).
write_alias_path(AliasPath, Bundle) :-
	fast_write(bundle_alias_path(library, Bundle, AliasPath)).

% Compute absolute paths names for alias paths (add @var{Base} to the
% beginning of each relative alias path).
abs_alias_paths([], _, []).
abs_alias_paths([RelAliasPath|RelAliasPaths], Base, [AliasPath|AliasPaths]) :-
	abs_alias_path(RelAliasPath, Base, AliasPath),
	abs_alias_paths(RelAliasPaths, Base, AliasPaths).

abs_alias_path(X, Base, Y) :-
	( X = (AliasName=RelAliasPath) -> Y = (AliasName=AliasPath)
	; X = RelAliasPath, Y = AliasPath
	),
	( RelAliasPath = '.' ->
	    AliasPath = Base
	; path_concat(Base, RelAliasPath, AliasPath)
	).

% ---------------------------------------------------------------------------

% Load a Manifest.pl file
loop_read_file(FileName, Xs) :-
	open(FileName, read, S),
	loop_read(S, Xs),
	close(S).

loop_read(S, [X|Xs]) :-
	read(S, X),
	X \== end_of_file,
	!,
	loop_read(S, Xs).
loop_read(_S, []).

member_chk(Item, BundleDir, Sents) :-
	( member(Item, Sents) -> true
	; warning_message("Item ~w not found in bundle Manifest at ~w",
            [Item, BundleDir])
	).

% ---------------------------------------------------------------------------
% Load bundle version/patch from GlobalVersion,GlobalPath files if not
% provided in Manifest

% TODO: Deprecate in the future?

:- use_module(library(system_extra), [file_to_line/2]).

read_bundle_version(_BundleDir, ManifestSents0, ManifestSents) :-
	( member(bundle_version(_), ManifestSents0)
	; member(bundle_patch(_), ManifestSents0)
	),
	!,
	ManifestSents = ManifestSents0.
read_bundle_version(BundleDir, ManifestSents0, ManifestSents) :-
        path_concat(BundleDir, 'Manifest/GlobalVersion', FileV),
        path_concat(BundleDir, 'Manifest/GlobalPatch', FileP),
	( file_to_atom(FileV, Version),
	  file_to_atom(FileP, Patch) ->
	    ManifestSents = [
              bundle_version(Version), bundle_patch(Patch)|ManifestSents0
            ]
	; ManifestSents = ManifestSents0
	).

file_to_atom(File, X) :-
	file_exists(File),
	file_to_line(File, Str),
	atom_codes(X, Str).


