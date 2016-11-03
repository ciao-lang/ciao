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
:- use_module(library(pathnames), [path_concat/3, path_split/3]).
:- use_module(library(read), [read/2]).
:- use_module(library(messages), [warning_message/2]).

:- use_module(library(system), [file_exists/1, working_directory/2]).
:- use_module(library(streams), [open_output/2, close_output/1]).

:- use_module(engine(internals), [bundlereg_version/1]).

% ---------------------------------------------------------------------------

:- export(is_bundledir/1).
% @var{BundleDir} is the directory for a bundle (i.e., contains a
% Manifest.pl file)
is_bundledir(BundleDir) :-
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

:- export(lookup_bundle_root/2).
% Detect the bundle root dir for the given File (a directory or normal
% file)
lookup_bundle_root(File, BundleDir) :-
	fixed_absolute_file_name(File, '.', Path),
	lookup_bundle_root_(Path, BundleDir).

lookup_bundle_root_(File, BundleDir) :-
	is_bundledir(File),
	!,
	% Found a bundle dir
	BundleDir = File.
lookup_bundle_root_(File, BundleDir) :-
	% Not a bundle dir, visit the parent
	path_split(File, Base, Name),
	\+ (Base = '/', Name = ''),
	lookup_bundle_root_(Base, BundleDir).

% ---------------------------------------------------------------------------
% TODO: Use standard compiler? (with a package to delimit the language)

:- export(gen_bundlereg/4).
% Generate a bundlereg @var{RegFile} for bundle @var{BundleName} at
% @var{BundleDir}.  The base path for alias paths is @var{AliasBase}.
gen_bundlereg(BundleDir, BundleName, AliasBase, RegFile) :-
	% Load Manifest.pl
	locate_manifest_file(BundleDir, ManifestFile),
	loop_read_file(ManifestFile, Sents0),
	% Check that this is a manifest file
	( Sents0 = [(:- bundle(BundleName0))|Sents1] ->
	    true
	; BundleName0 = _,
	  warning_message("Missing ':- bundle/1' declaration on Manifest.pl for ~w", [BundleName])
	),
	% Fill version number
	read_bundle_version(BundleDir, BundleName, Sents1, Sents),
	% Check that bundle name matches declared
	( BundleName0 = BundleName -> % (unify if leaved undefined)
	    true
	; warning_message("Mismatch in bundle name (expected ~w)", [BundleName])
	),
	%
	( member(alias_paths(RelAliasPaths), Sents) ->
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
	fast_write(bundle_id(BundleName)),
        % Alias paths
	write_alias_paths(AliasPaths, BundleName),
	% TODO: write extra info in a separate file?
	( member(packname(Packname), Sents) ->
	    true
	; Packname = BundleName % reuse bundle name as packname
	),
	fast_write(bundle_prop(BundleName, packname(Packname))),
	( member(depends(Depends), Sents) ->
	    fast_write(bundle_prop(BundleName, depends(Depends)))
	; true
	),
	( member(version_(Version), Sents) ->
	    fast_write(bundle_prop(BundleName, version(Version)))
	; true
	),
	( member(patch_(Patch), Sents) ->
	    fast_write(bundle_prop(BundleName, patch(Patch)))
	; true
	),
	% TODO: relocate BundleDir for InsType=global? (not a good
	%   idea: hierarchy may not be preserved in installation
	%   change; use bundle-qualified alias paths?)
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

:- use_module(library(system_extra), [file_to_line/2]).
:- use_module(library(version_strings), [parse_version/3]).

read_bundle_version(_BundleDir, BundleName, Sents0, Sents) :-
	member(version(Ver), Sents0),
	!,
	( parse_version(Ver, Version, Patch) ->
	    Sents = [version_(Version), patch_(Patch)|Sents0]
	; warning_message("Cannot parse version number ~w for ~w", [Ver, BundleName]),
	  Sents = Sents0
	).
read_bundle_version(BundleDir, _BundleName, Sents0, Sents) :-
	% TODO: Deprecate
        path_concat(BundleDir, 'Manifest/GlobalVersion', FileV),
        path_concat(BundleDir, 'Manifest/GlobalPatch', FileP),
	( file_to_atom(FileV, Version),
	  file_to_atom(FileP, Patch) ->
	    Sents = [version_(Version), patch_(Patch)|Sents0] 
	; Sents = Sents0
	).

file_to_atom(File, X) :-
	file_exists(File),
	file_to_line(File, Str),
	atom_codes(X, Str).

