:- module(_, [], [assertions, basicmodes, nativeprops, fsyntax, hiord, regtypes]).

:- doc(title,  "Bundle Hashing").
:- doc(author, "Jos@'{e} F. Morales").
:- doc(author, "Ciao Developer Team").

:- doc(module, "This module defines the operations to map the whole
   bundle source code to shorter identifiers (which can be precise
   like cryptohashes for DVCS or coarse like release date, etc.).

   @begin{note}
   @bf{Note:} Some functions (like Git commit number) simply ignore
   uncommited changes in the source tree.
   @end{note}
").

:- doc(bug, "Do hashing of distributions instead of bundles?").
:- doc(bug, "Hashing relies on Git or SVN identifiers of the whole
   repository, not the specified bundle").
:- doc(bug, "Allow hashing with configuration options?").
:- doc(bug, "Allow hashing of binaries or distributions?").

:- use_module(engine(data_facts)).
:- use_module(library(stream_utils), [string_to_file/2]).
:- use_module(library(system), [file_exists/1]).
:- use_module(library(system_extra), [file_to_line/2]).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(pathnames), [path_concat/3]).

:- use_module(engine(internals), ['$bundle_prop'/2]).
:- use_module(library(bundle/bundle_paths), [bundle_path/3, bundle_workspace/2]).
:- use_module(library(bundle/bundle_info), [bundle_version/2]).

% ===========================================================================
% Extract and save commit information about the bundle source

:- export(bundle_gen_commit_info/1).
:- pred bundle_gen_commit_info/1 # "Extract and save the commit information metadata".
bundle_gen_commit_info(Bundle) :-
	save_bundle_commit_info(Bundle, branch),
	save_bundle_commit_info(Bundle, id),
	save_bundle_commit_info(Bundle, date),
	save_bundle_commit_info(Bundle, desc).

save_bundle_commit_info(Bundle, Field) :-
	bundle_commit_info(Bundle, Field, X),
	commit_info_file(Bundle, Field, FieldFile),
	string_to_file(~atom_codes(X), FieldFile).

% COMMIT_ID: Git commit id or SVN revision number (0 if everything else fails)
% COMMIT_BRANCH: branch name (ignored in SVN at this moment)
% COMMIT_DATE: Git commit date (ignored for SVN)
% COMMIT_DESC: human-readable description of the commit (including
%   version, patch, branch, etc. if available)

% TODO: also changed in ciaobot code
% TODO: per bundle or per workspace (current)?
%:- export(commit_info_file/3).
commit_info_file(Bundle, Field) := R :-
	File = ~commit_info_file_(Field),
	R = ~path_concat(~bundle_path(Bundle, 'Manifest'), File).

commit_info_file_(branch) := 'COMMIT_BRANCH'.
commit_info_file_(id) := 'COMMIT_ID'.
commit_info_file_(date) := 'COMMIT_DATE'.
commit_info_file_(desc) := 'COMMIT_DESC'.


:- data bundle_commit_info_db/3.

:- export(bundle_commit_info/3).
% Get bundle information (directly from the repository or from the
% saved commit info files).
bundle_commit_info(Bundle, Field, Value) :-
	( current_fact(bundle_commit_info_db(Field, Bundle, Value0)) ->
	    true
	; bundle_commit_info_(Bundle, Field, Value0),
	  assertz_fact(bundle_commit_info_db(Field, Bundle, Value0))
	),
	Value = Value0.

bundle_commit_info_(Bundle, Field, Value) :-
	bundle_repo_kind(Bundle, RepoKind),
	bundle_commit_info__(Field, RepoKind, Bundle, Value).

bundle_commit_info__(Field, svn, Bundle, Value) :-
	svn_commit_info(Field, Bundle, Value0),
	!,
	Value = Value0.
bundle_commit_info__(Field, git, Bundle, Value) :-
	git_commit_info(Field, Bundle, Value0),
	!,
	Value = Value0.
bundle_commit_info__(Field, none, Bundle, Date) :-
	commit_info_file(Bundle, Field, File),
	file_exists(File),
	!,
	Date0 = ~file_to_line(File),
	atom_codes(Date, Date0).
bundle_commit_info__(_, _, _Bundle, 'Unknown'). % TODO: throw exception instead?

% The kind of repository (git, svn, none)
% :- export(bundle_repo_kind/2).
bundle_repo_kind(Bundle, RepoKind) :-
	bundle_svn_repo_dir(Bundle, _),
	!,
	RepoKind = svn.
bundle_repo_kind(Bundle, RepoKind) :-
	bundle_git_repo_dir(Bundle, _),
	!,
	RepoKind = git.
bundle_repo_kind(_, none).

% ---------------------------------------------------------------------------
% Extract commit information (SVN)

:- use_module(library(vcs/vcs_svn)).

svn_commit_info(branch, _Bundle, Branch) :-
	% TODO: (ignored for SVN)
	Branch = ''.
svn_commit_info(id, Bundle, Id) :-
	% TODO: This is incorrect
	% Note: svnversion is computed only over Manifest/ directory (to make it faster)
	Path = ~bundle_path(Bundle, 'Manifest'),
	( Id = ~svn_get_revision(Path) -> true
	; throw(error_msg("Cannot get revision number (svn_get_revision/2 failed).", []))
	),
	Id \== 'exported',
	!.
svn_commit_info(date, Bundle, Date) :-
	svn_commit_info(id, Bundle, Rev),
	SvnRepository = ~svn_repository_root(~bundle_svn_repo_dir(Bundle)),
	\+ SvnRepository = '',
	Date0 = ~svn_revision_date(SvnRepository, Rev),
	!,
	atom_codes(Date, Date0).
svn_commit_info(desc, Bundle, Desc) :-
	Version = ~bundle_version(Bundle),
	( svn_commit_info(id, Bundle, Rev) ->
	    Desc = ~atom_concat([Version, '-', Rev])
	; Desc = Version
	).

bundle_svn_repo_dir(Bundle, Path) :-
	( Path0 = ~bundle_path(Bundle, '.'),
	  svn_repo_at_dir(Path0) ->
	    Path = Path0
	; Path0 = ~bundle_workspace(Bundle),
	  svn_repo_at_dir(Path0) ->
	    Path = Path0
	; fail
	).

% ---------------------------------------------------------------------------
% Extract commit information (Git)

:- use_module(library(version_strings), [version_split_patch/3]).
:- use_module(library(vcs/vcs_git)).

git_commit_info(branch, Bundle, Branch) :-
	bundle_git_output(Bundle, ['rev-parse', '--abbrev-ref', 'HEAD'], Branch1),
	!,
	( Branch1 = 'HEAD' ->
	    Branch = '' % Detached HEAD in Git repository!
	; Branch = Branch1
	).
git_commit_info(id, Bundle, Id) :-
%	bundle_git_output(Bundle, ['show-ref', '--heads', '-s'], Id0),
	bundle_git_output(Bundle, ['log', '-1', '--format=%H'], Id0),
	!,
	Id = Id0.
git_commit_info(short_id, Bundle, Id) :- % (Not stored)
	bundle_git_output(Bundle, ['log', '-1', '--format=%h'], Id0),
	!,
	Id = Id0.
git_commit_info(date, Bundle, Date) :-
	% Note: use ISO 8601 format for date (necessary for pbundle_meta_time/2)
	bundle_git_output(Bundle, ['log', '-1', '--format=%ci'], Date0),
	!,
	Date = Date0.
git_commit_info(desc, Bundle, Desc) :-
	% Describe the commit (see 'git describe')
	( bundle_git_output(Bundle, ['describe', '--tags'], Desc0) ->
	    Desc1 = Desc0
	; Desc1 = ''
	),
	( Desc1 = '' ->
	    % Create our own human-readable commit description using the
	    % branch name (this should not be needed if we have proper
	    % tags in our commit graph).
	    git_commit_info(short_id, Bundle, ShortId),
	    git_commit_info(branch, Bundle, Branch),
	    Desc2 = ~atom_concat([Branch, '-g', ShortId])
	; Desc2 = Desc1
	),
        % Fix COMMIT_DESC (this assumes that we may include version
        % and patch in tag and branch names, and removes redundant
        % information)
	Version = ~bundle_version(Bundle),
	version_split_patch(Version, VersionNopatch, _),
	( % Version in COMMIT_DESC, let us assume that is a
	  % particular code release (a commit with a tag, not a branch).
	  ( atom_concat(['v', Version, _], Desc2)
	  ; atom_concat([Version, _], Desc2)
	  ) ->
	    Desc = Version
	; % VersionNopatch in COMMIT_DESC, just use it (it is a
	  % develoment release).
	  atom_concat(['v', VersionNopatch, _], Desc2) ->
	    atom_concat('v', Desc, Desc2) % (remove 'v')
	; atom_concat([VersionNopatch, _], Desc2) ->
	    Desc = Desc2
	; % No version info in commit desc, just append it (also a
	  % development release).
	  Desc = ~atom_concat([VersionNopatch, '-', Desc2])
	).

% Execute a git_output on the source directory of the specified bundle
bundle_git_output(Bundle, Args, R) :-
	bundle_git_repo_dir(Bundle, Path),
	git_output(Path, Args, R).

bundle_git_repo_dir(Bundle, Path) :-
	( root_git_repo_dir(Path0) -> % TODO: check anything?
	    % Git repo explicitly specified in configuration
	    Path = Path0
	; Path0 = ~bundle_path(Bundle, '.'),
	  git_repo_at_dir(Path0) ->
	    Path = Path0
	; Path0 = ~bundle_workspace(Bundle),
	  git_repo_at_dir(Path0) ->
	    Path = Path0
	; fail
	).

:- use_module(ciaobld(builder_flags), [get_builder_flag/2]).

% TODO: should 'git_repo_dir' be an option for each bundle?
% TODO: See ciaobot/bundle_builder.sh (COMMIT_INFO_OPTS)
%   (specify where the Git repo is stored for this bundle)
root_git_repo_dir(V) :-
	( get_builder_flag(git_repo_dir, V) ->
	    true
	; fail
	).

