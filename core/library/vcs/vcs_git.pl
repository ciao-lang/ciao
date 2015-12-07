:- module(vcs_git, [], [assertions, basicmodes,
		nativeprops, fsyntax, hiord, regtypes]).

:- doc(title, "VCS Abstraction for Git"). 
:- doc(author, "Jose F. Morales").

:- doc(summary, "This module defines predicates to interact with the
  Git version control system (VCS)").

:- doc(bug, "This abstraction layer is really incomplete").

:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(system), [file_exists/1]).
:- use_module(library(process), [process_call/3]).

% ---------------------------------------------------------------------------

:- export(git_repo_at_dir/1).
:- pred git_repo_at_dir(D) # "There is a Git repository at directory @var{D}".

git_repo_at_dir(Dir) :-
	file_exists(~atom_concat(Dir, '/.git')).

% ---------------------------------------------------------------------------

:- export(git_output/3).
% TODO: Improve
% Execute a Git command on the specified directory, ignore standard
% error, and get standard output as an atom.
git_output(Path, Args, R) :-
	process_call(path(git), Args,
	             [cwd(Path),
		      stderr(null), stdout(line(R0)),
		      status(0)]),
	atom_codes(R, R0).


