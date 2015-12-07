:- module(vcs_svn, [], [assertions, basicmodes,
		nativeprops, fsyntax, hiord, regtypes]).

:- doc(title, "VCS Abstraction for SVN"). 
:- doc(author, "Jose F. Morales").
:- doc(author, "Edison Mera (original author)").

:- doc(summary, "This module defines predicates to interact with the
  SVN version control system (VCS)").

:- use_module(library(lists)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(datetime)).
:- use_module(library(system), [file_exists/1]).
:- use_module(library(process), [process_call/3, process_pipe/2]).

% ---------------------------------------------------------------------------

:- export(svn_repo_at_dir/1).
:- pred svn_repo_at_dir(D) # "@var{D} contains a SVN repository".
svn_repo_at_dir(Dir) :-
	file_exists(~atom_concat(Dir, '/.svn')).

% ---------------------------------------------------------------------------

:- export(svn_revision_date/3).
:- pred svn_revision_date(+atm, +atm, ?string).
% (Revision date string is in ISO 8601 format)
svn_revision_date(Repository, Revision0, Date) :-
	just_revision_number(Revision0, Revision),
	% TODO: do not use pipes, parse in Prolog
	process_pipe(
          [process_call(path(svn), ['info', Repository, '--xml', '-r', Revision], [status(0)]),
	   process_call(path(grep), ['<date>'], [status(0)]),
           process_call(path(sed), ['-e', 's:<date>::g', '-e', 's:</date>::g'])],
	  [stdout(string(Date0)), status(0)]),
	!,
	Date = Date0.

just_revision_number(Revision0, Revision) :-
	Revision1 = ~atom_codes(Revision0),
	append(Revision2, [C|_], Revision1),
	\+ is_digit(C),
	!,
	atom_codes(Revision, Revision2).
just_revision_number(Revision, Revision).

is_digit(X) :- X >= 0'0, X =< 0'9.

% ---------------------------------------------------------------------------

:- export(svn_repository_root/2).
:- pred svn_repository_root(+Path, ?Root) :: atm * atm
   # "The path @var{Path} is part of a working copy of the 
      repository @var{Root}.".

svn_repository_root(Path) := R :-
	% TODO: do not use pipes, parse in Prolog
	process_pipe(
          [process_call(path(svn), ['info', Path], [stderr(null), status(_)]),
	   process_call(path(grep), ['URL: '], status(_)),
	   process_call(path(sed), ['-e', 's/URL: //g'], status(_))],
	  [stdout(line(S))]),
	atom_codes(R, S).

% ---------------------------------------------------------------------------

:- export(svn_get_revision/2).
:- pred svn_get_revision(+Path, ?Rev) :: atm * atm 
   # "Obtain the revision number @var{Rev} corresponding to file or
      directory at @var{Path}".

% TODO: can we avoid 'which'?
svn_get_revision(Path) := Rev :-
	% Is svnversion available?
	process_call(path(which), ['svnversion'],
	    [stderr(null), stdout(null), status(0)]),
	% Call it
	process_call(path(svnversion), [Path],
	    [stdout(line(Rev0)), status(0)]),
	%
	( Rev0 == "" -> Rev = 'exported'
	; % Check that we are in a SVN checkout.
	  % This will match 'Unversioned...' or 'Uncommited...',
	  % see 'svnversion' documentation for details.
	  \+ Rev0 = "Un"||_,
	  atom_codes(Rev, Rev0)
	).

