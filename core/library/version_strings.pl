:- module(version_strings, [], [assertions]).

:- doc(title, "Version string parsing and comparison").

:- doc(module, "This module contains operations to work with version
   strings. The specification of the format and operations is
   partially inspired on @href{http://semver.org/}{Semantic Versioning}.
").

:- doc(bug, "Finish (add version comparison, parse prerelease and build fields)").

:- use_module(library(lists), [append/3]).

:- export(parse_version/3).
% Obtain Major.Minor and Patch version numbers, as follows:
%   'Major' -> 'Major', '0'
%   'Major.Minor' -> 'Major.Minor', '0'
%   'Major.Minor.Patch' -> 'Major.Minor', 'Patch'
parse_version(VerAtm, VersionAtm, PatchAtm) :-
	atom_codes(VerAtm, Ver),
	splitdots(Ver, Vs),
	( Vs = [] -> fail
	; Vs = [Major|Vs0] ->
	    ( Vs0 = [] -> Version = Major, Patch = "0"
	    ; Vs0 = [Minor|Vs1] ->
	        append(Major, "."||Minor, Version),
		( Vs1 = [] -> Patch = "0"
		; Vs1 = [Patch] -> true
		; fail
		)
	    ; fail
	    )
	; fail
	),
	atom_codes(VersionAtm, Version),
	atom_codes(PatchAtm, Patch).

% Split string at dots (e.g., "1.2.3" => ["1","2","3"])
splitdots([], []) :- !.
splitdots(Cs, [X|Xs]) :-
	splitdots_(Cs, X, Cs2),
	splitdots(Cs2, Xs).

splitdots_([], [], []).
splitdots_([0'.|Cs], [], Cs) :- !.
splitdots_([C|Cs], [C|Ds], Cs2) :-
	splitdots_(Cs, Ds, Cs2).

