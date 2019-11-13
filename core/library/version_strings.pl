:- module(version_strings, [], [assertions]).

:- doc(title, "Version string parsing and comparison").
:- doc(author, "Jose F. Morales").

:- doc(module, "This module contains operations to work with version
   strings (as atoms). The specification of the format and operations
   is partially inspired on @href{http://semver.org/}{Semantic
   Versioning}.

   Normal versions are specified as strings of the form @tt{X.Y.Z},
   where @tt{X}, @tt{Y}, and @tt{Z} are non-negative integers. The
   components denote the major, minor, and patch version,
   respectively. Missing version numbers are assumed to be 0.

   Pre-release versions include an optional @em{pre-release}
   identifier (@tt{X.Y.Z-Prerelease}), which itself may contain many
   dot separated components.

   Versions are compared by comparing each component from left to
   right. Each component is compared numerically (if numbers) or
   lexicographically (if not numbers). Numeric components have lower
   precedence than non-numeric components. A pre-release version has
   lower precedence than its corresponding normal version.").

% TODO: Extend module interface as needed

% TODO: 'git describe' is incompatible with semver, use a dot instead
%   of hyphen to separate commit numbers from hash?

% TODO: Add support for build metadata in versions (X.Y.Z-Prerelease+Build)
%   Versions can include prerelease and build metadata

:- use_module(library(lists), [append/3]).

:- export(version_split_patch/3).
:- pred version_split_patch(VerAtm, VerNopatchAtm, PatchAtm) # "Split
   version @var{VerAtm} into major and minor @var{VerNopatchAtm}, and
   patch and prerelease @var{PatchAtm})".

version_split_patch(VerAtm, VerNopatchAtm, PatchAtm) :-
    atom_codes(VerAtm, Ver0),
    splitpre(Ver0, Ver, Pre),
    splitdots(Ver, Vs),
    ( Vs = [] -> fail
    ; Vs = [Major|Vs0] ->
        ( Vs0 = [] -> VerNopatch = Major, Patch = "0"
        ; Vs0 = [Minor|Vs1] ->
            append(Major, "."||Minor, VerNopatch),
            ( Vs1 = [] -> Patch = "0"
            ; Vs1 = [Patch] -> true
            ; fail
            )
        ; fail
        )
    ; fail
    ),
    ( Pre = "" -> PatchPre = Patch
    ; append(Patch, "-"||Pre, PatchPre)
    ),
    atom_codes(VerNopatchAtm, VerNopatch),
    atom_codes(PatchAtm, PatchPre).

% Split string at dots (e.g., "1.2.3" => ["1","2","3"])
splitdots([], []) :- !.
splitdots(Cs, [X|Xs]) :-
    splitdots_(Cs, X, Cs2),
    splitdots(Cs2, Xs).

splitdots_([], [], []).
splitdots_([0'.|Cs], [], Cs) :- !.
splitdots_([C|Cs], [C|Ds], Cs2) :-
    splitdots_(Cs, Ds, Cs2).

:- export(version_compare/3).
:- pred version_compare(C, A, B) # "Compare versions @var{A} and
   @var{B} (see @pred{compare/3} for values of @var{C})".

version_compare(C, A, B) :-
    version_to_term(A, A2),
    version_to_term(B, B2),
    compare(C, A2, B2).

%:- export(version_to_term/2).
% Transform to term (for comparison using compare/3)
version_to_term(Ver, Term) :-
    atom_codes(Ver, Str),
    splitpre(Str, VerN, VerPre),
    ( VerPre = "" -> Pres2 = none('','','') % greater than any list
    ; splitdots(VerPre, Pres),
      ids_to_terms(Pres, Pres2)
    ),
    splitdots(VerN, Ns),
    ids_to_terms(Ns, Ns2),
    Term = Ns2-Pres2.    

splitpre(Str, Norm, Pre) :-
    ( append(Norm, "-"||Pre0, Str), \+ Pre0 = "" -> Pre = Pre0
    ; Norm = Str, Pre = ""
    ).

ids_to_terms([], []).
ids_to_terms([X|Xs], [Y|Ys]) :- id_to_term(X, Y), ids_to_terms(Xs, Ys).

id_to_term("0"||Cs, Id) :- !, Id = 0, Cs = "". % forbid leading zeroes
id_to_term(Cs, Id) :- nonneg_int_codes(Id0, Cs), !, Id = Id0.
id_to_term(Cs, Id) :- atom_codes(Id, Cs). % alphanum % TODO: missing check

nonneg_int_codes(N, Cs) :-
    number_codes(N, Cs),
    integer(N),
    N >= 0.
    
:- test test_norm # "Comparison of normal versions".
test_norm :- strict_sorted_vers(['1.0.0', '2.0.0', '2.1.0', '2.1.1']).

:- test test_pre # "Comparison of prerelease and normal versions (simple)".
test_pre :- strict_sorted_vers(['1.0.0-alpha', '1.0.0']).

:- test test_norm # "Comparison of prerelease versions (with subcomponents)".
test_presub :- strict_sorted_vers([
    '1.0.0-alpha', '1.0.0-alpha.1', '1.0.0-alpha.beta',
    '1.0.0-beta', '1.0.0-beta.2', '1.0.0-beta.11',
    '1.0.0-rc.1', '1.0.0']).

% TODO: only for tests! disable when not testing
strict_sorted_vers([]).
strict_sorted_vers([_]) :- !.
strict_sorted_vers([A,B|Xs]) :- version_compare(<, A, B), strict_sorted_vers([B|Xs]).

