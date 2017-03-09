% (included file)
%
% NOTE: Duplicated from library(pathnames). These predicates are
%   needed before modules are initialized in internals:setup_paths/0.
%   We cannot use the library(_) version yet.

% :- export(path_concat/3).
% :- export(path_split/3).
% :- use_module(engine(internals), ['$path_is_absolute'/1]).

path_concat('', B, R) :- !, R = B.
path_concat(_A, B, R) :- '$path_is_absolute'(B), !,
	R = B.
path_concat(A, B, R) :-
	( atom_concat(_, '/', A) ->
	    A0 = A
        ; atom_concat(A, '/', A0) % add '/' if needed
	),
	atom_concat(A0, B, R).

path_split(Path, Dir, Base) :-
	atom_codes(Path, PathS),
	path_split_(PathS, DirS, BaseS),
	atom_codes(Dir, DirS),
	atom_codes(Base, BaseS).

path_split_(Path, Dir, Base) :-
	reverse(Path, R),
	( append(BaseR, "/"||DirR, R) ->
	    anysep(DirR, DirR2), % Strip all trailing /
	    ( DirR2 = "" -> % (Dir is root, preserve all /)
	        Dir = "/"||DirR
	    ; reverse(DirR2, Dir)
	    ),
	    reverse(BaseR, Base)
	; Dir = "",
	  Base = Path
	).

% Zero or more '/'
anysep("/"||Xs, Ys) :- !, anysep(Xs, Ys).
anysep(Xs, Xs).

% [duplicated from library(lists)]

append([], L, L).
append([E|Es], L, [E|R]) :- append(Es, L, R).

reverse(Xs,Ys):- reverse_(Xs,[],Ys).

reverse_([], L, L).
reverse_([E|Es],L,R) :- reverse_(Es,[E|L],R).

