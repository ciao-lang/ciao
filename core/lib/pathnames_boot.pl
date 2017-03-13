% (included file)
%
% NOTE: Duplicated from library(pathnames). These predicates are
%   needed before modules are initialized in internals:setup_paths/0.
%   We cannot use the library(_) version yet.

% :- export(path_concat/3).
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
