:- module(terms, [term_size/2, copy_args/3, arg/2, atom_concat/2],
        [assertions, nativeprops]).

:- doc(title, "Term manipulation utilities").

:- doc(author, "The Ciao Development Team").

:- doc(module, "This module implements some utils to do term
   manipulation.").

%-------------------------------------------------------------------------

:- pred term_size(Term, N) => nnegint(N) # "Determines the size of
    a term.".

term_size(Term, 1) :- var(Term), !.
term_size(Term, 1) :- atomic(Term), !.
term_size(Term, Size) :-
    functor(Term, _, N),
    term_size_(N, Term, Size1),
    Size is Size1 + 1.

term_size_(0, _, 0) :- !.
term_size_(N, Term, Size) :-
    N > 0,
    arg(N, Term, Arg),
    term_size(Arg, Size1),
    N1 is N - 1,
    term_size_(N1, Term, Size2),
    Size is Size1 + Size2.

:- test term_size(A, B) : (A=p(a, b, c(d, e))) => B=6.
:- test term_size(A, B) : (A=p(a, b, c(d, _))) => B=6.
:- test term_size(A, B) : (A=[1, 2, 3]) => B=7.

%-------------------------------------------------------------------------

:- pred copy_args(N, Term, Copy) : nnegint(N) # "@var{Term} and
   @var{Copy} have the same first @var{N} arguments.".

:- trust comp copy_args(N, Term, Copy) : (ground(N), nonvar(Term)) + eval.
:- trust comp copy_args(N, Term, Copy) + sideff(free).

copy_args(0, _,  _) :- !.
copy_args(I, F1, F2) :-
    arg(I, F1, X),
    arg(I, F2, X),
    I1 is I-1,
    copy_args(I1, F1, F2).

%-------------------------------------------------------------------------

:- pred arg(Term, Arg) # "@var{Arg} is an argument of @var{Term}. Gives
   each of the arguments on backtracking.".

arg(T, A) :-
    functor(T, _, N),
    args(1, N, T, A).

args(X, _, T, A) :-
    arg(X, T, A).
args(X, M, T, A) :-
    X < M,
    X1 is X+1,
    args(X1, M, T, A).

%-------------------------------------------------------------------------

:- doc(atom_concat(Atms, Atm), "@var{Atm} is the atom resulting from
   concatenating all atoms in the list @var{Atms} in the order in which
   they appear. If @var{Atm} is an atom at call then @var{Atms} can
   contain free variables.").

:- pred atom_concat(Atms, Atm) : list(atm, Atms) => atm(Atm).
:- pred atom_concat(Atms, Atm) : atm(Atm) => list(atm, Atms).
% TODO: disabled by now, review rtchecks of compat properties and the interaction with unittests (JF)
%:- pred atom_concat(Atms, Atm) :: list(atm, Atms) : atm(Atm) => list(atm, Atms).

atom_concat([], '') :- !.
atom_concat(L,  Atom) :-
    var(L),
    !,
    L = [A|As],
    atom_concat(A, Atom1, Atom),
    A \== '',
    atom_concat(As, Atom1).
atom_concat([A|As], Atom) :-
    atom(Atom),
    !,
    atom_concat(A, Atom1, Atom),
    atom_concat(As, Atom1).
atom_concat([A|As], Atom) :-
    atoms_concat(As, A, Atom).

atoms_concat([],         Atom, Atom).
atoms_concat([A1|Atoms], A2,   Atom) :-
    atom_concat(A2, A1, A3),
    atoms_concat(Atoms, A3, Atom).

:- test atom_concat(A, B) : (A = [a, b, c]) => (B == abc).

% TODO: B and C shared between call and success (not supported in unit tests)
% :- test atom_concat(X, Y)
%    : ( X = [a, B|C], Y = abcde )
%    => ( member((B, C),
%                [ ('', [b, c, d, e]), ('', [b, c, de]), ('', [b, cd, e]),
%                  ('', [b, cde]), ('', [bc, d, e]), ('', [bc, de]),
%                  ('', [bcd, e]), ('', [bcde]), (b, [c, d, e]), (b, [c, de]),
%                  (b, [cd, e]), (b, [cde]), (bc, [d, e]), (bc, [de]),
%                  (bcd, [e]), (bcde, []) ])
%     ) # "atom_concat that generates several solutions.".

:- test atom_concat(X, Y) :
    ( X = [a, B|C],
      Y = abcde
    ) + solutions([
        atom_concat([a, '', b, c, d, e], abcde),
        atom_concat([a, '', b, c, de], abcde),
        atom_concat([a, '', b, cd, e], abcde),
        atom_concat([a, '', b, cde], abcde),
        atom_concat([a, '', bc, d, e], abcde),
        atom_concat([a, '', bc, de], abcde),
        atom_concat([a, '', bcd, e], abcde),
        atom_concat([a, '', bcde], abcde),
        atom_concat([a, b, c, d, e], abcde),
        atom_concat([a, b, c, de], abcde),
        atom_concat([a, b, cd, e], abcde),
        atom_concat([a, b, cde], abcde),
        atom_concat([a, bc, d, e], abcde),
        atom_concat([a, bc, de], abcde),
        atom_concat([a, bcd, e], abcde),
        atom_concat([a, bcde], abcde)
    ]) # "atom_concat that generates several solutions.".

:- test atom_concat(X, Y)
   % : ( X = [A, b, C], Y = abc )
   % => ( A == a, C == c ). % TODO: A and C shared between call and success (not supported in unit tests)
   : ( X = [_, b, _], Y = abc )
   => ( X = [a, b, c] ).

:- test atom_concat(X, Y)
   : ( X = [A, b, ''], Y = ab )
   => ( A = a ).
