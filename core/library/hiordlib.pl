:- module(hiordlib, [
     foldl/4,
     foldl/5,
     foldl/6,
     foldl/7,
     foldl/8,
     foldr/4,
     minimum/3,
     filter/3,
     partition/4,
     maplist/2,
     maplist/3,
     maplist/4,
     maplist/5,
     maplist/6
   ], [assertions, basicmodes, nativeprops, dcg, fsyntax, hiord, unittestdecls]).

% :- fun_eval(arith(true)).

:- doc(title, "Common higher-order predicates").

:- doc(author, "Daniel Cabeza").
:- doc(author, "Manuel Carro").
:- doc(author, "Edison Mera").
:- doc(author, "Jose F. Morales").

:- doc(module, "This library implements a few basic higher-order
   predicates for reducing and transforming lists.
").

% ---------------------------------------------------------------------------

% NOTE: foldl/N was called scanlist/N in Quintus and DEC-10. According
%   to Richard O'Keefe that was a mistake:
%   http://www.cs.otago.ac.nz/staffpriv/ok/pllib.htm

:- meta_predicate foldl(pred(3), ?, ?, ?).
:- meta_predicate foldl(pred(4), ?, ?, ?, ?).
:- meta_predicate foldl(pred(5), ?, ?, ?, ?, ?).
:- meta_predicate foldl(pred(6), ?, ?, ?, ?, ?, ?).
:- meta_predicate foldl(pred(7), ?, ?, ?, ?, ?, ?, ?).

:- pred foldl(+P, ?Xs, ?V0, ?V)
   :: callable * list(term) * term * term
   # "Reduces (fold) @var{Xs} from the left applying @var{P} and using
      @var{V0}-@var{V} as accumulator.".
%
:- pred foldl(+P, ?Xs, ?Ys, ?V0, ?V)
   :: callable * list(term) * list(term) * term * term
   # "Like @pred{foldl/4} but applied to successive tuples
      from @var{Xs}, @var{Ys}.".
%
:- pred foldl(+P, ?Xs, ?Ys, ?Zs, ?V0, ?V)
   :: callable * list(term) * list(term) * list(term) * term * term
   # "Like @pred{foldl/4} but applied to successive tuples
      from @var{Xs}, @var{Ys}, @var{Zs}.".
%
:- pred foldl(+P, ?Xs, ?Ys, ?Zs, ?Us, ?V0, ?V)
   :: callable * list(term) * list(term) * list(term) * list(term) * term * term
   # "Like @pred{foldl/4} but applied to successive tuples
      from @var{Xs}, @var{Ys}, @var{Zs}, @var{Us}.".
%
:- pred foldl(+P, ?Xs, ?Ys, ?Zs, ?Us, ?Ws, ?V0, ?V)
   :: callable * list(term) * list(term) * list(term) * list(term) * list(term) * term * term
   # "Like @pred{foldl/4} but applied to successive tuples
      from @var{Xs}, @var{Ys}, @var{Zs}, @var{Us}, @var{Ws}.".

:- doc(foldl/4, "
The left fold family @tt{foldl/N} is equivalent to:

@begin{verbatim}
foldl(P, [X11,...,X1n], [Xm1,...,Xmn], V0, V) :-
    P(X11, ..., Xm1, V0, V1),
    ...
    P(Xn,  ..., Xmn, Vn_1, Vn).
@end{verbatim}
").

% Note on argument order:
%
%  - algebraic-style (like Ocaml, Haskell)
%    foldl: f(f(f(z, 1), 2), 3)
%
%  - accumulator-style (like Elm, Elixir, Erlang)
%    foldl: f(3, f(2, f(1, z)))

foldl(P, Xs, V0, V) :-
    foldl1(Xs, P, V0, V).
foldl(P, Xs, Ys, V0, V) :-
    foldl2(Xs, P, Ys, V0, V).
foldl(P, Xs, Ys, Zs, V0, V) :-
    foldl3(Xs, P, Ys, Zs, V0, V).
foldl(P, Xs, Ys, Zs, Us, V0, V) :-
    foldl4(Xs, P, Ys, Zs, Us, V0, V).
foldl(P, Xs, Ys, Zs, Us, Ws, V0, V) :-
    foldl5(Xs, P, Ys, Zs, Us, Ws, V0, V).

:- meta_predicate foldl1(?, pred(3), ?, ?).
:- meta_predicate foldl2(?, pred(4), ?, ?, ?).
:- meta_predicate foldl3(?, pred(5), ?, ?, ?, ?).
:- meta_predicate foldl4(?, pred(6), ?, ?, ?, ?, ?).
:- meta_predicate foldl5(?, pred(7), ?, ?, ?, ?, ?, ?).

foldl1([], _P, V, V).
foldl1([X|Xs], P, V0, V) :-
    P(X, V0, V1),
    foldl1(Xs, P, V1, V).

foldl2([], _P, [], V, V).
foldl2([X|Xs], P, [Y|Ys], V0, V) :-
    P(X, Y, V0, V1),
    foldl2(Xs, P, Ys, V1, V).

foldl3([], _P, [], [], V, V).
foldl3([X|Xs], P, [Y|Ys], [Z|Zs], V0, V) :-
    P(X, Y, Z, V0, V1),
    foldl3(Xs, P, Ys, Zs, V1, V).

foldl4([], _P, [], [], [], V, V).
foldl4([X|Xs], P, [Y|Ys], [Z|Zs], [U|Us], V0, V) :-
    P(X, Y, Z, U, V0, V1),
    foldl4(Xs, P, Ys, Zs, Us, V1, V).

foldl5([], _P, [], [], [], [], V, V).
foldl5([X|Xs], P, [Y|Ys], [Z|Zs], [U|Us], [W|Ws], V0, V) :-
    P(X, Y, Z, U, W, V0, V1),
    foldl5(Xs, P, Ys, Zs, Us, Ws, V1, V).

% (example)
:- test foldl(P, Xs, V0, V) : (
     P = (''(A,B,C) :- C=[A|B]),
     Xs = [1,2,3,4],
     V0 = []
   ) => (V = [4,3,2,1]) + (not_fails, is_det)
   # "Reverse a list".

:- test foldl(F, Xs, Y, Y0) : (
     F = (''(A,B,C) :- B=c(A,C)),
     Xs = [1,2,3,4],
     Y0 = nil
   ) => (Y = c(1,c(2,c(3,c(4,nil))))) + (not_fails, is_det)
   # "Change list representation (tail-recursive)".

:- test foldl(F, As, Bs, Cs, Cs0) : (
     F = (''(A,B,Ys,Ys0) :- Ys=[(A,B)|Ys0]),
     As = [1,2,3,4],
     Bs = [a,b,c,d],
     Cs0 = []
   ) => (Cs = [(1,a),(2,b),(3,c),(4,d)]) + (not_fails, is_det)
   # "Zip two lists (tail-recursive)".

:- test foldl(P, Xs, Ys, Ys0) : (
    P = (''(I, [E|Es], Es) :- arg(I, f(a, b, c, d), E)),
    Xs = [1, 3, 2],
    Ys0 = [x, y]
   ) => (Ys = [a, c, b, x, y]) + (not_fails, is_det).

% ---------------------------------------------------------------------------

:- doc(bug, "We only provide foldr/4. Add more versions if needed.").
:- doc(bug, "We do not provide scanl/N or scanr/N. Add them if needed.").

:- meta_predicate foldr(pred(3), ?, ?, ?).
:- pred foldr(+F, ?Xs, +V0, ?V)
   :: callable * list(term) * term * term
   # "Reduces (fold) @var{Xs} from the right applying @var{P} and using
      @var{V0}-@var{V} as accumulator.".

:- doc(foldr/4, "
The right fold family @tt{foldr/N} is equivalent to:

@begin{verbatim}
foldr(P, [X11,...,X1n], [Xm1,...,Xmn], V0, V) :-
    P(Xn,  ..., Xmn, V0, V1),
    ...
    P(X11, ..., Xm1, Vn_1, V).
@end{verbatim}

Note that @tt{foldr/N} is not tail recursive. When @tt{P(...,?,?)} is
a valid calling mode, it would be possible to reorder the calls as in:

@begin{verbatim}
foldr_tail(P, [X11,...,X1n], [Xm1,...,Xmn], V0, V) :-
    P(X11, ..., Xm1, Vn_1, V),
    ...
    P(Xn,  ..., Xmn, V0, V1).
@end{verbatim}

which is exactly like @tt{foldl/N} but with fliped accumulator
arguments.  See @tt{foldl/N} examples").

% E.g.,
%    foldr: f(1, f(2, f(3, z)))

foldr(P, Xs, V0, V) :-
    foldr_(Xs, P, V0, V).

:- meta_predicate foldr_(?, pred(3), ?, ?).
foldr_([], _P, Z, Z).
foldr_([X|Xs], P, Z, R) :-
    foldr_(Xs, P, Z, R1),
    P(X, R1, R).

% (example)
:- test foldr(P, Xs, Y0, Y) : (
     P = (''(A,B,C) :- C=c(A,B)),
     Xs = [1,2,3,4],
     Y0 = nil
   ) => (Y = c(1,c(2,c(3,c(4,nil))))) + (not_fails, is_det)
   # "Change list representation".

% (example)
% TODO: find an example where P is not commutative
:- test foldr(P, Xs, V0, V) : (
     P = (''(A,B,C) :- C is A+B),
     Xs = [1,2,3,4],
     V0 = 0
   ) => (V = 10) + (not_fails, is_det)
   # "Sum elements of list".

% ---------------------------------------------------------------------------

:- meta_predicate minimum(_, pred(2), _).
:- pred minimum(?List, +SmallerThan, ?Minimum) : list * callable *
    term # "@var{Minimum} is the smaller in the nonempty list
    @var{List} according to the relation @var{SmallerThan}:
    @pred{SmallerThan(X, Y)} succeeds iff X is smaller than Y.".

minimum([X|Xs], Pred, Min) :- minimum_carry(Xs, Pred, X, Min).

minimum_carry([],     _Pred, M,        M).
minimum_carry([X|Xs], Pred,  MinSoFar, Min) :-
    (
        Pred(MinSoFar, X) ->
        minimum_carry(Xs, Pred, MinSoFar, Min)
    ;
        minimum_carry(Xs, Pred, X, Min)
    ).

% ---------------------------------------------------------------------------

:- meta_predicate filter(pred(1), +, ?).
:- pred filter(+P, +Xs, ?Ys)
    :: (callable(P), list(Xs), list(Ys)) + is_det
   # "@var{Ys} contains all elements @var{X} of @var{Xs} such that
     @tt{P(X)} holds (preserving the order)".

filter(Goal, List, Included) :-
    filter_(List, Goal, Included).

filter_([], _, []).
filter_([X|Xs], P, Ys) :-
    ( P(X) ->
        Ys = [X|Ys0]
    ; Ys = Ys0
    ),
    filter_(Xs, P, Ys0).

% ---------------------------------------------------------------------------

:- meta_predicate partition(pred(1), ?, ?, ?).
:- pred partition(+P, +Xs, ?Ys, ?Zs) ::
   callable * list * list * list
   # "@var{Ys} contains all elements @var{X} of @var{Xs} such that
      @tt{P(X)} holds, and @var{Zs} all that does not (preserving the
      order)".

partition(P, Xs, Ys, Zs) :-
    partition_(Xs, P, Ys, Zs).

partition_([], _, [], []).
partition_([X|Xs], P, Ys, Zs) :-
    ( P(X) ->
        Ys = [X|Ys0],
        Zs = Zs0
    ; Ys = Ys0,
      Zs = [X|Zs0]
    ),
    partition_(Xs, P, Ys0, Zs0).

:- test partition(P, Xs, Ys, Zs) : (
    P='<'(4), Xs=[1, 2, 3, 4, 5, 6]
   ) => (Ys=[5, 6], Zs=[1, 2, 3, 4]) + not_fails.

% ---------------------------------------------------------------------------
% maplist/N

:- meta_predicate maplist(pred(1), ?).
:- meta_predicate maplist(pred(2), ?, ?).
:- meta_predicate maplist(pred(3), ?, ?, ?).
:- meta_predicate maplist(pred(4), ?, ?, ?, ?).
:- meta_predicate maplist(pred(5), ?, ?, ?, ?, ?).

:- pred maplist(+P, +Xs) ::
    (callable(P), list(Xs))
   # "@tt{P(X)} succeeds for each element @var{X} of @var{Xs}".
%
:- pred maplist(+P, +Xs, ?Ys) ::
    (callable(P), list(Xs), list(Ys))
   # "Like @pred{maplist/2} but applied to successive tuples
      from @var{Xs}, @var{Ys}.".
%
:- pred maplist(+P, +Xs, ?Ys, ?Zs) ::
    (callable(P), list(Xs), list(Ys), list(Zs))
   # "Like @pred{maplist/2} but applied to successive tuples
      from @var{Xs}, @var{Ys}, @var{Zs}.".
%
:- pred maplist(+P, +Xs, ?Ys, ?Zs, ?Vs) ::
    (callable(P), list(Xs), list(Ys), list(Zs), list(Vs))
   # "Like @pred{maplist/2} but applied to successive tuples
      from @var{Xs}, @var{Ys}, @var{Zs}, @var{Vs}.".
%
:- pred maplist(+P, +Xs, ?Ys, ?Zs, ?Vs, ?Ws) ::
    (callable(P), list(Xs), list(Ys), list(Zs), list(Vs), list(Ws))
   # "Like @pred{maplist/2} but applied to successive tuples
      from @var{Xs}, @var{Ys}, @var{Zs}, @var{Vs}, @var{Ws}.".

:- doc(maplist/2, "
The map list family @tt{maplist/N} is equivalent to:

@begin{verbatim}
maplist(P, [X11,...,X1n], [Xm1,...,Xmn]) :-
    P(X11, ..., Xm1),
    ...
    P(Xn,  ..., Xmn).
@end{verbatim}
").

maplist(P, Xs) :-
    maplist1(Xs, P).
maplist(P, Xs, Ys) :-
    maplist2(Xs, P, Ys).
maplist(P, Xs, Ys, Zs) :-
    maplist3(Xs, P, Ys, Zs).
maplist(P, Xs, Ys, Zs, Vs) :-
    maplist4(Xs, P, Ys, Zs, Vs).
maplist(P, Xs, Ys, Zs, Vs, Ws) :-
    maplist5(Xs, P, Ys, Zs, Vs, Ws).

:- meta_predicate maplist1(?, pred(1)).
:- meta_predicate maplist2(?, pred(2), ?).
:- meta_predicate maplist3(?, pred(3), ?, ?).
:- meta_predicate maplist4(?, pred(4), ?, ?, ?).
:- meta_predicate maplist5(?, pred(5), ?, ?, ?, ?).

maplist1([], _).
maplist1([X|Xs], P) :-
    P(X),
    maplist1(Xs, P).

maplist2([], _, []).
maplist2([X|Xs], P, [Y|Ys]) :-
    P(X, Y),
    maplist2(Xs, P, Ys).

maplist3([], _, [], []).
maplist3([X|Xs], P, [Y|Ys], [Z|Zs]) :-
    P(X, Y, Z),
    maplist3(Xs, P, Ys, Zs).

maplist4([], _, [], [], []).
maplist4([X|Xs], P, [Y|Ys], [Z|Zs], [V|Vs]) :-
    P(X, Y, Z, V),
    maplist4(Xs, P, Ys, Zs, Vs).

maplist5([], _, [], [], [], []).
maplist5([X|Xs], P, [Y|Ys], [Z|Zs], [V|Vs], [W|Ws]) :-
    P(X, Y, Z, V, W),
    maplist5(Xs, P, Ys, Zs, Vs, Ws).

:- load_test_module(library(lists), [nth/3, append/3]).

:- test maplist(P, A, B) : (
    P = (''(X,Y) :- arg(X,f(a, b, c, d),Y)),
    A = [1, 3, 2]
   ) => (B = [a, c, b]) + (not_fails, is_det).

:- test maplist(P, A, B) : (
    P = (''(X,Y) :- nth(X,[a, b, c, d],Y)),
    A = [1, 3, 2]
   ) => (B = [a, c, b]) + (not_fails, is_det).

:- test maplist(P, A, B) : (
    P = append("."),
    A = ["D", "C"]
   ) => (B = [".D", ".C"]) + (not_fails, is_det).


