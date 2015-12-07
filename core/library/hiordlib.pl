:- module(hiordlib, [map/3, map/4, map/5, map/6, foldl/4, minimum/3, split/4],
	    [assertions, basicmodes, nativeprops, dcg, fsyntax, hiord,
		unittestdecls]).

% :- fun_eval(arith(true)).

:- doc(title, "Higher-order predicates").

:- doc(author, "Daniel Cabeza").
:- doc(author, "Manuel Carro").
:- doc(author, "Edison Mera").

:- doc(module, "This library implements a few basic higher-order
   predicates. These add functionality to the basic 
   higher-order functionality of Ciao. Examples of the latter are:


   Using pred(1):

@begin{verbatim}
  list(L, functor(_,2))
  list(L, >(0))
@end{verbatim}

   Using pred(2):

").

:- pred map(LList, Op, RList) # "Examples of use:
@begin{verbatim}
  map([1,3,2], arg(f(a,b,c,d)), [a,c,b]) or
  map([1,3,2], nth([a,b,c,d]), [a,c,b])
  map([\"D\",\"C\"], append(\".\"), [\"D.\",\"C.\"])
@end{verbatim}
".

:- load_test_module(library(lists), [nth/3, append/3]).

:- test map(A, B, C) : (A = [1, 3, 2], B = arg(f(a, b, c, d)))
	=> (C = [a, c, b]) + (not_fails, is_det).

:- test map(A, B, C) : (A = [1, 3, 2], B = nth([a, b, c, d]))
	=> (C = [a, c, b]) + (not_fails, is_det).

:- test map(A, B, C) : (A = ["D", "C"], B = append("."))
	=> (C = ["D.", "C."]) + (not_fails, is_det).

:- meta_predicate map(_, pred(2), _).

map([],     _) := [].
map([X|Xs], P) := [~P(X) |~map(Xs, P)].

:- pred map(LList, Op, RList, Tail) # "DCG version of map.".

:- test map(A, B, C, D) :
	(
	    A = [1, 3, 2],
	    B = (''(L, [E|T], T) :- arg(L, f(a, b, c, d), E)),
	    D = [x, y])
	=> (C = [a, c, b, x, y]) + (not_fails, is_det).

:- meta_predicate map(?, pred(3), ?, ?).

map([],     _) --> [].
map([X|Xs], P) --> P(X), map(Xs, P).

:- meta_predicate map(?, ?, pred(4), ?, ?).

map([],     [], _) --> [].
map([X|Xs], [Y|Ys], P) --> P(X, Y), map(Xs, Ys, P).

:- meta_predicate map(?, ?, ?, pred(5), ?, ?).

map([],     [],     [],     _) --> [].
map([X|Xs], [Y|Ys], [Z|Zs], P) --> P(X, Y, Z), map(Xs, Ys, Zs, P).

:- pred foldl(List, Seed, Op, Result) # "Example of use:
@begin{verbatim}
?- foldl([\"daniel\",\"cabeza\",\"gras\"], \"\", 
         (''(X,Y,Z) :- append(X, \" \"||Y, Z)), R).

R = \"daniel cabeza gras \" ? 
@end{verbatim}
".

:- meta_predicate foldl(_, _, pred(3), _).

foldl([],     Seed, _Op) := Seed.
foldl([X|Xs], Seed, Op) := ~Op(X, ~foldl(Xs, Seed, Op)).

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

:- pred split(+List, +Condition, ?Left, ?Right) : ( list * callable *
	    term * term ) => (list * callable * list * list) # "Divides
	@var{List} in two list, where @var{Left} contains the elements
	for which the call to @var{Condition} succeeds, and @var{Right} the
	remaining elements.".

:-test split(A, B, C, D)
	: (A=[1, 2, 3, 4, 5, 6], B= '>'(4))
	=> (C=[5, 6], D=[1, 2, 3, 4])
	+ not_fails.

:- meta_predicate split(?, pred(1), ?, ?).

split([],    _,         [],    []).
split([E|R], Condition, Left0, Right0) :-
	(
	    Condition(E) ->
	    Left0 = [E|Left],
	    Right0 = Right
	;
	    Left0 = Left,
	    Right0 = [E|Right]
	),
	split(R, Condition, Left, Right).
