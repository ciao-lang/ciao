:- module(diff, [diff/4, patch/3, diff_item/1], [assertions, hiord, regtypes]).

:- doc(title, "Diff algorithm").

:- doc(summary, "Algorithm for obtaining the minimum edit distance
(insertions and deletions) between two lists.").

:- doc(module, "This module implements Eugene Myers' Greedy
Difference Algorithm described in 'An O(ND) Difference Algorithm and
Its Variations'

@begin{verbatim}
  title={AnO (ND) difference algorithm and its variations},
  author={Myers, Eugene W},
  journal={Algorithmica},
  volume={1},
  number={1-4},
  pages={251--266},
  year={1986},
  publisher={Springer}
@end{verbatim}

The implementation is parametric in the comparison predicate. This
predicate has to succeed if two elements are considered equal.

@section{Complexity}

This 'greedy' algorithm that computes the number
differences has complexity O((N+M)D) in both time and space. With M
and N the lenghts of the input lists and D the number of changes.

@section{Pseudocode}

This pseudocode is copied from the original article, where:
@begin{itemize}
@item @tt{a} and @tt{b} are the input lists of lengths @tt{N} and @tt{M},
@item @tt{k = x - y} and is used to label diagonals,
@item @tt{V[k]} contains the row index of the endpoint of a furthest
reaching path in diagonal k.
@end{itemize}

@begin{verbatim}
Constant MAX [0,M+N]

Var V: Array [-MAX.. MAX] of Integer

V[1] = 0

For D = 0 to MAX Do
   For k = -D to D in steps of 2 Do
      If k = -D or (k \= D and V[k-1] < V[k+1] Then
	x = V[k+1]
      Else
	x = V[k-1] + 1
	y = x - k
      While (x < N, y < M and a(x+1) = b(y+1) Do
	(x,y) = (x+1,y+1)
      V[k] = x
      If (x >= N and y >= M) Then
	    Length of an SES is D
	    Stop

Length of an SES is greater than MAX
@end{verbatim}

@section{Example}

@begin{verbatim}

?- diff([a,a,b,c], [b,c,d], '=', Diff).

Diff = [del(0,a),del(0,a),ins(2,d)] ?

yes
?-
@end{verbatim}

@section{Patch}

The patch operation applies a list of changes expressed as
@prop{diff_item/1} to obtain a new list. Note that given two lists L1,
L2, if their Diff is applied to L1, it will be obtained L2:

@begin{verbatim}

?- L1 = [a,a,b,c], L2 = [b,c,d],
   diff(L1, L2, '=', Diff),
   patch(L1, Diff, L2).


Diff = [del(0,a),del(0,a),ins(2,d)],
L1 = [a,a,b,c],
L2 = [b,c,d] ?
yes
?-
@end{verbatim}
").

:- doc(author, "Isabel Garcia").
:- doc(author, "Jose F. Morales").

% TODO:
:- doc(bug, "A match/3 predicate could be implemented to compute the
longest common sequence between two lists (using a computed diff).").

:- use_module(library(lists), [length/2, reverse/2]).

:- meta_predicate diff(?, ?, pred(2), ?).
:- pred diff(Ls1, Ls2, Compare, Diff) : (list(Ls1), list(Ls2)) => list(Diff, diff_item)
	#"@var{Diff} are the changes needed to transform @var{Ls1}
	into @var{Ls2}.".

diff(As, Bs, Compare, Diff) :-
	loop(Bs, As, Compare, _, Stop),
	Stop = stop(diag(_, [], [], RevDiff)),
	reverse(RevDiff, Diff).

:- meta_predicate loop(?, ?, pred(2), ?, ?).
loop(As, Bs, Compare, DKs, Stop) :-
	length(As, N),
	length(Bs, M),
	Max is M+N,
	diag1(As, Bs, DK1),
	loop_d(0, Max, Compare, [DK1], DKs, Stop).

:- meta_predicate loop_d(?, ?, pred(2), ?, ?).
loop_d(D, Max, _, DKs, DKs, _Stop) :- D > Max, !.
loop_d(D, Max, Compare, DKs, DKs2, Stop) :-
	Kinit is -D,
	loop_k(Kinit, D, Compare, DKs, DKs1, Stop),
	( nonvar(Stop) ->
	    true
	; D1 is D + 1,
	  loop_d(D1, Max, Compare, DKs1, DKs2, Stop)
	).

:- meta_predicate loop_k(?, ?, pred(2), ?, ?, ?).
loop_k(K, D, _, _, [], _Stop) :- K > D, !.
loop_k(K, D, Compare, [DK1], [DK], Stop) :- K=0, D=0, !,
	samediag(DK1, DK, Compare),
	( stopdiag(DK) ->
	    Stop = stop(DK)
	; true
	).
loop_k(K, D, Compare, [DKprev], [DK2], Stop) :- K=D, !,
	moveright(DKprev, DK),
	samediag(DK,DK2,Compare),
	( stopdiag(DK2) ->
	    Stop = stop(DK2)
	; true
	).
loop_k(K, D, Compare, [DKnext|DKothers], [DK2|DKs], Stop) :- K is -D, !,
	movedown(DKnext, DK),
	%seek(DK, DK2, Compare, DKs, Stop).
	samediag(DK,DK2,Compare),
	( stopdiag(DK2) ->
	    Stop = stop(DK2), DKs=[]
	; K1 is K + 2,
	  loop_k(K1, D, Compare, [DKnext|DKothers], DKs, Stop)
	).
loop_k(K, D, Compare, [DKprev, DKnext|DKothers], [DK2|DKs], Stop) :- !,
	( less(DKprev, DKnext) ->
	    movedown(DKnext, DK)
	; moveright(DKprev, DK)
	),
	samediag(DK,DK2,Compare),
	( stopdiag(DK2) ->
	    Stop = stop(DK2), DKs=[]
	; K1 is K + 2,
	  loop_k(K1, D, Compare, [DKnext|DKothers], DKs, Stop)
	).

stopdiag(diag(_,[],[],_Ops)).

less(diag(X1,_,_,_), diag(X2,_,_,_)) :- X1 < X2.

% (delete)
movedown(DK1,DK) :-
	DK1 = diag(X,As,[B|Bs],Ops), !,
	DK  = diag(X,As,Bs,[del(X,B)|Ops]).
movedown(DK1,DK) :-
	DK1 = diag(X,As,[],Ops), !,
	DK  = diag(X,As,[],Ops). % (advance on non existing elements)

% (insert A)
moveright(DK1,DK) :-
	DK1 = diag(X,[A|As],Bs,Ops), !,
	X1 is X+1,
	DK  = diag(X1,As,Bs,[ins(X,A)|Ops]).
moveright(DK1,DK) :- % (advance on non existing elements)
	DK1 = diag(X,[],Bs,Ops), !,
	X1 is X+1,
	DK  = diag(X1,[],Bs,Ops).

% first diagnoal (V[1]=0)
diag1(As,Bs,diag(0,As,Bs,[])).

:- meta_predicate samediag(?, ?, pred(2)).
samediag(diag(X,As,Bs,Ops),diag(X2,As2,Bs2,Ops), Compare) :-
	samediag_(As,Bs,Compare,As2,Bs2,X,X2).

:- meta_predicate samediag_(?, ?, pred(2), ?, ?, ?, ?, ?).
samediag_([A|As],[B|Bs],Compare,As2,Bs2,X,X2) :-
	Compare(A,B), !,
	X1 is X+1,
	samediag_(As,Bs,Compare,As2,Bs2,X1,X2).
samediag_(As,Bs,_,As,Bs,X,X).

% --------------------------------------------------
:- pred patch(Ls, Diff, LNew) : (list(Ls), list(Diff, diff_item)) => list(LNew)
	#"Apply a list of changes (@var{Diff}) onto a @var{Ls}.".
patch(Ls, Diff, LNew) :-
	patch_(Diff, Ls, 0, LNew).

patch_([], Ls, _, Ls) :- !.
patch_([ins(Pos, Elem)|Diff], Ls, Cont, [Elem|NewLs]) :-
	Pos = Cont, !,
	Cont1 is Cont + 1,
	patch_(Diff, Ls, Cont1, NewLs).
patch_([del(Pos, _Elem)|Diff], [_|Ls], Cont, NewLs) :-
	Pos = Cont, !,
	patch_(Diff, Ls, Cont, NewLs).
patch_(Diff, [Elem|Ls], Cont, [Elem|NewLs]) :-
	Cont1 is Cont + 1,
	patch_(Diff, Ls, Cont1, NewLs).

:- regtype diff_item(X) # "@var{X} is a single edition in a sequence
   (insertion or deletion)".
:- doc(diff_item(X), "@var{X} is an insertion denoted with
   @code{ins(Pos, Elem)} meaning that @var{Elem} has to be inserted in
   position @var{Pos}, or @code{del(Pos, Elem)} meaning that @var{Elem} has
   to be removed from position @var{Pos}. It is defined as:
   @includedef{diff_item/1}").
diff_item(ins(P, _)) :- nnegint(P).
diff_item(del(P, _)) :- nnegint(P).
