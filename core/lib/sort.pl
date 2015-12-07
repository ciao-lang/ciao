% Adapted from shared code written by Richard A. O'Keefe.
% All changes by UPM CLIP Group.
:- module(sort, [sort/2, keysort/2, keylist/1, keypair/1],
	    [assertions, nortchecks, isomodes]).

:- doc(title,"Sorting lists").  

:- doc(author,"Richard A. O'Keefe (original version)").
:- doc(author,"The CLIP Group (changes and modifications)").

:- doc(module,"This module implements some sorting list
   predicates.").

:- set_prolog_flag(multi_arity_warnings, off).

:- doc(sort(List1,List2), "The elements of @var{List1} are sorted
        into the standard order (see @ref{Comparing terms}) and any
        identical elements are merged, yielding @var{List2}. The
        time and space complexity of this operation is at worst
        @var{O(N lg N)} where @var{N} is the length of @var{List1}.").

:- pred sort(+list,?list) + native
	# "@var{List2} is the sorted list corresponding to @var{List1}.".

:- test sort(A,B) : (A = [1,2,6,5,2,1]) => (B == [1,2,5,6]).

:- true comp sort(A,B) : list(A) + eval.
:- true comp sort(A,B) + sideff(free).

sort(List, Sorted) :-
	sort(List, -1, S, []), !,
	Sorted = S.
sort(List, _) :-
        list(List), !,
        throw(error(instantiation_error, sort/2-1)).
sort(NoList, _) :-
	throw(error(type_error(list,NoList), sort/2-1)).

sort([Head|Tail], Lim, Sorted, Rest) :- !,
	Qh = [Head|_],
	samrun(Tail, Qh, Qh, Run, Rest0),
	sort(Rest0, 1, Lim, Run, Sorted, Rest).
sort(Rest, _, [], Rest).

sort([Head|Tail], J, Lim, Run0, Sorted, Rest) :-
	J =\= Lim, !,
	Qh = [Head|_],
	samrun(Tail, Qh, Qh, Run1, Rest0),
	sort(Rest0, 1, J, Run1, Run2, Rest1),
	merge(Run0, Run2, Run),
	K is J<<1,
	sort(Rest1, K, Lim, Run, Sorted, Rest).
sort(Rest, _, _, Sorted, Sorted, Rest).


% samrun(List, Q1, Q2, Run, Rest)

% List is a list of elements, Rest is some tail of that list,
% Run is an ordered _set_ of the difference between List and Rest,
% Q1 is the cons containing the first element of List.
% Q2 is the last cons of Run.

samrun(Tail, _, _, _, _) :- var(Tail), !, fail.
samrun([H|Tail], QH, QT, Run, Rest) :-
	QT = [Q|_],
	compare(X, H, Q),
	samrunt(X, H, QH, QT, QH1, QT1), !,
        samrun(Tail, QH1, QT1, Run, Rest).
samrun(Rest, Run, [_], Run, Rest).

samrunh(<, H, QH, [H|QH]).
samrunh(=, _, QH, QH).

samrunt(<, H, QH, QT, QH1, QT) :-
	QH = [Q|_],
	compare(X, H, Q),
	samrunh(X, H, QH, QH1).
samrunt(=, _, QH, QT, QH, QT).
samrunt(>, H, QH, [_|QT], QH, QT) :-
	QT = [H|_].

% merge(+List, +List, -List).
merge([], Set0, Set) :- !,
	Set = Set0.
merge([O|Os], [N|Ns], Set) :- !,
	compare(C, O, N), 
	merge(C, O, Os, N, Ns, Set).
merge(Set, _, Set).

merge(<, O, [], N, Ns, [O,N|Ns]) :- !.
merge(<, O1, [O|Os], N, Ns, [O1|Set]) :-
	compare(C, O, N), 
	merge(C, O, Os, N, Ns, Set).
merge(=, _, Os, N, Ns, [N|Set]) :-
	merge(Os, Ns, Set).
merge(>, O, Os, N, [], [N,O|Os]) :- !.
merge(>, O, Os, N1, [N|Ns], [N1|Set]) :-
	compare(C, O, N), 
	merge(C, O, Os, N, Ns, Set).


:- doc(keysort(List1,List2),"@var{List1} is sorted into order
	according to the value of the @em{keys} of its elements,
	yielding the list @var{List2}. No merging takes place.  This
	predicate is @em{stable}, i.e., if an element @tt{A} occurs
	before another element @tt{B} @em{with the same key} in the
	input, then @tt{A} will occur before @tt{B} also in the
	output.  The time and space complexity of this operation is
	at worst @var{O(N lg N)} where @var{N} is the length of
	@var{List1}.").

:- pred keysort(+keylist,?keylist) + native
	# "@var{List2} is the (key-)sorted list corresponding to @var{List1}.".
%% :- trust pred keysort(+list(Arg1,keypair),?list(Arg2,keypair))
%% 	# "@var{Arg1} is the (key-)sorted list corresponding to @var{Arg2}.".

%   Keysorting.  Adapted from O'Keefe's code, but uses recursion instead of
%   an auxiliary stack.  Takes care to check validity of arguments.
%   Could be sped up if there were an inline keycompare/3.

keysort(List, Sorted) :-
	keysort(List, -1, S, []), !,
	Sorted = S.
keysort(List, _) :-
        keylist(List), !,
        throw(error(instantiation_error, keysort/2-1)).
keysort(NoList, _) :-
	throw(error(type_error(keylist,NoList), keysort/2-1)).


keysort([Head|Tail], Lim, Sorted, Rest) :- !,
	nonvar(Head),
	Head = _-_,
	Qh = [Head|_],
	samkeyrun(Tail, Qh, Qh, Run, Rest0),
	keysort(Rest0, 1, Lim, Run, Sorted, Rest).
keysort(Rest, _, [], Rest).

keysort([Head|Tail], J, Lim, Run0, Sorted, Rest) :-
	J =\= Lim, !,
	nonvar(Head),
	Head = _-_,
	Qh = [Head|_],
	samkeyrun(Tail, Qh, Qh, Run1, Rest0),
	keysort(Rest0, 1, J, Run1, Run2, Rest1),
	keymerge(Run0, Run2, Run),
	K is J<<1,
	keysort(Rest1, K, Lim, Run, Sorted, Rest).
keysort(Rest, _, _, Sorted, Sorted, Rest).

% samkeyrun(List, Q1, Q2, Run, Rest)
% List is a list of pairs K-V, Rest is some tail of that list,
% Run is an ordered _list_ of the difference between List and Rest,
% Q1 is the cons containing the first element of List.
% Q2 is the last cons of Run.

samkeyrun([Hd|Tail], QH, QT, Run, Rest) :-
	nonvar(Hd),
	Hd = H-_,
	QT = [Q-_|QT2], 
	H @>= Q, !,
	QT2 = [Hd|_],
	samkeyrun(Tail, QH, QT2, Run, Rest).
samkeyrun([Hd|Tail], QH, QT, Run, Rest) :-
	nonvar(Hd),
	Hd = H-_,
	QH = [Q-_|_],
	H @< Q, !,
	samkeyrun(Tail, [Hd|QH], QT, Run, Rest).
samkeyrun(Rest, Run, [_], Run, Rest).


% keymerge(+List, +List, -List).
keymerge([], L2, Out) :- !,
	Out = L2.
keymerge([H1|T1], L2, Out) :-	
	L2 = [K2-_|_],
	H1 = K1-_,
	K1 @=< K2, !,
	Out = [H1|Out1],
	keymerge(T1, L2, Out1).
keymerge(L1, [H2|L2], Out) :- !,
	Out = [H2|Out1],
	keymerge(L1, L2, Out1).
keymerge(List, _, List).

:- prop keylist(L) + regtype
   # "@var{L} is a list of pairs of the form @tt{Key-Value}.".

keylist([]).
keylist([_-_|KL]) :- keylist(KL).

:- doc(doinclude,keypair/1).
:- prop keypair(P) + regtype
   # "@var{P} is a pair of the form ""@tt{K-_}"",
      where @tt{K} is considered the @em{key}.".

keypair(_-_).
