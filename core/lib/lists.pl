:- module(lists, [
    nonsingle/1, append/3, reverse/2, reverse/3, delete/3, %member/2, 
    delete_non_ground/3, 
    select/3, length/2, nth/3, add_after/4, add_before/4,
    % list/1, list/2, 
    list1/2, dlist/3, list_concat/2, list_insert/2, insert_last/3, 
    contains_ro/2, contains1/2, nocontainsx/2, last/2, list_lookup/3,
    list_lookup/4,
    intset_insert/3, intset_delete/3, intset_in/2, intset_sequence/3,
    intersection/3, union/3, difference/3, sublist/2, subordlist/2,
    equal_lists/2, list_to_list_of_lists/2, powerset/2, cross_product/2,
    sequence_to_list/2
], [assertions,isomodes,hiord,nativeprops]).

:- doc(title, "List processing").

:- doc(author, "The Ciao Development Team").

:- doc(module, "This module provides a set of predicates for list
       processing.").

:- pred nonsingle(X) # "@var{X} is not a singleton.".

nonsingle([_]) :- !, fail.
nonsingle(_).

:- reexport(engine(basic_props), [member/2]). % TODO: reverse import relation?
%%% Now in engine(basic_props)
% :- pred member(X,Xs) # "@var{X} is an element of (list) @var{Xs}.".
% 
% member(X, [X|_]).
% member(X, [_Y|Xs]):- member(X, Xs).

%:- pred append(Xs, Ys)    # "@var{Ys} is the elements of @var{Xs} appended.".
%append([], []).
%append([L|Ls], R) :-
%    appends(Ls, L, R).
%appends([], L, L).
%appends([L|Ls], L2, R) :-
%    append(L2, L, L3),
%    appends(Ls, L3, R).

:- if(defined(optim_comp)). % TODO: fix?
% :- pred append(Xs,Ys,Zs) # "@var{Zs} is @var{Ys} appended to @var{Xs}.".
:- else.
% This case is needed for open-lists
:- success append(Xs,Ys,Zs) => list(Xs)
# "@var{Zs} is @var{Ys} appended to @var{Xs}.".
:- endif.

% More specific cases:
:- pred append(Xs,Ys,Zs)::(list(Xs), list(Ys), list(Zs)).

:- success append(Xs,Ys,Zs): (list(Xs), list(Ys)) => list(Zs).
:- success append(Xs,Ys,Zs): list(Zs) => (list(Xs), list(Ys)).
:- success append(Xs,Ys,Zs) => mshare([[Xs,Ys,Zs],[Xs,Zs],[Ys,Zs]]).
:- success append(Xs,Ys,Zs) : (ground(Xs), ground(Ys)) => ground(Zs).
:- success append(Xs,Ys,Zs) : ground(Zs) => (ground(Xs), ground(Ys)).

:- trust comp append(Xs,Ys,Zs) + sideff(free).
:- trust comp append(Xs,Ys,Zs) : list(Xs) + eval.
:- trust comp append(Xs,Ys,Zs) : list(Zs) + eval.

append([], L, L).
append([E|Es], L, [E|R]) :- append(Es, L, R).

:- pred reverse(Xs,Ys) : list * term => list * list
   # "Reverses the order of elements in @var{Xs}.".
:- trust comp reverse/2 + sideff(free).
:- trust comp reverse(Xs,_Ys) : list(Xs) + eval.

reverse(Xs,Ys):- reverse(Xs,[],Ys).

:- pred reverse(A,B,C) # "Reverse the order of elements in @var{A},
   and append it with @var{B}.".
:- trust comp reverse/3 + sideff(free).
:- trust comp reverse(Xs,Ys,Zs) : (list(Xs), list(Ys)) + eval.

reverse([], L, L).
reverse([E|Es],L,R) :- reverse(Es,[E|L],R).

:- pred delete(L1,E,L2) => (list(L1), list(L2))
   # "@var{L2} is @var{L1} without the occurrences of @var{E}.".
:- trust comp delete/3 + sideff(free).
:- trust comp delete(L1,E,L2) : (ground(L1), ground(L2)) + eval.

delete([], _, []).
delete([Head|Tail], Element, Rest) :-
    Head==Element, !,
    delete(Tail, Element, Rest).
delete([Head|Tail], Element, [Head|Rest]) :-
    delete(Tail, Element, Rest).

:- pred delete_non_ground(L1,E,L2) => (list(L1), list(L2))
# "@var{L2} is @var{L1} without the occurrences of @var{E}. @var{E} can
be a nonground term so that all the elements in @var{L1} it unifies
with will be deleted".
:- trust comp delete_non_ground/3 + sideff(true).
:- trust comp delete_non_ground(L1,E,L2) : (ground(L1), ground(L2)) + eval.

delete_non_ground([A], _, [A]) :-
    var(A), !.
delete_non_ground([], _, []).
delete_non_ground([Head|Tail], Element, Rest) :-
    eq(Head,Element), !,
    delete_non_ground(Tail, Element, Rest).
delete_non_ground([Head|Tail], Element, [Head|Rest]) :-
    delete_non_ground(Tail, Element, Rest).

eq(A, B):- \+ \+ A = B.

:- pred select(X,Xs,Ys) # "@var{Xs} and @var{Ys} have the same
   elements except for one occurrence of @var{X}.".
:- trust comp select/3 + sideff(free).
:- trust comp select(X,Xs,Ys) : (ground(X),ground(Xs)) + eval.

select(E, [E|Es], Es).
select(E, [X|Es], [X|L]) :- select(E, Es, L).

:- pred length(L,N) : list * var => list * int + det
    # "Computes the length of @var{L}.".
:- pred length(L,N) : var * int => list * int + det
    # "Outputs @var{L} of length @var{N}.".
:- pred length(L,N) : list * int => list * int + semidet
    # "Checks that @var{L} is of length @var{N}.".
:- pred length(L,N) + nondet
    # "Generates @var{L}s and @var{N}s. @var{L} can be partially instantiated.".

% Problems with this assertion: triple disjunction = nested disjunction
%:- calls length(L,N)
%         : ((list(L), int(N)); (var(L),int(N)); (list(L), var(N))).

:- trust comp length(A,B) + native.
:- trust comp length/2 + sideff(free).
:- trust comp length(L,N) : list(L) + eval.
:- trust comp length(L,N) : integer(N) + eval.

length(L, N) :- var(N), !, llength(L, 0, N).
length(L, N) :- dlength(L, 0, N).

llength([], I, I).
llength([_|L], I0, I) :- I1 is I0+1, llength(L, I1, I).

dlength([], I, I) :- !.
dlength([_|L], I0, I) :- I0<I, I1 is I0+1, dlength(L, I1, I).

:- doc(nth(N, List, Elem), "@var{N} is the position in @var{List} of
   @var{Elem}.  @var{N} counts from one.").

:- pred nth(+int, ?list, ?term)
    # "Unifies @var{Elem} and the @var{N}th element of @var{List}.".
:- pred nth(-int, ?list, ?term)
    # "Finds the positions where @var{Elem} is in @var{List}.
      Positions are found in ascending order.".

:- trust comp nth/3 + sideff(free).
:- trust comp nth(N,L,E) : integer(N) + eval.
:- trust comp nth(N,L,E) : list(L) + eval.
:- trust comp nth(N,L,E) : (int(N), list(L)) + is_det.

nth(N, List, Elem) :-
    integer(N), !, N >= 1, nthfunc(N, List, Elem).
nth(N, List, Elem) :-
    var(N), !,
    findnth(List, Elem, 1, N).
nth(N, _, _) :-
    throw(error(type_error(integer, N), nth/3-1)).

nthfunc(1, [Elem|_], Elem) :- !.
nthfunc(N, [_|List], Elem) :-
    N1 is N-1,
    nthfunc(N1, List, Elem).

findnth([Elem|_], Elem, N, N).
findnth([_|List], Elem, N0, N) :-
    N1 is N0+1,
    findnth(List, Elem, N1, N).

:- pred add_after(+L0, +E0, +E, -L) # "Adds element @var{E}
    after element @var{E0} (or at end) to list @var{L0} returning
    in @var{L} the new list (uses term comparison).".

add_after([], _, E, [E]).
add_after([E|Es], E0, E1, NEs) :-
    E == E0, !,
    NEs = [E0,E1|Es].
add_after([E|Es], E0, E1, [E|NEs]) :-
    add_after(Es, E0, E1, NEs).

:- pred add_before(+L0, +E0, +E, -L) # "Adds element E before
   element E0 (or at start) to list L0 returning in L the new list
   (uses term comparison).".

add_before(L, E0, E, NL) :-
    add_before_existing(L, E0, E, NL), !.
add_before(L, _, E, [E|L]).

add_before_existing([E|Es], E0, E1, NEs) :-
    E == E0, !,
    NEs = [E1,E0|Es].
add_before_existing([E|Es], E0, E1, NEEs) :-
    add_before_existing(Es, E0, E1, NEs), !,
    NEEs = [E|NEs].

:- prop list1(T,X) + regtype # "@var{X} is a list of @var{Y}s of at least one element.".
:- meta_predicate list1(pred(1),?).

list1(T,[X|R]) :- 
    T(X),
    list(T,R).

:- pred dlist(List,DList,Tail) # "@var{List} is the result of
   removing @var{Tail} from the end of @var{DList} (makes a difference
   list from a list).".

dlist([], X, X).
dlist([X|Y], [X|L], T) :- dlist(Y, L, T).

:- pred list_concat(LL,L): list(list,LL) => list(L) # "@var{L} is the
   concatenation of all the lists in @var{LL}.".

list_concat([],[]).
list_concat([L|RL],Head) :- 
    dlist(L,Head,Tail),
    list_concat(RL,Tail).

:- pred list_insert(?List, +Term) # "Adds @var{Term} to the end of
   @var{List} if there is no element in @var{List} identical to
   @var{Term}.".

list_insert(List, Term) :-
    var(List), !,
    List=[Term|_].
list_insert([Term0|_], Term) :-
    Term0==Term, !.
list_insert([_|List], Term) :-
    list_insert(List, Term).

:- pred insert_last(+L0, +E, ?L) # "Adds element @var{E} at end
   of list @var{L0} returning @var{L}.".

insert_last(Xs, X, Ys):- append(Xs, [X], Ys).

:- pred contains_ro/2 # "Impure membership (does not instantiate a
   variable in its first argument.".

contains_ro([], _) :- !, fail.
contains_ro([X|_], X).
contains_ro([_|Xs], X) :- contains_ro(Xs, X).

:- pred contains1/2 # "First membership.".

contains1([X|_], X) :- !.
contains1([_|Xs], X) :- contains1(Xs, X).

:- pred nocontainsx(L, X) # "@var{X} is not identical to any
   element of @var{L}.".

nocontainsx([], _).
nocontainsx([X1|Xs], X) :- X\==X1, nocontainsx(Xs, X).

:- pred last(L,X) # "@var{X} is the last element of list @var{L}.".

last([E|L], X) :- last_aux(L, E, X).

last_aux([], E, E).
last_aux([E|L], _, X) :-
    last_aux(L, E, X).

:- pred list_lookup(List, Functor, Key, Value) # "Look up
@var{Functor}(@var{Key},@var{Value}) pair in variable ended key-value
pair list @var{L} or else add it at the end.".

list_lookup(List, Functor, Key, Value) :-
    var(List), !,
    functor(Pair, Functor, 2),
    arg(1, Pair, Key),
    arg(2, Pair, Value),
    List=[Pair|_].
list_lookup([Pair|_], Functor, Key, Value) :-
    functor(Pair, Functor, 2),
    arg(1, Pair, Key0),
    Key0==Key, !,
    arg(2, Pair, Value).
list_lookup([_|List], Functor, Key, Value) :-
    list_lookup(List, Functor, Key, Value).

:- pred list_lookup(List, Key, Value) # "Same as
   @pred{list_lookup/4}, but use @pred{-/2} as functor.".

list_lookup(List, Key, Value) :- list_lookup(List, (-), Key, Value).

% intset_... deal with ordered lists of numbers

:- pred intset_insert(A,B,Set) # "Insert the element @var{B} in
   the ordered set of numbers @var{A}.".

intset_insert([], A, Set) :- !, Set=[A].
intset_insert(Set0, A, Set) :- Set0=[D|_], A<D, !, Set=[A|Set0].
intset_insert(Set0, D, Set) :- Set0=[D|_], !, Set=Set0.
intset_insert([D|Ds], A, [D|Bs]) :- intset_insert(Ds, A, Bs).

:- pred intset_delete(A,B,Set) # "Delete from the ordered set
   @var{A} the element @var{B}.".

intset_delete([D|Ds], D, Set) :- !, Set=Ds.
intset_delete([D|Ds], A, [D|Ds1]) :- A>D, intset_delete(Ds, A, Ds1).

:- pred intset_in(E, Set) # "Succeds iff @var{E} is element of @var{Set}".

intset_in(O, [O1|Os]) :-
    (   O1<O -> intset_in(O, Os)
    ;   O=O1
    ).

:- pred intset_sequence(N,L1,L2) # "Generates an ordered set of
   numbers from 0 to @var{N}-1, and append it to @var{L1}.".

intset_sequence(0, L0, L) :- !, L=L0.
intset_sequence(N, L0, L) :- M is N-1, intset_sequence(M, [M|L0], L).

%------------------------------------------------------------------------------
% operations on two lists:

:- pred intersection(+List1,+List2,?List) 
    : (list(List1), list(List2)) => list(List) 
# "@var{List} has the elements which are both in @var{List1} and
@var{List2}.".

intersection([], _, []).
intersection([Element|Residue], List, Intersection) :-
    member(Element, List), !, 
    Intersection = [Element|Intersection1],
    intersection(Residue, List, Intersection1).
intersection([_|Residue], List, Intersection) :-
    intersection(Residue, List, Intersection).

:- pred union(+List1, +List2, -List) 
    : (list(List1), list(List2)) => list(List) 
 # "@var{List} has the elements which are in @var{List1} followed by
 the elements which are in @var{List2} but not in @var{List1}.".

union([], List2, List2).
union([Element|Residue], List, Union) :-
    member(Element, List), !,
    union(Residue, List, Union).
union([Element|Residue], List, [Element|Union]) :-
    union(Residue, List, Union).

:- pred difference(+List1, +List2, -List)
    : (list(List1), list(List2)) => list(List) 
 # "@var{List} has the elements which are in @var{List1} but not in
 @var{List2}.".

difference([], _, []) :- !.
difference([Element|Residue], List, Difference) :-
    member(Element, List), !, 
    difference(Residue, List, Difference).
difference([Element|Residue], List, [Element|Difference]) :-
    difference(Residue, List, Difference).

:- prop subordlist(?List1, +List2)
    # "@var{List2} contains all the elements of @var{List1}
       in the same order.".

subordlist([],[]).
subordlist([H|SL], [H|T]) :-
    subordlist(SL, T).
subordlist(L, [_|T]) :-
    subordlist(L, T).

:- prop sublist(?List1, +List2)
    # "@var{List2} contains all the elements of @var{List1}.".

sublist([], _).
sublist([Element|Residue], List) :-
    member(Element, List),
    sublist(Residue, List).

:- pred equal_lists(List1, List2) : (list(List1), list(List2))
# "@var{List1} has all the elements of @var{List2}, and vice versa.".

equal_lists(List1, List2) :-
    sublist(List1, List2),
    sublist(List2, List1).

:- pred list_to_list_of_lists(List,LList) 
   : list(List) => list_of_lists(LList).

:- pred list_to_list_of_lists(List,LList) 
   : list_of_lists(LList) => list(List) 
   # "@var{LList} is the list of one element lists with elements of
   @var{List}.".

list_to_list_of_lists([],[]).
list_to_list_of_lists([X|Xs],[[X]|Xss]) :-
    list_to_list_of_lists(Xs,Xss).

:- prop list_of_lists/1 + regtype.
:- export(list_of_lists/1).

list_of_lists([]).
list_of_lists([L|Xs]):-
    list(L),
    list_of_lists(Xs).

:- pred powerset(+List,?LList)
  : list(List) => list_of_lists(LList)
# "@var{LList} is the powerset of @var{List}, i.e., the list of all
    lists which have elements of @var{List}.  If @var{List} is
    ordered, @var{LList} and all its elements are ordered.".

powerset([],[]).
powerset([X|Xs],[[X]|Xss]) :-
    powerset(Xs,Yss),
    add_x(Yss,X,Yss,Xss).

add_x([],_,Zss,Zss).
add_x([Ys|Yss],X,Zss,[[X|Ys]|Xss]) :-
    add_x(Yss,X,Zss,Xss).

:- pred cross_product(+LList,?List) 
    : list_of_lists(LList) => list_of_lists(List)
    # "@var{List} is the cartesian product of the lists in
      @var{LList}, that is, the list of lists formed with one
      element of each list in @var{LList}, in the same order.".

cross_product([], [[]]).
cross_product([L1|Ls], Lds) :-
    cross_product(Ls, Lda),
    add_each_elem(L1, Lda, Lds).

add_each_elem([], _, []).
add_each_elem([X|Xs], Lda, Lds) :-
    add_elem(Lda, X, Lds, Lds_),
    add_each_elem(Xs, Lda, Lds_).

add_elem([], _X, Lds, Lds).
add_elem([L|Ls], X, [[X|L]|XLs], Lds_) :-
    add_elem(Ls, X, XLs, Lds_).

:- pred sequence_to_list(Sequence, List) # "@var{List} is the list of
    all elements in the (comma-separated) sequence @var{Sequence}.
    The use of this predicate is reversible.".

sequence_to_list(V, [V]) :- var(V), !.
sequence_to_list((E,R), [E|L]) :- L \== [], !,
    sequence_to_list(R, L).
sequence_to_list(E, [E]).

