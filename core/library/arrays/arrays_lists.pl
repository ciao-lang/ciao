:- module(arrays_lists, [], [fsyntax]).
:- include(library(arrays/arrays_itf)). % implements arrays_itf

%! \title Lists as arrays (O(n) access)

:- use_module(library(lists)).

% NOTE: hook guards assume that checking first functor of list is enough
%   (i.e. [] or [_|_]). Obviously, this will not be reversible.

% TODO: maybe include new_list/2 ?
% new_list(N,List) :- 
%     length(List,N).

% (hook)
array_length(Array) := N :- nonvar(Array), Array = [], !, list_length(Array, N).
array_length(Array) := N :- nonvar(Array), Array = [_|_], !, list_length(Array, N).

list_length(Array, N) :- length(Array, N).

% (hook)
get_elem(Array,Index) := ~get_elem_list(Array,Index) :- nonvar(Array), Array = [], !.
get_elem(Array,Index) := ~get_elem_list(Array,Index) :- nonvar(Array), Array = [_|_], !.

get_elem_list([X|_],0) := Elem :- !,
    Elem = X.
get_elem_list([_|Xs],I) := Elem :- I > 0,
    I1 is I - 1,
    Elem = ~get_elem_list(Xs, I1).

% (hook)
replace_elem(Array,Index,Val) := ~replace_elem_list(Array,Index,Val) :- nonvar(Array), Array = [], !.
replace_elem(Array,Index,Val) := ~replace_elem_list(Array,Index,Val) :- nonvar(Array), Array = [_|_], !.

replace_elem_list([_|Xs],0,Val) := Res :- !,
    Res = [Val|Xs].
replace_elem_list([X|Xs],I,Val) := Res :- I > 0,
    I1 is I - 1,
    Res = [X|~replace_elem_list(Xs,I1,Val)].

