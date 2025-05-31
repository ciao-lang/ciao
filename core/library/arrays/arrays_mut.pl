:- module(arrays_mut, [], [fsyntax]).
:- include(library(arrays/arrays_itf)). % implements arrays_itf

%! \title Mutable arrays
%
%  \module Mutable arrays using (backtrackable) `setarg/3`

:- use_module(library(odd)).

:- export(new_array_mut/2).
% Creates a new array of free variables with the given length
new_array_mut(N,array_mut(Data)) :-
    % TODO: limited to N < 256 (without changes in engine)
    functor(Data,data,N).

% (hook)
% It gives the length of an array
array_length(Array,N) :- nonvar(Array), Array = array_mut(Data), !,
    functor(Data,data,N).

% (hook)
% It returns the element in the nth position of the array
get_elem(Array, Index) := Elem :- nonvar(Array), Array = array_mut(Data), !,
    I is Index + 1, % To make it zero-based index
    arg(I, Data, Elem).

% (hook)
replace_elem(Array,Index,Val) := Array2 :- nonvar(Array), Array = array_mut(Data), !,
    Array2 = Array, % (not changed)
    I is Index + 1, % To make it zero-based index
    % uses setarg to change the nth element
    setarg(I, Data, Val).

