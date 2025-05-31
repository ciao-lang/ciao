:- module(arrays_log, [], [fsyntax]).
:- include(library(arrays/arrays_itf)). % implements arrays_itf

%! \title Extensible arrays with logarithmic access
%
%  \module Extensible arrays with logarithmic access operations

:- use_module(library(logarrays)).

% NOTE: using `nonvar(Array), Array = array(_,_)` instead of
%   `is_array/1` for better indexing. Make sure to change it
%   if internal representation in `arrays` change.

:- export(new_array_log/1).
new_array_log := ~new_array.

% (hook)
% It gives the length of an array
array_length(Array,N) :- nonvar(Array), Array = array(_,_), !,
    logarrays:array_length(Array,N).

% (hook)
% It returns the element in the nth position of the array
get_elem(Array,Index) := ~get_elem_array(Array,Index) :-
     nonvar(Array), Array = array(_,_), !.

get_elem_array(Array,Index) := Elem :- % assume: is_array(Arry)
    Elem = ~aref(Index,Array).

% It changes the nth element of the list
replace_elem(Array,Index,Val) := ~replace_elem_array(Array,Index,Val) :- nonvar(Array), Array = array(_,_), !.

replace_elem_array(Array,Index,Val) := Res :- % assume: is_array(Arry)
    Res = ~aset(Index,Array,Val).

