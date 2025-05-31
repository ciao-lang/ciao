:- module(arrays_fix, [], [fsyntax]).
:- include(library(arrays/arrays_itf)). % implements arrays_itf

%! \title Immutable arrays

% This are the predicates used if we want to use fixed arrays without setarg 

:- export(new_array_fix/2).
new_array_fix(N,array_fix(Data)) :-
    % TODO: limited to N < 256 (without changes in engine)
    functor(Data,data,N).

% (hook)
% It gives the length of an array
array_length(Array,N) :- nonvar(Array), Array = array_fix(Data), !, 
    functor(Data,data,N).

% (hook)
% It returns the element in the nth position of the array
get_elem(Array,Index) := Elem :- nonvar(Array), Array = array_fix(Data), !,
    I is Index + 1, % To make it zero-based index
    arg(I, Data, Elem).

% (hook)
% It changes the nth element of the list
replace_elem(Array,Index,Val) := Array2 :- nonvar(Array), Array = array_fix(_), !,
    I is Index + 1, % To make it zero-based index
    Array2 = ~replace_elem_array_fix(Array,I,Val).

% It makes a copy of the array changing the nth element
replace_elem_array_fix(array_fix(Data),Index,Val) := array_fix(Res) :-
    functor(Data,_,N),
    functor(Res,data,N),
    replace_elem_array_aux(Data,Res,1,N,Index,Val).

replace_elem_array_aux(_Data,_Res,I,N,_Index,_Val) :- 
    I > N, !.
replace_elem_array_aux(Data,Res,I,N,Index,Val) :- 
    Index = I , !, 
    arg(Index,Res,Val),
    I1 is I + 1,
    replace_elem_array_aux(Data,Res,I1,N,Index,Val).
replace_elem_array_aux(Data,Res,I,N,Index,Val) :- 
    arg(I,Data,Elem), 
    arg(I,Res,Elem), 
    I1 is I + 1,
    replace_elem_array_aux(Data,Res,I1,N,Index,Val).

