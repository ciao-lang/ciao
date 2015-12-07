:- module(liu,_,[fuzzy]).

tall(santiago,1) :~ .
tall(tomas,0.9) :~ .
tall(juan,0.8) :~ .
tall(blas,0.6) :~ .

%use_big_shoes(X,M) :-
%	tall(X,Ma),
%	{M .=<. Ma * 0.9}.


use_big_shoes(X,M):~ '=>'(prod,tall(X,Ma),big_shoes(X,MZ),M).

big_shoes(X,0.9) :~ .
