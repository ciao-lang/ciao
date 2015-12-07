:- module(_, [f_nargs/2], [assertions]).

:- use_module(library(resdefs/res_litinfo)).

f_nargs(LitInfo, Cost) :-
	litinfo_get_lit(LitInfo, Pred),
	functor(Pred, _, Cost).
