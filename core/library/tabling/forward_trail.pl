:- module(forward_trail,
	[
	    '$forward_trail'/2
	], 
	[assertions,hiord,regtypes,foreign_interface]).

:- use_module(engine(hiord_rt), 
 	[
 	    '$meta_call'/1
 	]).

:- doc('$forward_trail'/2,"@pred{$forward_trail'/2} allows untrailing
and forward trailing of consumers.").

'$forward_trail'(_Forw,Back) :- '$meta_call'(Back).

