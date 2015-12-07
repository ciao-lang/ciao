:- module(fd_bounds, 
	[
	    absolute_upper_bound/1,
	    absolute_lower_bound/1,
	    absolute_bounds/2
	]).

absolute_upper_bound(67108863).      % 2**26 - 1
absolute_lower_bound(-67108864).     % -2**26
absolute_bounds(U,L) :-
	absolute_upper_bound(U),
	absolute_lower_bound(L).
