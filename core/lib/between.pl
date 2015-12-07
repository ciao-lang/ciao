:- module(between, [between/3], [assertions, isomodes]).

:- doc(title, "Enumeration of integers inside a range").

:- doc(author, "The CLIP Group").

:- doc(module, "This modules enumerates integers between two
	numbers, or checks that an integer lies within a range").

:- doc(summary, "This modules enumerates integers between two numbers,
	or checks that an integer lies within a range.  If the second
	purpose is the needed one, it is probably wiser (faster and
	clearer) to check in the program itself using directly
	arithmetic predicates.").


:- pred between(+Min, +Max, ?N) : (num(Min), num(Max)) => int(N) #
	"@var{N} is a number which is greater than or equal to
 	@var{Min} and smaller than or equal to @var{Max}.  Both
 	@var{Min} and @var{Max} can be either integer or real
 	numbers.".

between(Min, Max, N) :- integer(N), !, N >= Min, N =< Max.
between(Min, Max, V) :- var(V),
	IntMin is ceiling(Min),
	IntMin =< Max,
	between_nd(V, IntMin, Max).

between_nd(Min, Min, _Max).
between_nd(N,   Min, Max) :-
	NMin is Min+1,
	NMin =< Max,
	between_nd(N, NMin, Max).
