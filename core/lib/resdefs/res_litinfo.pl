:- module(_, [get_litinfo/4, litinfo/1,
		litinfo_get_lit/2, litinfo_get_approx/2, litinfo_get_extra/2],
	    [assertions, regtypes]).

:- use_package(library(resdefs/resources_decl)).

:- use_module(library(resdefs/resources_types)).

:- pred get_litinfo/4 :: % rtcheck -- EMM
	term * approx * term * litinfo #
"Construct a litinfo structure using the first arguments, which are
the clause key, a literal, the literal number and a list of modes.".

get_litinfo(Literal, Approx, Extra, litinfo${literal => Literal,
		approx => Approx, extra => Extra}).

:- pred litinfo_get_lit/2 :: litinfo * term.
litinfo_get_lit(litinfo${literal => Literal}, Literal).

:- pred litinfo_get_approx/2 :: litinfo * approx.
litinfo_get_approx(litinfo${approx => Approx}, Approx).

:- pred litinfo_get_extra/2 :: litinfo * term.
litinfo_get_extra(litinfo${extra => Extra}, Extra).

:- regtype litinfo/1.

litinfo(litinfo(Literal, Approx, Extra)) :-
	callable(Literal),
	approx(Approx),
	term(Extra).
