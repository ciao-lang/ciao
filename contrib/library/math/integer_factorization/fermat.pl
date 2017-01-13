:- module(fermat, [prime_factor/2], [assertions, isomodes, regtypes]).
%:- module(fermat, _, [assertions, isomodes, regtypes]).

:- doc(title, "Fermat Factorization").
:- doc(author, "Jose F. Morales (implementation)").

:- doc(module, "Implementation of the
   @href{http://en.wikipedia.org/wiki/Fermat's_factorization_method}{Fermat factorization}
   algorithm.").

:- pred prime_factor(N, Prime) :: int * int # "On backtracking, return
   the nontrivial prime factors of @var{N}. @var{Prime} is a number
   different than @tt{1} and @var{N} that divides @var{N}".

prime_factor(N, Prime) :-
	factorize(N, Prime0, Rest),
	( Prime0 = 1 ->
	    Prime = Rest
	; ( Prime = Prime0
	  ; prime_factor(Rest, Prime)
	  )
	).

:- pred factorize(N, Prime, Rest) :: int * int * int # "Obtain a
   nonnontrivial prime factor @var{Prime} of @var{N}, and the rest
   @var{Rest} (exists @var{Rest} so that @var{Prime} * @var{Rest} =
   @var{N})".

factorize(1, _, _) :- !, fail.
factorize(N, Prime, Rest) :-
	N /\ 1 =:= 0, !, % the number is even
	Prime = 2,
	Rest is N >> 1.
factorize(N, Prime, Rest) :-
	ceil_isqrt(N, A),
	B is A*A-N,
	factorize_(B, A, N, Prime, Rest).

factorize_(B, A, N, Prime, Rest) :-
%	display(factorize_(B, A, N, Prime, Rest)), nl,
	isqrt(B, S),
	( S * S =\= B -> % B is not a square
	    A1 is A + 1,
	    B2 is A1 * A1 - N,
	    factorize_(B2, A1, N, Prime, Rest)
	; isqrt(B, Bsq),
	  Prime is A - Bsq,
	  Rest is N // Prime
	).

ceil_isqrt(N, CeilSqrt) :-
	isqrt(N, R),
	( R * R =:= N ->
	    CeilSqrt = R
	; CeilSqrt is R + 1
	).

%:- export(isqrt/2).
:- pred isqrt(N, R) :: int * int # "@var{R} is the integer square root
   of @var{N} (which may be a bigint)".
isqrt(N, R) :- integer(R), !, N is R * R.
isqrt(N, R) :-
	% "bit" starts at the highest power of four <= the argument.
	highest_four_power(N, Bit),
	isqrt_(Bit, N, 0, R).

isqrt_(0, _N, R0, R) :- !, R = R0.
isqrt_(Bit0, N0, R0, R) :-
	RBit is R0 + Bit0,
        ( N0 >= RBit -> % TODO: If N0 > RBit we provide the floor; we would want the ceil
	    N is N0 - RBit,
	    R1 is (R0 >> 1) + Bit0
	; R1 is R0 >> 1,
	  N = N0
	),
	Bit is Bit0 >> 2,
	isqrt_(Bit, N, R1, R).

%:- export(highest_four_power/2).
% highest_four_power(Num, Bit): Bit is the highest 4 power =< than Num
highest_four_power(Num, Bit) :-
	highest_four_power_(Num, 1, Bit).

highest_four_power_(Num, Bit0, Bit) :-
	Bit1 is Bit0 << 2,
	( Bit1 > Num ->
	    Bit = Bit0
	; highest_four_power_(Num, Bit1, Bit)
	).

:- doc(bug, "We do not have an isqrt algorithm for bignums. So this
   module implements one (which may be inefficient if compared with
   the rest of bignum operations)").

