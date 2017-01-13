:- module(pollard_rho, [prime_factor/2], [assertions, isomodes, regtypes]).

:- doc(title, "Pollard's rho Factorization").
:- doc(author, "Jose F. Morales (implementation)").

:- doc(module, "Implementation of the
   @href{http://en.wikipedia.org/wiki/Pollard%27s_rho_algorithm}{Pollard's rho}
   integer factorization algorithm. 

   The algorithm is @bf{probabilistic}: it may not terminate or fail to find
   soem prime factors.").

:- pred prime_factor(N, Prime) :: int * int # "On backtracking, return
   the nontrivial prime factors of @var{N}. @var{Prime} is a number
   different than @tt{1} and @var{N} that divides @var{N}".

:- doc(bug, "This algorithm depends on a pseudo-random function
       (@pred{f/2}). It may not able to factorize some composite
       numbers. In order to fix it, add a @tt{isprime} test and change
       the @var{C} constant of the @pred{f/2} function in each call to
       @pred{factorize/3}").

prime_factor(N, Prime) :-
	factorize(N, Prime0, Rest),
	( Rest = 1 ->
	    Prime = Prime0
	; ( Prime = Prime0
	  ; prime_factor(Rest, Prime)
	  )
	).

:- pred factorize(N, Prime, Rest) :: int * int * int # "Obtain a
   prime number @var{Prime} that divides @var{N} (exists @var{Rest} so
   that @var{Prime} * @var{Rest} = @var{N})".

factorize(N, Prime, Rest) :-
	% Initial values x(i) and x(2*i) for i = 0.
	factorize_(N, 2, 2, Prime, Rest).

factorize_(N, X, Y, Prime, Rest) :-
	% Find x(i+1) and x(2*(i+1))
	f(X, N, X2),
	f(Y, N, Y1), f(Y1, N, Y2),
	Diff is X2 - Y2,
	( Diff < 0 -> Diff2 is -Diff ; Diff2 = Diff ),
        D is gcd(Diff2, N),
	( D =\= 1 ->
	    Prime = D,
	    Rest is N // D
	; factorize_(N, X2, Y2, Prime, Rest)
	).

% A pseudo-random function modulo N
f(X, N, R) :-
	C = 3,
	R is (X * X + C) mod N.
