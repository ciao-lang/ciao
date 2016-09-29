
:- class(nulqueen).

:- inherit_class(queen).

:- export([ attacks/2, next/0, solution/1, solve/0 ]).

nulqueen:- % just to avoid the compile-time error
	queen(0,0,none).

solve.

next:- fail.

attacks(_ThisLine,_ThisColumn):- fail.

solution([]).
