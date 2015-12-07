
:- class(nulqueen).

:- inherit_class(queen).

:- export([ attacks/2, next/0, solution/1, solve/0 ]).

solve.

next:- fail.

attacks(_ThisLine,_ThisColumn):- fail.

solution([]).
