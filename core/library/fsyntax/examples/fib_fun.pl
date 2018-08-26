:- module(_,_,[functional]).

:- use_module(engine(io_aux), [message/2]).

fib(0) := 0.
fib(1) := 1.
fib(N) := fib(N-1) + fib(N-2) :- integer(N), N > 1.

write_fib(N):-
        message(['The ',N,'. Fibonacci number is: ',~fib(N),'.']).
