:- module(_,_,[functional]).

:- use_module(engine(messages_basic), [message/2]).

fib(0) := 0.
fib(1) := 1.
fib(N) := fib(N-1) + fib(N-2) :- integer(N), N > 1.

write_fib(N):-
    message(user, ['The ',N,'. Fibonacci number is: ',~fib(N),'.']).
