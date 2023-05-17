:- module(fib, [fib/2], []).

% Compite the nth Fibinacci number
% Example: ?- fib(100,F).

fib(N,F):-
    fibaux(N,0,1,F).

fibaux(0, Fact, _Fpost, Fact) :- !.
fibaux(N, Fact, Fpost, F) :-
    N1 is N - 1,
    Nfib is Fact + Fpost,
    fibaux(N1, Fpost, Nfib, F).

