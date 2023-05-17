:- module(primes, [primes/2], []).

% Prime numbers up to Limit
% Example: ?- primes(100, Ps).

primes(Limit, Ps) :-
    integers(2, Limit, Is),
    sift(Is, Ps).

integers(Low, High, [Low | Rest]) :-
    Low =< High, !,
    M is Low + 1,
    integers(M, High, Rest).
integers(_,_,[]).

sift([], []).
sift([I | Is], [I | Ps]) :-
    remove(Is, I, New),
    sift(New, Ps).

remove([], _, []).
remove([I | Is], P, Nis0) :-
    IModP is I mod P,
    IModP =\= 0, !,
    Nis0 = [I | Nis],
    remove(Is, P, Nis).
remove([_I | Is], P, Nis) :-
    remove(Is, P, Nis).

