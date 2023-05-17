:- module(crypt, [crypt/1], []).

% Cryptomultiplication
% (based on code originally written by Peter Van Roy)
% Find the unique answer to:
%   OEE     Sol:    ABC     348
%  x EE            x DE    x 28
%  ----            ----    ----
%  EOEE            FGHI    2784
%  EOE             JKL     696
%  ----            ----    ----
%  OOEE            MNOP    9744
%
% where E=even, O=odd.

% ?- crypt([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]).

crypt([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]) :-
    odd(A), even(B), even(C), even(E),
    mult([C, B, A], E, [I, H, G, F | X]),
    lefteven(F), odd(G), even(H), even(I), zero(X), lefteven(D),
    mult([C, B, A], D, [L, K, J | Y]),
    lefteven(J), odd(K), even(L), zero(Y),
    sum2([I, H, G, F], [0, L, K, J], [P, O, N, M | Z]),
    odd(M), odd(N), even(O), even(P), zero(Z).
    % write(' '), write(A), write(B), write(C), nl,
    % write('  '), write(D), write(E), nl,
    % write(F), write(G), write(H), write(I), nl,
    % write(J), write(K), write(L), nl,
    % write(M), write(N), write(O), write(P), nl.

sum2(AL, BL, CL) :-
    sum3(AL, BL, 0, CL).
  
sum3([A | AL], [B | BL], Carry, [C | CL]) :- !,
    X0 is A + B,
    X is X0 + Carry,
    C is X mod 10,
    NewCarry is X // 10,
    sum3(AL, BL, NewCarry, CL).
sum3([], BL, 0, BL) :- !.
sum3(AL, [], 0, AL) :- !.
sum3([], [B | BL], Carry, [C | CL]) :- !,
    X is B + Carry,
    NewCarry is X // 10,
    C is X mod 10,
    sum3([], BL, NewCarry, CL).
sum3([A | AL], [], Carry, [C | CL]) :- !,
    X is A + Carry,
    NewCarry is X // 10,
    C is X mod 10,
    sum3([], AL, NewCarry, CL).
sum3([], [], Carry, [Carry]).

mult(AL, D, BL) :- mult2(AL, D, 0, BL).

mult2([], _, Carry, [C, Cend]) :-
    C is Carry mod 10,
    Cend is Carry // 10.
mult2([A | AL], D, Carry, [B | BL] ) :-
    X0 is A * D,
    X is X0 + Carry,
    B is X mod 10,
    NewCarry is X // 10,
    mult2(AL, D, NewCarry, BL).

zero([]).
zero([X | L]) :-
    X = 0,
    zero(L).

odd(1).
odd(3).
odd(5).
odd(7).
odd(9).

even(0).
even(2).
even(4).
even(6).
even(8).

lefteven(2).
lefteven(4).
lefteven(6).
lefteven(8).

