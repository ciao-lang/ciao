/*-------------------------------------------------------------------------*/
/* Benchmark (Finite Domain)                                               */
/*                                                                         */
/* Name           : bridge.pl                                              */
/* Title          : bridge scheduling problem                              */
/* Original Source: P. Van Hentenryck's book and                           */
/*                  COSYTEC (version used in "Overview of a CHIP Compiler")*/
/* Date           : October 1994                                           */
/*                                                                         */
/* Find a scheduling that minimizes the time to build a 5-segment bridge.  */
/*                                                                         */
/* Solution:                                                               */
/*                                                                         */
/* Optimal (End=104)                                                       */
/*                                                                         */
/* [[start,0,0],[a1,4,3],[a2,2,13],[a3,2,7],[a4,2,15],[a5,2,1],[a6,5,38],  */
/*  [p1,20,9],[p2,13,29],[ue,10,0],[s1,8,10],[s2,4,18],[s3,4,29],[s4,4,42],*/
/*  [s5,4,6],[s6,10,46],[b1,1,18],[b2,1,22],[b3,1,33],[b4,1,46],[b5,1,10], */
/*  [b6,1,56],[ab1,1,19],[ab2,1,23],[ab3,1,34],[ab4,1,47],[ab5,1,11],      */
/*  [ab6,1,57],[m1,16,20],[m2,8,36],[m3,8,44],[m4,8,52],[m5,8,12],         */
/*  [m6,20,60],[l1,2,30],[t1,12,44],[t2,12,56],[t3,12,68],[t4,12,92],      */
/*  [t5,12,80],[ua,10,78],[v1,15,56],[v2,10,92],[k1,0,42],[k2,0,80],       */
/*  [stop,0,104]]                                                          */
/*-------------------------------------------------------------------------*/

:- module(bridge, [main/0, bridge/2, setup/3, choice/2], [clpfd, assertions]).
 
:- use_module(engine(io_basic), [nl/0]).
:- use_module(library(write), [write/1]).
:- use_module(engine(runtime_control), [statistics/2]).
:- use_module(library(lists), [member/2, reverse/2]).
:- use_module(library(clpfd/fd_constraints)).
:- use_module(library(clpfd/fd_term)).

main :-
    statistics(runtime, _),
    bridge(Ld, End),
    statistics(runtime, [_, Y]),
    write(Ld),
    nl,
    write(End),
    nl,
    write('time : '),
    write(Y),
    nl.


bridge(K, End) :-
    setup(K, End, Disj),
    minimize(choice(Disj, K), End).


setup(K, Ende, Disj) :-
    jobs(L),
    make_vars(L, K),
    member([stop, _, Ende], K),
    precedence(M),
    make_precedence(M, K),
    max_nf(M1),
    make_max_nf(M1, K),
    max_ef(M2),
    make_max_ef(M2, K),
    min_af(M3),
    make_min_af(M3, K),
    min_sf(M4),
    make_min_sf(M4, K),
    min_nf(M5),
    make_min_nf(M5, K),
    resources(R),
    make_disj(R, K, [], Disj1),
    reverse(Disj1, Disj).

choice(Disj, K) :-
    disjunct(Disj),
    mylabel(K).

make_vars([], []).
make_vars([H|T], [[H, D, A]|R]) :-
    duration(H, D),
    domain([A], 0, 200),
    make_vars(T, R).

make_precedence([], _).
make_precedence([[A, B]|R], L) :-
    member([A, Ad, Aa], L),
    member([B, _Bd, Ba], L),
    Ba #>= Aa + Ad,
    make_precedence(R, L).

make_max_nf([], _).
make_max_nf([[A, B, C]|R], L) :-
    member([A, Ad, Aa], L),
    member([B, _Bd, Ba], L),
    C1 is C + Ad,
    Ba #=< Aa + C1,
    make_max_nf(R, L).

make_max_ef([], _).
make_max_ef([[A, B, C]|R], L) :-
    member([A, Ad, Aa], L),
    member([B, Bd, Ba], L),
    C1 is Ad + C - Bd,
    Ba #=< Aa + C1,
    make_max_ef(R, L).

make_min_af([], _).
make_min_af([[A, B, C]|R], L) :-
    member([A, _Ad, Aa], L),
    member([B, _Bd, Ba], L),
    Ba #>= Aa + C,
    make_min_af(R, L).

make_min_sf([], _).
make_min_sf([[A, B, C]|R], L) :-
    member([A, _Ad, Aa], L),
    member([B, Bd, Ba], L),
    C1 is C - Bd,
    Ba #=< Aa + C1,
    make_min_sf(R, L).

make_min_nf([], _).
make_min_nf([[A, B, C]|R], L) :-
    member([A, Ad, _Aa], L),
    member([B, _Bd, Ba], L),
    C1 is C + Ad,
    Ba #>= Ad + C1,
    make_min_nf(R, L).

make_disj([], _R, D, D).
make_disj([[_H, R]|T], K, Din, Dout) :-
    el_list(R, K, R1),
    make_disj1(R1, Din, D1),
    make_disj(T, K, D1, Dout).

make_disj1([], D, D).
make_disj1([H|T], Din, Dout) :-
    make_disj2(H, T, Din, D1),
    make_disj1(T, D1, Dout).

make_disj2(_H, [], D, D).
make_disj2([A, B], [[C, D]|S], Din, Dout) :-
    make_disj2([A, B], S, [[A, B, C, D]|Din], Dout).

el_list([], _, []).
el_list([H|T], L, [[A, D]|S]) :-
    member([H, D, A], L),
    el_list(T, L, S).

disjunct([]).
disjunct([[A, B, C, D]|R]) :-
    disj(A, B, C, D),
    disjunct(R).

disj(Aa, Ad, Ba, _Bd) :-
    Ba #>= Aa + Ad.

disj(Aa, _Ad, Ba, Bd) :-
    Aa #>= Ba + Bd.


mylabel([]).
mylabel([[_A, _Ad, Aa]|R]) :-
    labeling([], [Aa]),
    mylabel(R).




/*


            DATA


*/

jobs([start, a1, a2, a3, a4, a5, a6, p1, p2, ue, s1, s2, s3, s4, s5, s6, b1, b2, b3, b4, b5, b6, ab1, ab2, ab3, ab4, ab5, ab6, m1, m2, m3, m4, m5, m6, l1, t1, t2, t3, t4, t5, ua, v1, v2, k1, k2, stop]).

duration(start, 0).
duration(a1, 4).
duration(a2, 2).
duration(a3, 2).
duration(a4, 2).
duration(a5, 2).
duration(a6, 5).
duration(p1, 20).
duration(p2, 13).
duration(ue, 10).
duration(s1, 8).
duration(s2, 4).
duration(s3, 4).
duration(s4, 4).
duration(s5, 4).
duration(s6, 10).
duration(b1, 1).
duration(b2, 1).
duration(b3, 1).
duration(b4, 1).
duration(b5, 1).
duration(b6, 1).
duration(ab1, 1).
duration(ab2, 1).
duration(ab3, 1).
duration(ab4, 1).
duration(ab5, 1).
duration(ab6, 1).
duration(m1, 16).
duration(m2, 8).
duration(m3, 8).
duration(m4, 8).
duration(m5, 8).
duration(m6, 20).
duration(l1, 2).
duration(t1, 12).
duration(t2, 12).
duration(t3, 12).
duration(t4, 12).
duration(t5, 12).
duration(ua, 10).
duration(v1, 15).
duration(v2, 10).
duration(k1, 0).
duration(k2, 0).
duration(stop, 0).

precedence([[start, a1], [start, a2], [start, a3], [start, a4], [start, a5], [start, a6], [start, ue], [a1, s1], [a2, s2], [a5, s5], [a6, s6], [a3, p1], [a4, p2], [p1, s3], [p2, s4], [p1, k1], [p2, k1], [s1, b1], [s2, b2], [s3, b3], [s4, b4], [s5, b5], [s6, b6], [b1, ab1], [b2, ab2], [b3, ab3], [b4, ab4], [b5, ab5], [b6, ab6], [ab1, m1], [ab2, m2], [ab3, m3], [ab4, m4], [ab5, m5], [ab6, m6], [m1, t1], [m2, t1], [m2, t2], [m3, t2], [m3, t3], [m4, t3], [m4, t4], [m5, t4], [m5, t5], [m6, t5], [m1, k2], [m2, k2], [m3, k2], [m4, k2], [m5, k2], [m6, k2], [l1, t1], [l1, t2], [l1, t3], [l1, t4], [l1, t5], [t1, v1], [t5, v2], [t2, stop], [t3, stop], [t4, stop], [v1, stop], [v2, stop], [ua, stop], [k1, stop], [k2, stop]]).

max_nf([[start, l1, 30], [a1, s1, 3], [a2, s2, 3], [a5, s5, 3], [a6, s6, 3], [p1, s3, 3], [p2, s4, 3]]).

min_sf([[ua, m1, 2], [ua, m2, 2], [ua, m3, 2], [ua, m4, 2], [ua, m5, 2], [ua, m6, 2]]).

max_ef([[s1, b1, 4], [s2, b2, 4], [s3, b3, 4], [s4, b4, 4], [s5, b5, 4], [s6, b6, 4]]).

min_nf([[start, l1, 30]]).

min_af([[ue, s1, 6], [ue, s2, 6], [ue, s3, 6], [ue, s4, 6], [ue, s5, 6], [ue, s6, 6]]).


resources([[crane, [l1, t1, t2, t3, t4, t5]], [bricklaying, [m1, m2, m3, m4, m5, m6]], [schal, [s1, s2, s3, s4, s5, s6]], [excavator, [a1, a2, a3, a4, a5, a6]], [ram, [p1, p2]], [pump, [b1, b2, b3, b4, b5, b6]], [caterpillar, [v1, v2]]]).



