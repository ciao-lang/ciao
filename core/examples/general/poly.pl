:- module(poly, [], []).

% ?- test_poly_exp(Out).

:- export(test_poly_exp/1).
test_poly_exp(Out) :-
    test_poly(Data),
    poly_exp(10, Data, Out).

test_poly1(P) :-
    P = poly(x, [term(0,1), term(1,1)]).

test_poly2(P) :-
    P = poly(y, [term(1,1)]).

test_poly3(P) :-
    P = poly(z, [term(1,1)]).

test_poly(P) :-
    poly_add(poly(x, [term(0,1), term(1,1)]), poly(y, [term(1, 1)]), Q),
    poly_add(poly(z, [term(1,1)]), Q, P).

poly_add(poly(Var,Terms1), poly(Var,Terms2), poly(Var,Terms)) :- !,
    term_add(Terms1, Terms2, Terms).
poly_add(poly(Var1,Terms1), poly(Var2,Terms2), poly(Var1,Terms)) :-
    Var1 @< Var2, !,
    add_to_order_zero_term(Terms1, poly(Var2,Terms2), Terms).
poly_add(Poly, poly(Var,Terms2), poly(Var,Terms)) :- !,
    add_to_order_zero_term(Terms2, Poly, Terms).
poly_add(poly(Var,Terms1), C, poly(Var,Terms)) :- !,
    add_to_order_zero_term(Terms1, C, Terms).
poly_add(C1, C2, C) :-
    C is C1 + C2.

term_add([], X, X) :- !.
term_add(X, [], X) :- !.
term_add([term(E,C1)|Terms1], [term(E,C2)|Terms2], [term(E,C)|Terms]) :- !,
    poly_add(C1, C2, C),
    term_add(Terms1, Terms2, Terms).
term_add([term(E1,C1)|Terms1], [term(E2,C2)|Terms2], [term(E1,C1)|Terms]) :-
    E1 < E2, !,
    term_add(Terms1, [term(E2,C2)|Terms2], Terms).
term_add(Terms1, [term(E2,C2)|Terms2], [term(E2,C2)|Terms]) :-
    term_add(Terms1, Terms2, Terms).

add_to_order_zero_term([term(0,C1)|Terms], C2, [term(0,C)|Terms]) :- !,
    poly_add(C1, C2, C).
add_to_order_zero_term(Terms, C, [term(0,C)|Terms]).

poly_exp(0, _, 1) :- !.
poly_exp(N, Poly, Result) :-
    N1 is N/\1, N1 = 0, !,
    M is N>>1,
    poly_exp(M, Poly, Part),
    poly_mul(Part, Part, Result).
poly_exp(N, Poly, Result) :-
    M is N - 1,
    poly_exp(M, Poly, Part),
    poly_mul(Poly, Part, Result).

poly_mul(poly(Var,Terms1), poly(Var,Terms2), poly(Var,Terms)) :- !,
    term_mul(Terms1, Terms2, Terms).
poly_mul(poly(Var1,Terms1), poly(Var2,Terms2), poly(Var1,Terms)) :-
    Var1 @< Var2, !,
    mul_through(Terms1, poly(Var2,Terms2), Terms).
poly_mul(P, poly(Var,Terms2), poly(Var,Terms)) :- !,
    mul_through(Terms2, P, Terms).
poly_mul(poly(Var,Terms1), C, poly(Var,Terms)) :- !,
    mul_through(Terms1, C, Terms).
poly_mul(C1, C2, C) :-
    C is C1 * C2.

term_mul([], _, []) :- !.
term_mul(_, [], []) :- !.
term_mul([Term|Terms1], Terms2, Terms) :-
    single_term_mul(Terms2, Term, PartA),
    term_mul(Terms1, Terms2, PartB),
    term_add(PartA, PartB, Terms).

single_term_mul([], _, []).
single_term_mul([term(E1,C1)|Terms1], term(E2,C2), [term(E,C)|Terms]) :-
    E is E1 + E2,
    poly_mul(C1, C2, C),
    single_term_mul(Terms1, term(E2,C2), Terms).

mul_through([], _, []).
mul_through([term(E,Term)|Terms], Poly, [term(E,NewTerm)|NewTerms]) :-
    poly_mul(Term, Poly, NewTerm),
    mul_through(Terms, Poly, NewTerms).

