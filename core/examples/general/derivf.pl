:- module(derivf, [df/3], [fsyntax]).

% Compute derivatives symbolically (using fsyntax)
% (reduced version, no simplification) 

% Examples:
%
% ?- df((x + 1) * ((x**2 + 2) * (x**3 + 3)), x, E).
%
% ?- df(((((((((x / x) / x) / x) / x) / x) / x) / x) / x) / x, x, E).
%
% ?- df(log(log(log(log(log(log(log(log(log(log(x)))))))))), x, E).
%
% ?- df(((((((((x * x) * x) * x) * x) * x) * x) * x) * x) * x, x, E).

df(X, _)      := 0 :- number(X), !.
df(X, X)      := 1 :- !.
df(U+V, X)    := ~df(U,X) + ~df(V,X).
df(U-V, X)    := ~df(U,X) - ~df(V,X).
df(U*V, X)    := ~df(U,X) * V  +  U * ~df(V,X).
df(U/V, X)    := ( ~df(U,X) * V - U * ~df(V,X) ) / V**2.
df(U**N, X)   := N * (U**(~(N-1))) * ~df(U,X) :- number(N).
df(log(U), X) := ~df(U,X) / U.

