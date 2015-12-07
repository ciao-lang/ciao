:- module(deriv, [der/2, derf/2, dera/2], [fsyntax]).

% Using Predefined settings

der(x)    :=  1.
der(C)    :=  0                 :- number(C).
der(A+B)  := ~der(A) + ~der(B).
der(C*A)  := C * ~der(A)        :- number(C).
der(x**N) := N * x ** ~(N-1)    :- integer(N), N>0.

% Declaring derf evaluable

:- fun_eval(derf/1).

derf(x)    :=  1.
derf(C)    :=  0                 :- number(C).
derf(A+B)  := derf(A) + derf(B).
derf(C*A)  := C * derf(A)        :- number(C).
derf(x**N) := N * x ** ~(N-1)    :- integer(N), N>0.

% Enabling arithmetic functor evaluation
% (not very useful in this case):

:- fun_eval(arith(true)).

dera(x)       :=  1.
dera(C)       :=  0                     :- number(C).
dera(^(A+B))  := ^(~dera(A) + ~dera(B)).
dera(^(C*A))  := ^(C * ~dera(A))        :- number(C).
dera(^(x**N)) := ^(N * ^(x**(N-1)))     :- integer(N), N>0.
