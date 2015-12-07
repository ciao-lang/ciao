:- module(example1,_,[fuzzy]).

p(X,Y,_Mup1) :~ prod q(X,_Muq1),r(Y,_Mur).
p(X,Y,_Mup2) :~ prod q(X,_Muq2),s(Y,_Mus).

q(m,0.3) :~ .

r(X,_Mur):~ t(X,_Mut).

s(n,1) :~ .

t(n,0.4) :~ .
