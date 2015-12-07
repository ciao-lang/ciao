:- package(default).
:- module(default,_,[fuzzy,expander]).


:- default(p/2,unknown).
%:- default(p/2,fail).
%:- default(p/2,[0.4,0.8]).


p(a,0.4):~ .
p(b,0.3):~ .
