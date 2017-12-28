:- module(math, [sin/2, cos/2, fabs/2], [foreign_interface]).

:- true pred sin(in(X),go(Y)) :: c_double * c_double + (foreign,returns(Y)).
:- true pred cos(in(X),go(Y)) :: c_double * c_double + (foreign,returns(Y)).
:- true pred fabs(in(X),go(Y)) :: c_double * c_double + (foreign,returns(Y)).

:- extra_compiler_opts(['-O2']).
:- extra_compiler_opts('LINUXi686',['-ffast-math']).
:- extra_compiler_opts('LINUXx86_64',['-ffast-math']).
:- use_foreign_library('LINUXi686', m).
:- use_foreign_library('LINUXx86_64', m).

