:- module(par2_3,
        [
            m/0
	],
	[]).

:- use_package(and_sim_plan).

m :- p(1,2), a(_) '&' b(_).

a(1) :- c(_) '&' d(_).
a(2).
a(3).

b(1).
b(2).
b(3).

p(1,2).
 %% p(1,2).

c(1).
c(2).

d(1).
d(2).