% A simple server (an active module)
:- module(simple_server,[population/2, shutdown/0],[]).

population(belgium,9).
population(france,52).
population(germany,80).
population(italy,60).
population(spain,42).
population(sweden,8).
population(united_kingdom,55).

shutdown :- halt.
