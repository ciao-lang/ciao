:- module(example2,[p/1],[regrtestdecls,assertions,rtchecks]).

:- regr_texec p(1.0).

:- pred p(N) : int(N).

p(X) :- display_list(['test_output: ',X]),nl.
