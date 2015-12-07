:- module(example1,[p/1],[regrtestdecls]).

:- regr_texec p(1.0).

p(X) :- display_list(['test_output: ',X]),nl.
