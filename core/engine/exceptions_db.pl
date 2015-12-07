:- module(_, _, []).

:- data catching/3, thrown/1, disabled/1.


asserta_catching(Ch, Er, Ha) :- asserta_fact(catching(Ch, Er, Ha)).
asserta_catching(Ch, Er, Ha) :- retract_fact_nb(catching(Ch, Er, Ha)), fail.

retract_catching(Ch, Er, Ha) :- retract_fact_nb(catching(Ch, Er, Ha)).
retract_catching(Ch, Er, Ha) :- asserta_fact(catching(Ch, Er, Ha)), fail.

asserta_disabled(Ref) :- asserta_fact(disabled(Ref)).
asserta_disabled(Ref) :- retract_fact_nb(disabled(Ref)), fail.

retract_disabled(Ref) :- retract_fact_nb(disabled(Ref)).
retract_disabled(Ref) :- asserta_fact(disabled(Ref)), fail.
