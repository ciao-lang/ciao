:- module(prog_teresa,_,[rfuzzy, clpr]).
% :- use_module(library(write),[write/1]).

rel_ex_time(_Whatever).
rfuzzy_type_for(greater/1, [rel_ex_time/1]).
rfuzzy_default_value_for(greater/1, 0).

rfuzzy_type_for(parallelizing_prog_fuzzy_/3, [rel_ex_time/1, rel_ex_time/1, rel_ex_time/1]).
rfuzzy_default_value_for(parallelizing_prog_fuzzy_/3, 0).
parallelizing_prog_fuzzy_(Cond1,Cond2,Cond3) :~ max((greater(Cond1), greater(Cond2), greater(Cond3))).