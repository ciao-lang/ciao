
:- ['flora.pl'].
:- ['aux_time2.pl'].

time_query :- '_$_$_flora_isa_rhs'(_,direct), fail.

:- yap_flag(tabling_mode, retroactive),
	time, (benchmark_execution ; true), halt.

