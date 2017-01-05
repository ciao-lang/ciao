
:- ['model_checking', 'data/trans_leader'].

time_query :- reach(systemLeader_0(5,end), par(D, E, A, B)), fail.

debug_query :- Query = reach(systemLeader_0(5,end), par(D, E, A, B)),
               call(Query),
               numbervars(Query, 0, _),
               write(Query), nl, fail.
:- time, (benchmark_execution ; true), halt.
