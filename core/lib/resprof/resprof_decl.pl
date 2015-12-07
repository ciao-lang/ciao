:- package(resprof_decl).

:- meta_predicate head_cost(?,?,pred(2)).
:- discontiguous head_cost/3.

:- meta_predicate literal_cost(?,?,pred(2)).
:- discontiguous literal_cost/3.

:- discontiguous init_resource_usage/0.

:- data resource_usage_db/3.
