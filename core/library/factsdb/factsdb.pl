:- package(factsdb).

:- load_compilation_module(library(factsdb/factsdb_tr)).
:- add_sentence_trans(factsdb_tr:factsdb_exp/2, 750). % TODO: Probably not right priority

:- use_module(library(factsdb/factsdb_rt), 
	[asserta_fact/1, assertz_fact/1, call/1, current_fact/1,
	 retract_fact/1]).
:- meta_predicate '$factsdb$cached_goal'(fact,?,?).
:- multifile '$factsdb$cached_goal'/3.
:- discontiguous '$factsdb$cached_goal'/3.

:- redefining(asserta_fact/1).
:- redefining(assertz_fact/1).
:- redefining(current_fact/1).
:- redefining(retract_fact/1).
