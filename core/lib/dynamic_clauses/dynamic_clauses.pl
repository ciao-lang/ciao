:- package(dynamic_clauses).
:- new_declaration(dynamic_handling/0). % see c_itf:dynamic_handling/9
:- use_module(library(dynamic_clauses/dynamic_clauses_rt)).

:- if(defined('SHELL')).
:- else. % not SHELL
:- use_package(datafacts).

:- multifile '\3\mfclause'/2.
%:- meta_predicate '\3\mfclause'(primitive(fact), ?).
:- data '\3\mfclause'/2.
%:- meta_predicate '\3\clause'(primitive(fact), ?).
:- data '\3\clause'/2.

:- endif.
