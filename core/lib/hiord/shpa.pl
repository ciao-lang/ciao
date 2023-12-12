:- package(shpa).

% Predicate abstractions with shared-by-default variables
% (`-[X1...Xn] -> Head :- Body`).

:- load_compilation_module(library(hiord/shpa_tr)).
:- add_clause_trans(shpa_tr:shpa_clause/3, 109000). % (after_mexp phase)
%:- add_clause_trans(shpa_tr:shpa_clause/3, 9000). % (after_mexp phase)

