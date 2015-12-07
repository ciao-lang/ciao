:- package(persdb_mysql).

:- use_module(library(persdb_mysql/persdb_mysql_rt)).

% NOTE: priority like 'persdb' (incompatible?)
:- load_compilation_module(library(persdb_sql_common/persdb_sql_tr)).
:- add_sentence_trans(persdb_sql_tr:sql_persistent_tr/2, 1110).
:- add_goal_trans(persdb_sql_tr:sql_goal_tr/2, 1110). %JCF

:- use_package(assertions).
:- use_package(det_hook).

%% Data predicate to declare a predicate as a "sql persistent" one
:- multifile('$is_sql_persistent'/3).
:- discontiguous('$is_sql_persistent'/3).

%% Data predicate used to define "sql persistent locations"
:- multifile sql_persistent_location/2.
:- data sql_persistent_location/2.
:- discontiguous('sql_persistent_location'/2).

%% Data predicates used to keep the status information about the sessions and the queries : .......

%% Data predicates used by the pl2sql compiler 
:- multifile([sql__relation/3,sql__attribute/4]).
:- data([sql__relation/3,sql__attribute/4]).
:- discontiguous(sql__relation/3).
:- discontiguous(sql__attribute/4).
