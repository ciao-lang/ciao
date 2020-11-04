:- package(tabling).

:- new_declaration(table/1).

% :- new_declaration(const_table_module/1).

:- new_declaration(active_tclp/0).
:- new_declaration(table_aggregate/1).
:- new_declaration(table_subsumption/0).

:- op(1150, fx, [ table, table_aggregate, table_subsumption ]).
 %% :- op(1150, fx, [ const_table_module ]).

:- if(defined('SHELL')).
:- else.
:- load_compilation_module(library(tabling/tabling_tr)).
:- add_sentence_trans(tabling_tr:do_term_expansion/3, 750). % TODO: Probably not right priority
:- endif.

:- use_module(library(tabling/tabling_rt)).

:- if(defined('SHELL')).
:- else.
:- reexport(library(tabling/tabling_rt),
        [
            print_counters/0,        % debug:    print_counters_c
            tabling_stats/0,
            set_tabling_flag/2,      % debug:    set_tabling_flag_c
            current_tabling_flag/2,  % debug:    current_tabling_flag_c
            abolish_all_tables/0
        ]).
:- endif.


