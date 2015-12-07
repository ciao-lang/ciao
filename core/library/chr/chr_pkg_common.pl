%:- use_package(expander).

:- set_prolog_flag( multi_arity_warnings   , off ).
:- set_prolog_flag( discontiguous_warnings , off ).

:- op(1150, fx, [constraints, chr_constraint, handler, rules] ).

:- new_declaration( constraints/1    ).
:- new_declaration( chr_constraint/1 ).
:- new_declaration( handler/1        ).
:- new_declaration( rules/1          ).

:- new_declaration( chr_option/2     ).
:- new_declaration( chr_type/1       ).

:- include(library(chr/chr_op)).

:- use_module(library(chr/chr_runtime)).

:- use_module(library(chr/hprolog)). % , [ term_variables/2 ] ).

:- use_package(attr).
:- use_module(library(attr/attr_rt), [get_attr/3, put_attr/3, del_attr/2]).

:- use_module(library(odd), [setarg/3]).
:- use_module(library(sort), [sort/2]).
:- use_module(library(write), [print/1]).
:- use_module(library(iso_misc), [compound/1]).
:- use_module(library(sets), [merge/3]).

:- set_prolog_flag( check_cycles, on ).
'chr debug_event'(_).

