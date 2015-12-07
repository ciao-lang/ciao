:- package(prelude).
:- multifile '$primitive_meta_predicate'/2.
:- discontiguous '$primitive_meta_predicate'/2.
:- multifile '$current_module'/1.
:- dynamic '$current_module'/1.
:- multifile '$ldlibs'/1.
:- multifile '$multifile'/3.
:- multifile '$load_libs'/0.
:- multifile '$meta_args'/2.
:- dynamic '$meta_args'/2.
:- multifile '$u'/2.
:- multifile '$initialization'/1.
:- multifile '$on_abort'/1.
:- multifile '$imports'/5.
:- dynamic '$imports'/5.
:- multifile '$defines'/3.
:- use_module(engine(term_basic), [functor/3]).
%:- set_prolog_flag(unknown, fail).
