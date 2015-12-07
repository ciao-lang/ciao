% Common packages to the classic mode (in the shell and programs).
:- use_package(dcg).

:- use_module(library(aggregates)).
:- use_module(library(between)).
:- use_module(library(compiler), [use_module/1, use_module/2, ensure_loaded/1]).
:- use_module(library(dec10_io)).
:- use_module(library(dynamic)).
:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(old_database)).
:- use_module(library(operators), [op/3, current_op/3]).
:- use_module(library(prolog_sys)).
:- use_module(library(read)).
:- use_module(library(sort)).
:- use_module(library(system)).
:- use_module(library(ttyout)).
:- use_module(library(write)).

:- use_module(library(iso_char)).
:- use_module(library(iso_misc)).
