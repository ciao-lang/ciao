:- module(classic_predicates, [], [assertions]).

:- doc(title,"Classic Prolog predicates").

:- doc(author, "Daniel Cabeza").
:- doc(author, "Jose F. Morales").
:- doc(author, "Manuel Hermenegildo").

:- doc(module, "This module reexports the predicates required by the
   @concept{classical Prolog} package").

:- reexport(library(aggregates)).
:- reexport(library(read)).
:- reexport(library(write)).
:- reexport(library(operators), [op/3, current_op/3]).
:- reexport(library(iso_char)).
:- reexport(library(iso_misc)).
:- reexport(library(format)).
:- reexport(library(lists),
        [member/2, append/3, delete/3, select/3, nth/3, last/2,reverse/2, length/2]).
:- reexport(library(sort)).
:- reexport(library(between)).
:- reexport(library(compiler), [use_module/1, use_module/2, ensure_loaded/1]).
:- reexport(library(system)).
:- reexport(library(dec10_io)).
:- reexport(library(old_database)).
:- reexport(library(ttyout)).
