:- module(classic_predicates, [], [assertions]).

:- doc(title,"Classic Prolog predicates").

:- doc(author, "Daniel Cabeza").
:- doc(author, "Jose F. Morales").
:- doc(author, "Manuel Hermenegildo").

:- doc(module, "This module reexports the predicates required by the
   @concept{classical Prolog} package.").

:- reexport(library(aggregates)).
:- reexport(library(read)).
:- reexport(library(write)).
:- reexport(library(operators), [op/3, current_op/3]).
%
:- if(defined(optim_comp)).
:- else.
:- reexport(library(iso_char)).
:- reexport(library(iso_misc)).
:- endif.
%
:- reexport(library(format)).
:- reexport(library(lists),
    [member/2, append/3, delete/3, select/3, nth/3, last/2,reverse/2, length/2]).
:- reexport(library(sort)).
:- reexport(library(between)).
%
:- if(defined(optim_comp)).
:- reexport(compiler(dynload), [use_module/1, use_module/2, ensure_loaded/1]).
:- else.
:- reexport(library(compiler), [use_module/1, use_module/2, ensure_loaded/1]).
:- endif.
%
:- reexport(library(system)).
%
:- if(defined(optim_comp)).
:- else.
:- reexport(library(dec10_io)).
:- reexport(library(old_database)).
:- endif.
%
:- reexport(library(ttyout)).
