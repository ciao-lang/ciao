:- module(pe_types,[dyn/1,const/1,f_sig/1,i_sig/1],[assertions]).

:- doc(title,"Basic PE types declarations").

:- doc(author, "M. Zamalloa").

:- doc(module,"This module provides the basic Partial evaluation types (pe_types).").

:- use_package(regtypes).

:-regtype dyn/1.
dyn(X) :- basic_props:term(X).

:-regtype const/1.
const(X) :- basic_props:term(X).

:-regtype f_sig/1.
f_sig(X) :- basic_props:term(X).

:-regtype i_sig/1.
i_sig(X) :- basic_props:term(X).
