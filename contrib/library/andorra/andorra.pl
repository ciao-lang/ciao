:- package(andorra).

:- use_module(library(andorra/andorra_rt), [wakeup/2, 
	                  suspend_andorra/5, 
			  obtain_vars/2,
			  verify_det/3,
			  simplify/2]).

:- use_module(library(andorra/andorra_builtins)).

:- use_module(library(terms_vars), [varset/2]).

:- include(library(andorra/andorraops)).

:- load_compilation_module(library(andorra/andorra_tr)).
:- add_sentence_trans(andorra_tr:translation1/3, 750).
:- add_clause_trans(andorra_tr:translation2/3, 750).
