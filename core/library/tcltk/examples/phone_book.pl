:- module(phone_book,[main/0]).

:- use_package(persdb).

:- use_module(library(aggregates)).
:- use_module(library(tcltk)).

:-export(phone_book_search/2).
:-export(phone_book_store/2).

persistent_dir(phb_dir,'./pers').

:- persistent(phb/2, phb_dir).

phone_book_search(N,T) :-
     findall(Y, phb(Y,_), N),
     findall(Z, phb(_,Z), T).

phone_book_store(N,T) :-
     assertz_fact(phb(N,T)).



main :- 
	tk_new([name('Ciao+TclTk - Phone_book')],I),
	tcl_eval(I,'source phone_book.tcl',_),
	menu(I).

menu(I) :-
	tcl_eval(I,[show_menu],_),
	tk_next_event(I,Event),
	tcl_eval(I,[clear_board_menu],_),
	( Event = name -> name_recall(I)
	  ;( Event = store -> store(I)
	   ; closedown(I)
	   )
	).

closedown(X) :- tcl_delete(X).

name_recall(I) :- 
	phone_book_search(N,T),
	tcl_eval(I,[show_solution_search,br(N),br(T)],_),
	tk_next_event(I,Event),
	( Event = quit -> closedown(I)
	  ; ( Event = menu -> tcl_eval(I,[clear_name_recall],_),menu(I)
	    ; fail
	    )
	).

store(I) :- 
	tcl_eval(I,[show_solution_store],_),
	tk_next_event(I,Event),
	( Event = quit -> closedown(I)
	  ; ( Event = menu -> tcl_eval(I,[clear_store],_), menu(I)
	    ; call(Event), tcl_eval(I,[clear_store],_), store(I)
	    )
	).
