:- package(and_sim_plan).

:- use_package(assertions).

:- doc(nodoc, assertions).

:- doc(filetype, package).

:- doc(title,"Simulation of and parallel execution").

:- doc(author,"Pablo Chico de Guzmán Huerta").
:- doc(author,"The CLIP Group").

:- doc(module, "TO BE WRITTEN").

:- doc(doinclude,aux_par/1).
:- doc(aux_par/1,"To be defined.").

:- op(950, xfy, [&]).

% TODO: this seems a clause translation instead of a term translation
:- load_compilation_module(library(and_sim_plan/and_sim_plan_tr)).
:- add_sentence_trans(and_sim_plan_tr:do_term_expansion/3, 750). % TODO: unsure about priority

:- use_module(library(and_sim_plan/and_sim_plan_rt)).
:- use_module(library(dynamic)).
:- use_module(library(odd)).
:- use_module(library(lists)).
:- use_module(library(aggregates)).
:- use_module(library(sort)).
:- use_module(library(write), [write/1]).

:- export(sim/3).

:- dynamic last_id/1.
:- dynamic current_id/1.
:- dynamic event_list/2.
:- dynamic num_max_wam_aux/1.
:- dynamic num_max_wam/1.

sim(X,_,_) :- 
	retractall(event_list(_,_)),
	retractall(last_id(_)), assert(last_id(0)), 
	retractall(current_id(_)), assert(current_id(1)),
	retractall(num_max_wam(_)), assert(num_max_wam(1)),
	retractall(num_max_wam_aux(_)), assert(num_max_wam_aux(1)),
 	par([X],[1],[1]), 
	display('Soluciones: '), write(X), nl,
	fail.

sim(_,_,_) :-
 %% 	display(insertando), nl,
	findall(event_list(Id,Event_LR),event_list(Id,Event_LR),List1),
	sort(List1,List2),
	retractall(event_list(_,_)),
	member(event_list(Id,Event_LR),List2),
	reverse(Event_LR,Event_L),
	skip_first(Event_L,Event_LOK),
 %% 	display(dupla(Id,Event_LOK)), nl,
	insert_info(Event_LOK),
	fail.

sim(_,NThreads,_) :- 
 %% 	display(precedences), nl,
 %%      	print_info, nl,
  	precedences_info(NThreads),
	fail.

sim(_,_,_) :- nl, %%display('FORMATO INTERNO'), nl,
 %%     	print_info, nl,
 %% 	display('Numero maximo de WAMS es: '), 
 %% 	num_max_wam(N),
 %% 	display(N), nl,
	fail.

sim(_,_,L) :- 
	display(simulando), nl,
  	member(all,L),
  	!,
 %%   	sim_seq_memo,
 %%   	sim_par_memo,
 %%   	sim_par,
	sim_pred_s_amadeo,
  	sim_seq,
	sim_par_back,
	sim_seq_memo,
  	free_info.
  
sim(_,_,L) :- 
 	member(seq,L),
 	sim_seq,
 	fail.
 
 %% sim(_,_,L) :- 
 %% 	member(seq_memo,L),
 %% 	sim_seq_memo,
 %% 	fail.
 %% 
 %% sim(_,_,L) :- 
 %% 	member(par_memo,L),
 %% 	sim_par_memo,
 %% 	fail.
 %% 
 %% sim(_,_,L) :- 
 %% 	member(par,L),
 %% 	sim_par,
 %% 	fail.
 %% 
sim(_,_,L) :- 
	member(amadeo,L),
	sim_pred_s_amadeo,
	fail.

sim(_,_,L) :- 
	member(par_back,L),
	sim_pred_s_amadeo,
	fail.

sim(_,_,_) :- free_info.


skip_first([start([1])|R],R) :- !.
skip_first(L,L) :- !.


w_event(Event,Id) :- 
	event_list(Id,L_Event), !,
 %% 	display('Lista previa: '), display(L_Event), nl,
	retract(event_list(Id,L_Event)),
	not_end(L_Event,Id, Event).

w_event(Event,Id) :-
	assert(event_list(Id,[Event])).

not_end([fail|R],Id,_) :- !, assert(event_list(Id,[fail|R])).
not_end(L_Event,Id,Event) :- assert(event_list(Id,[Event|L_Event])).

par(GL,IdL,IdL_OK) :-
	retract(last_id(Id)),
	get_listOK(IdL,IdL_OK,Id,0),
	current_id(Parent),
	length(IdL_OK,Size),
	retract(num_max_wam_aux(NWamOld)),
	NWamNew is NWamOld + Size - 1,
	retract(num_max_wam(NWamMax)),
	update_max_wam(NWamNew,NWamMax),
 %% 	display('Mete '), display(Parent), display(' a '), display(IdL_OK), nl,
	w_event(start(IdL_OK),Parent),
	undo((retractall(current_id(_)), assert(current_id(Parent)))),
	par_aux(GL,IdL_OK),
	retractall(current_id(_)), assert(current_id(Parent)).

par_aux([],[]).
par_aux([G|R_G],[Id|R_Id]) :-
	retractall(current_id(_)), assert(current_id(Id)),
	undo((retract(num_max_wam_aux(NWamOld)),
	      NWamNew is NWamOld - 1,
	      retract(num_max_wam(NWamMax)),
	      update_max_wam(NWamNew,NWamMax), 
	      w_event(fail,Id)
	     )),
	call(G),
	undo((retractall(current_id(_)), assert(current_id(Id)))),
	w_event(answer,Id),
	par_aux(R_G,R_Id).	

update_max_wam(NWamNew,NWamMax) :-
	NWamNew > NWamMax,
	!, assert(num_max_wam(NWamNew)),
	assert(num_max_wam_aux(NWamNew)).
update_max_wam(NWamNew,NWamMax) :-
	assert(num_max_wam(NWamMax)),
	assert(num_max_wam_aux(NWamNew)).

get_listOK([H|R],[H_OK|R_OK],Id,N) :-
	N1 is N + 1,
	H_OK is H + Id,
	get_listOK(R,R_OK,Id,N1).
	
get_listOK([],[],Id,N) :-
	retractall(last_id(_)),
	NewId is Id + N,
	assert(last_id(NewId)).
	
	


