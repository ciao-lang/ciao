% Gauge additions

insert_counters(true, true) :- !.
insert_counters(B, ProfB) :- profile_body(B, 1, _, ProfB).

profile_body((A,B), Gn0, Gn, (ProfA,ProfB)) :- !,
	profile_body(A, Gn0, Gn1, ProfA),
	profile_body(B, Gn1, Gn, ProfB).
profile_body((A ; B), Gn0, Gn, ProfDisj) :- !, 
	Gn is Gn0 + 1,
	profile_disjunct(A, left, ProfA),
	profile_disjunct(B, right, ProfB),
	profile_goal((ProfA ; ProfB), Gn0, ProfDisj).
profile_body(Goal, Gn0, Gn, ProfGoal) :-
	Gn is Gn0 + 1,
	profile_goal(Goal, Gn0, ProfGoal).

profile_disjunct(fail, right, fail) :- !.
profile_disjunct((A;B), right, (ProfA;ProfB)) :- !,
	profile_disjunct(A, left, ProfA),
	profile_disjunct(B, right, ProfB).
profile_disjunct((A->B), _, (ProfA -> ProfB)) :- !,
	profile_body(A, 1, Gn, ProfA),
	profile_body(B, Gn, _, ProfB).
profile_disjunct(A, _, ProfA) :-
	profile_body(A, 1, _, ProfA).

profile_goal(Goal, Gn, ('PROFILE POINT'(body_goal(Gn,body)),Goal)).

%  Peephole optimizer for counters.  The basic idea is to pull counter
%  instructions forward twords the beginning of the clause, eliminating
%  the instuction if you can make it bump up against another counter.
%
%  peep_counter(
%    Atom indicating if the code has counter instructions in it,
%    Initial code
%    Counter model for code,
%    Instruction model for code,
%    Number of counters required in optimized code,
%    Optimized code,
%  )
%
peep_counters(unprofiled,Code,Code).
peep_counters(profiled([try(var)-1,try(indexed)-2|CntrModel],InsnModel,N),Code0,Code) :-
	peep_counters(Code0,Push,read,_,_,Models,Code1,[]),
	deposit_first_counter(Push,Code1,Code,Models,models(CntrModel,InsnModel,Cntrs)),
	assign_counters(Cntrs,3,N).

peep_counters([],none,_,_,InsnCnt,models([],[],[])) --> 
	{ init_instruction_model(InsnCnt) }.
peep_counters([Insn|Insns],Push,Mode,Dic,InsnCnt,Models) -->
	peep_counters(Insn,Insns,Push,Mode,Dic,InsnCnt0,Models),
	{ add_to_instruction_model(Insn,InsnCnt0,InsnCnt) }.

peep_counters(profile_point(PP),Insns,Push,Mode,Dic,InsnCnt,Models) --> !,
	c(S0, S),
	peep_counters(Insns,Push,Mode,Dic,InsnCnt0,Models0),
	{ instruction_model(PP,S0,S,Dic,Push,InsnCnt0,InsnCnt,Models0,Models) }.
peep_counters(bump_counter,Insns,Push,Mode,Dic,InsnCnt,Models) --> !,
	peep_counters(Insns,Push0,Mode,Dic,InsnCnt,Models),
	{ push_counter(Push0,bump_counter(_),Push) }.
peep_counters(counted_neck(A,T,R),Insns,Push,Mode,Dic,InsnCnt,Models) --> !,
	[counted_neck(A,T,R)],
	peep_counters(Insns,Push0,Mode,Dic,InsnCnt,Models0),
	{
	   push_counter(Push0,pegged_counter(T+R),Push),
	   neck_counters(nocut,T,R,Models0,Models)
	}.
peep_counters(get_list_x0,Insns,Push,Mode,Dic,InsnCnt,Models) --> !,
	peep_counters(Insns,Push0,Mode,Dic,InsnCnt,Models),
	{ push_counter(Push0,get_list_x0,Push) }.
peep_counters(get_nil_x0,Insns,Push,Mode,Dic,InsnCnt,Models) --> !,
	peep_counters(Insns,Push0,Mode,Dic,InsnCnt,Models),
	{ push_counter(Push0,get_nil_x0,Push) }.
peep_counters(get_structure_x0(S),Insns,Push,Mode,Dic,InsnCnt,Models) --> !,
	peep_counters(Insns,Push0,Mode,Dic,InsnCnt,Models),
	{ push_counter(Push0,get_structure_x0(S),Push) }.
peep_counters(get_constant_x0(K),Insns,Push,Mode,Dic,InsnCnt,Models) --> !,
	peep_counters(Insns,Push0,Mode,Dic,InsnCnt,Models),
	{ push_counter(Push0,get_constant_x0(K),Push) }.
peep_counters(get_large_x0(K),Insns,Push,Mode,Dic,InsnCnt,Models) --> !,
	peep_counters(Insns,Push0,Mode,Dic,InsnCnt,Models),
	{ push_counter(Push0,get_large_x0(K),Push) }.
peep_counters(cutb,Insns,Push,Mode,Dic,InsnCnt,Models) --> !,
	[cutb],
	swap_neck(Insns,Push,Mode,Dic,InsnCnt,Models).
peep_counters(cute,Insns,Push,Mode,Dic,InsnCnt,Models) --> !,
	[cute],
	swap_neck(Insns,Push,Mode,Dic,InsnCnt,Models).
peep_counters(cutb_x(X),Insns,Push,Mode,Dic,InsnCnt,Models) --> !,
	[cutb_x(X)],
	swap_neck(Insns,Push,Mode,Dic,InsnCnt,Models).
peep_counters(cute_x(X),Insns,Push,Mode,Dic,InsnCnt,Models) --> !,
	[cute_x(X)],
	swap_neck(Insns,Push,Mode,Dic,InsnCnt,Models).
peep_counters(call(Internal,N),Insns,Push,Mode,Dic,InsnCnt,Models) -->
	{ xfer_internal(Internal, Push) }, !,
	[call(Internal,N)],
	c(S0, S),
	peep_counters(Insns,Push0,Mode,Dic,InsnCnt,Models0),
	{ deposit_counter(Push0,S0,S,Models0,Models) }.
peep_counters(Insn,Insns,Push,Mode0,Dic,InsnCnt,Models) -->
	{ skipable(Insn,Mode0,Mode) }, !,
	[Insn],
	peep_counters(Insns,Push,Mode,Dic,InsnCnt,Models).
peep_counters(Insn,Insns,none,_,Dic,InsnCnt,Models) -->
	[Insn],
	c(S0, S),
	peep_counters(Insns,Push,read,Dic,InsnCnt,Models0),
	{ deposit_counter(Push,S0,S,Models0,Models) }.

deposit_counter(none,S,S,Counters,Counters).
deposit_counter(pegged_counter(T+R),S,S,
   models(CntrModel, InsnModel, Counters),
   models(CntrModel, InsnModel, [T,R|Counters])).
deposit_counter(internal_pred(_),S,S,Models,Models).
deposit_counter(index(Insn,Counter),[Insn|S0],S,Models0,Models) :-
	deposit_counter(Counter,S0,S,Models0,Models).
deposit_counter(bump_counter(C),[bump_counter(C)|S],S,
   models(CntrModel, InsnModel, Counters),
   models(CntrModel, InsnModel, [C|Counters])).

deposit_first_counter(pegged_counter(T+R),Code,
   [bump_counter(E)|Code],
   models([entry(fact)-_|CntrModel],InsnModel,Counters),
   models([entry(fact)-E|CntrModel],InsnModel,[E,T,R|Counters])) :-
	Code = [profile_point(entry(fact))|_], !.
deposit_first_counter(Push,Code0,Code,Models0,Models) :-
	deposit_counter(Push,Code,Code0,Models0,Models).

push_counter(Push,Insn,Index) :-
	push_index(Insn,Push,Index), !.
push_counter(index(Insn,Push0),Push1,index(Insn,Push)) :-
	push_counter(Push0,Push1,Push), !.
push_counter(none,C,C) :- !.
push_counter(internal_pred(_),pegged_counter(C),pegged_counter(C)) :- !.
push_counter(internal_pred(P),Push,internal_pred(P)) :- !,
	arg(1,Push,P).
push_counter(pegged_counter(C),bump_counter(C),pegged_counter(C)) :- !.
push_counter(bump_counter(C),Push,Push) :- !,
	arg(1,Push,C).
push_counter(P0,P1,P2) :-
	format(user_error,'ERROR: push_counter(~w,~w,~w) failed~n',[P0,P1,P2]).

push_index(get_list_x0,C,index(get_list_x0,C)).
push_index(get_nil_x0,C,index(get_nil_x0,C)).
push_index(get_constant_x0(K),C,index(get_constant_x0(K),C)).
push_index(get_large_x0(K),C,index(get_large_x0(K),C)).
push_index(get_structure_x0(S),C,index(get_structure_x0(S),C)).

neck_counters(cut,Cut, _, models(CntrModel, InsnModel, Counters),
   models([neck(cut)-Cut|CntrModel], InsnModel, Counters)).
neck_counters(nocut,Try, Retry, 
   models(CntrModel, InsnModel, Counters),
   models([neck(try)-Try,neck(retry)-Retry|CntrModel], InsnModel, Counters)).

swap_neck([],none,_,_,InsnCnt,models([],[],[])) --> 
	{ init_instruction_model(InsnCnt) }.
swap_neck([Insn|Insns],Push,Mode,Dic,InsnCnt,Models) -->
	swap_neck(Insn,Insns,Push,Mode,Dic,InsnCnt0,Models),
	{ add_to_instruction_model(Insn,InsnCnt0,InsnCnt) }.

swap_neck(profile_point(PP),Insns,Push,Mode,Dic,InsnCnt,Models) --> !,
	c(S0, S),
	swap_neck(Insns,Push,Mode,Dic,InsnCnt0,Models0),
	{ instruction_model(PP,S0,S,Dic,Push,InsnCnt0,InsnCnt,Models0,Models) }.
swap_neck(bump_counter,Insns,Push,Mode,Dic,InsnCnt,Models) --> !,
	swap_neck(Insns,Push0,Mode,Dic,InsnCnt,Models),
	{ push_counter(Push0,bump_counter(_),Push) }.
swap_neck(counted_neck(A,C,constant(0)),Insns,Push,Mode,Dic,InsnCnt,Models) --> !,
	[neck(A)],
	peep_counters(Insns,Push0,Mode,Dic,InsnCnt,Models0),
	{
	   push_counter(Push0,bump_counter(C),Push),
	   neck_counters(cut,C, constant(0), Models0, Models)
	}.
swap_neck(Insn,Insns,Push,Mode,Dic,InsnCnt,Models) -->
	peep_counters(Insn,Insns,Push,Mode,Dic,InsnCnt,Models).
	
body_goal(entry(body), _, Body, CntrModel, [entry(body)-Body|CntrModel]) :- !.
body_goal(exit(shallow), Push, Cntr + _ , CntrModel, [exit(shallow)-Cntr|CntrModel]) :-
        !,
	valid_counter(Push, Cntr).
body_goal(exit(deep), Push, _ + Cntr, CntrModel, [exit(deep)-Cntr|CntrModel]) :-
        !,
	valid_counter(Push, Cntr).
body_goal(PP, Push, _, CntrModel, [PP-Cntr|CntrModel]) :-
	valid_counter(Push, Cntr).

valid_counter(index(_,Cntr0),Cntr) :- valid_counter(Cntr0,Cntr).
valid_counter(bump_counter(Cntr),Cntr).
valid_counter(pegged_counter(Cntr),Cntr).
valid_counter(internal_pred(Pred),Pred).

init_instruction_model(insn_cnt(0,_,none)).

instruction_model(PP, S0, S, Dic, Push, InsnCnt0, InsnCnt,
   models(CntrModel0, InsnModel0, Counters),
   models(CntrModel,  InsnModel,  Counters)) :-
	body_goal(PP, Push, Dic, CntrModel0, CntrModel),
	new_instruction_model(PP, PP, S0, S, InsnCnt0, InsnModel0, InsnCnt, InsnModel).

new_instruction_model(body_goal(_,_), PP, S0, S, InsnCnt0, InsnModels0, InsnCnt, InsnModels) :-
	instruction_model(PP, S0, S, InsnCnt0, InsnModels0, InsnCnt, InsnModels).
new_instruction_model(head_sucess(_), PP, S0, S, InsnCnt0,InsnModels0,InsnCnt,InsnModels) :-
	instruction_model(PP, S0, S, InsnCnt0, InsnModels0, InsnCnt, InsnModels).
new_instruction_model(entry(_),PP,S0,S,InsnCnt0,InsnModels0,InsnCnt,InsnModels) :-
	instruction_model(PP, S0, S, InsnCnt0, InsnModels0, InsnCnt, InsnModels).
new_instruction_model(exit(_),_,S,S,InsnCnt,InsnModels,InsnCnt,InsnModels).

instruction_model(PP, [profile_point(PP)|S], S, insn_cnt(N,_,else), InsnModels, 
   insn_cnt(0,_,else), [PP-insn_cnt(N,_,none) | InsnModels]) :- !.
instruction_model(PP, [profile_point(PP)|S], S, InsnCnt, InsnModels, insn_cnt(0,_,none), [PP-InsnCnt | InsnModels]).


add_to_instruction_model(profile_point(_), InsnCnt, InsnCnt) :- !.
add_to_instruction_model(bump_counter, InsnCnt, InsnCnt) :- !.
add_to_instruction_model(endif,InsnCnt,InsnCnt) :- !.
add_to_instruction_model(else,insn_cnt(N0,_,none),insn_cnt(N,_,else)) :- !,
	N is N0 + 1.
add_to_instruction_model(builtin_1(Id,_),insn_cnt(N0,_,_),insn_cnt(N,_,builtin_pred(Id))) :- !,
	N is N0 + 1.
add_to_instruction_model(builtin_2(Id,_,_),insn_cnt(N0,_,_),insn_cnt(N,_,builtin_pred(Id))) :- !,
	N is N0 + 1.
add_to_instruction_model(builtin_3(Id,_,_,_),insn_cnt(N0,_,_),insn_cnt(N,_,builtin_pred(Id))) :- !,
	N is N0 + 1.
add_to_instruction_model(function_1(Id,_,_,_,_),insn_cnt(N0,_,_),insn_cnt(N,_,builtin_function(Id))) :- !,
	N is N0 + 1.
add_to_instruction_model(function_2(Id,_,_,_,_,_),insn_cnt(N0,_,_),insn_cnt(N,_,builtin_function(Id))) :- !,
	N is N0 + 1.
add_to_instruction_model(call(Internal,_),insn_cnt(N0,_,_),insn_cnt(N,_,InternalPred)) :-
	xfer_internal(Internal, InternalPred), !, 
	N is N0 + 1.
add_to_instruction_model(call(Pred,_),insn_cnt(N0,_,_),insn_cnt(N,_,pred(Pred))) :- !,
	N is N0 + 1.
add_to_instruction_model(execute(Internal),insn_cnt(N0,_,End),insn_cnt(N,_,InternalPred)) :-
	xfer_internal(Internal, InternalPred), !,
	execute_update(End,N0,N).
add_to_instruction_model(execute(Pred),insn_cnt(N0,_,End),insn_cnt(N,_,pred(Pred))) :- !, 
	execute_update(End,N0,N).
add_to_instruction_model(proceed,insn_cnt(N0,_,End),insn_cnt(N,_,none)) :- !, 
	execute_update(End,N0,N).
add_to_instruction_model(_,insn_cnt(N0,_,End),insn_cnt(N,_,End)) :-
	N is N0 + 1.

execute_update(none,N0,N) :- N is N0 + 1.
execute_update(else,N,N).

xfer_internal(Internal, internal_pred(Internal)) :- 
	Internal = (_-_)/_ .

%%% Table of instructions that always succeed.
%%% Counters can be moved over these instructions
%%% 
skipable(cutb,                    Mode,    Mode).
skipable(cute,                    Mode,    Mode).
skipable(cutb_x(_),               Mode,    Mode).
skipable(cute_x(_),               Mode,    Mode).
skipable(execute(_),              Mode,    Mode).
skipable(proceed,                 Mode,    Mode).
skipable(put_x_variable(_,_),     Mode,    Mode).
skipable(put_y_variable(_,_),     Mode,    Mode).
skipable(put_y_first_value(_,_),  Mode,    Mode).
skipable(put_x_value(_,_),        Mode,    Mode).
skipable(put_y_value(_,_),        Mode,    Mode).
skipable(put_x_unsafe_value(_,_), Mode,    Mode).
skipable(put_y_unsafe_value(_,_), Mode,    Mode).
skipable(put_constant(_,_),       Mode,    Mode).
skipable(put_large(_,_),          Mode,    Mode).
skipable(put_structure(_,_),      _,       write).
skipable(put_nil(_),              Mode,    Mode).
skipable(put_list(_),             _,       write).
skipable(get_y_variable(_,_),     Mode,    Mode).
skipable(get_x_variable(_,_),     Mode,    Mode).
skipable(unify_void,              Mode,    Mode).
skipable(unify_x_variable(_),     Mode,    Mode).
skipable(unify_y_variable(_),     Mode,    Mode).
skipable(unify_y_first_value(_),  write,   write).
skipable(unify_x_value(_),        write,   write).
skipable(unify_y_value(_),        write,   write).
skipable(unify_x_local_value(_),  write,   write).
skipable(unify_y_local_value(_),  write,   write).
skipable(unify_constant(_),       write,   write).
skipable(unify_large(_),          write,   write).
skipable(unify_structure(_),      write,   write).
skipable(unify_nil,               write,   write).
skipable(unify_list,              write,   write).
skipable(choice_x(_),             Mode,    Mode).
skipable(choice_y(_),             Mode,    Mode).
skipable(allocate,                Mode,    Mode).
skipable(deallocate,              Mode,    Mode).
skipable(init(_),                 Mode,    Mode).
skipable(neck(_),                 Mode,    Mode).
skipable(heapmargin_call(_,_),    Mode,    Mode).
skipable(get_nil_x0,              Mode,    Mode).
skipable(get_list_x0,             Mode,    Mode).
skipable(get_structure_x0(_),     Mode,    Mode).
skipable(get_constant_x0(_),      Mode,    Mode).
skipable(get_large_x0(_),         Mode,    Mode).



%  Assign positions in the counter vector to the 
%  counters in the code.
%
assign_counters([],N0,N) :- N is N0 -1.
assign_counters([N0|Cntrs],N0,N) :-
	N1 is N0 + 1,
	assign_counters(Cntrs,N1,N).

write_counter_model([Field-Cntr|Cnts],Pred,Stream) :-
	write_counter_model(Field,Cntr,Pred,Stream),
	write_counter_model(Cnts,Pred,Stream).
write_counter_model([],_,Stream) :-
	nl(Stream).

write_counter_model(Field,Cntr,Pred,Stream) :-
	format(Stream,'~w.~n',[counter(Pred,Field,Cntr)]).

write_instruction_model([PP-insn_cnt(Ops,Insns,InternalPred)|InsnModel],Clause,Stream) :-
	format(Stream,'~w.~n',[instruction_count(Clause,PP,Ops,Insns,InternalPred)]),
	write_instruction_model(InsnModel,Clause,Stream).
write_instruction_model([],_,Stream) :-
	nl(Stream).
