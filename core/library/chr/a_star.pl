%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Author:	Tom Schrijvers
% Email:	Tom.Schrijvers@cs.kuleuven.be
% Copyright:	K.U.Leuven 2004
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(a_star,
	[
		a_star/6
	]).

:- use_module(library(chr/binomialheap)).

:- use_module(library(chr/chr_find)).

:- use_module(library(chr/hprolog)).

%% SICStus begin
%% :- use_module(library(terms),[term_variables/2]).
%% SICStus end

%% Ciao begin
:- meta_predicate a_star(?,?,goal,?,goal,?).
%% Ciao end


a_star(DataIn,O,FinalData,E1^E2^E3,ExpandData,DataOut) :-
	a_star_node(DataIn,0,InitialNode),
	empty_q(NewQueue),
	insert_q(NewQueue,InitialNode,Queue),
	a_star_aux(Queue,O^FinalData,E1^E2^E3^ExpandData,EndNode),
	a_star_node(DataOut,_,EndNode).

a_star_aux(Queue,FinalData,ExpandData,EndNode) :-
	delete_min_q(Queue,Queue1,Node), 
	( final_node(FinalData,Node) ->
		Node = EndNode
	;
		expand_node(ExpandData,Node,Nodes),
		insert_list_q(Nodes,Queue1,NQueue),
		a_star_aux(NQueue,FinalData,ExpandData,EndNode)
	).


final_node(D^Call,Node) :-
	a_star_node(Data,_,Node),	
	term_variables(Call,Vars),
	chr_delete(Vars,D,DVars),
	copy_term_nat(D^Call-DVars,Data^NCall-DVars),
	call(NCall).

expand_node(D^Ds^C^Call,Node,Nodes) :-
	a_star_node(Data,Score,Node),
	term_variables(Call,Vars),
	chr_delete(Vars,D,DVars0),
	chr_delete(DVars0,Ds,DVars1),
	chr_delete(DVars1,C,DVars),
	copy_term_nat(D^Ds^C^Call-DVars,Data^EData^Cost^NCall-DVars),
	term_variables(Node,NVars,DVars),	
	find_with_var_identity(ENode,NVars,(NCall,EScore is Cost + Score,a_star_node(EData,EScore,ENode)),Nodes).	

a_star_node(Data,Score,Data-Score).
