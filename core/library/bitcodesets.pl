
:- module(bitcodesets,
	[ bitset_empty/1,
	  bitset_equal/2,
	  bitset_member/2,
	  not_bitset_member/2,
	  bitset_member_SoS/2,
	  bitset_subset/2,
	  bitset_union/3,
	  bitset_union_list/2,
	  bitset_union_list_list_s/4,
	  bitset_intersect/3,
	  bitset_subtract/3,
	  bitset_subtract_list/3,
	  bitset_size/2,
	%
	  bitcode_to_listofbitcode/2,
	  bitcode_to_set/2,	
	  bitcode_to_set/3,
	  bitcode_to_set_array/2,
	  int_to_bitcode/2,
	  lbitcode_to_llist/2,
	  lbitcode_to_llistS/2,
	  llist_to_lbitcode/2,
	  set_to_bitcode/2

	],
	[ assertions
	]).

:- use_module(library(sort), [sort/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- doc(title,"Bit-coded-set operations").
:- doc(module,"Only finite sets and complements of finite sets.").
:- doc(author,"Ann Mulkers (first version)").
:- doc(author,"Francisco Bueno (port to Ciao)").
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
:- mode
	bitset_empty(+),
	bitset_equal(+,+),
	bitset_member(+,+),		
	bitset_member_SoS(+,+),		
	bitset_subset(+,+),		
	bitset_union(+,+,-),		
	bitset_union_list(+,-),		
	bitset_union_list_list_s(+,+,+,-),
	bitset_intersect(+,+,-),	
	bitset_subtract(+,+,-),		
	bitset_subtract_list(+,+,-),	
	bitset_size(+,-).		
*/

bitset_empty(0). 

bitset_equal(Bc,Bc).

bitset_member(Bc,Bcs) :-  Bc /\ Bcs =\= 0.

not_bitset_member(Bc,Bcs) :-  0 is Bc /\ Bcs.

bitset_member_SoS(Bc,[H|_]) :-   Bc /\ H =\= 0,!.
bitset_member_SoS(Bc,[_|Tail]) :-  bitset_member_SoS(Bc,Tail).

bitset_subset(Subset,Set) :- 	     Subset is Subset /\ Set.

bitset_intersect(Bcs1,Bcs2,Bcs3) :-  Bcs3 is Bcs1 /\ Bcs2.

bitset_subtract(Bcs1,Bcs2,Bcs3) :-   Bcs3 is Bcs1 /\ \(Bcs2).

bitset_union(Bcs1,Bcs2,Bcs3) :-      Bcs3 is Bcs1 \/ Bcs2.

%-------------------------------------------------------------------------
% bitset_union_list(+,-)
% bitset_union_list(LBcs,Union)
%-------------------------------------------------------------------------
:- push_prolog_flag(multi_arity_warnings,off).

bitset_union_list(LBcs,Union):-
	bitset_union_list(LBcs,0,Union).

bitset_union_list([],Union,Union). 
bitset_union_list([H|List],Acc,Union) :- 
	NewAcc is H \/ Acc,
	bitset_union_list(List,NewAcc,Union).

:- pop_prolog_flag(multi_arity_warnings).

%-------------------------------------------------------------------------
% bitset_union_list_list_s(+,+,+,-)
% bitset_union_list_list_s(LBcs,Bcs,LAcc,LUnion)
%-------------------------------------------------------------------------
bitset_union_list_list_s([],_,LAcc,LUnion):-
	sort(LAcc,LUnion).
bitset_union_list_list_s([H|List],Bcs,LAcc,LUnion) :- 
	NH is H \/ Bcs,
	bitset_union_list_list_s(List,Bcs,[NH|LAcc],LUnion). 

%-------------------------------------------------------------------------
% bitset_subtract_list(+,+,-)
% bitset_subtract_list(LBcs,Bcs,LSubtract)
%-------------------------------------------------------------------------
bitset_subtract_list([],_,[]). 
bitset_subtract_list([H|List],Bcs,[NH|NList]) :- 
	NH is H /\ \(Bcs),
	bitset_subtract_list(List,Bcs,NList). 

%-------------------------------------------------------------------------
% bitset_size(+,+,-)
% bitset_sixe(Bcs,Num,Size)
%-------------------------------------------------------------------------
:- push_prolog_flag(multi_arity_warnings,off).

bitset_size(0,Size,Size) :- !.
bitset_size(Bcs,Num,Size) :- 
	(Bcs /\ 1)  =\=  0,!,
	NNum is Num + 1,
	NBcs is Bcs >> 1,
	bitset_size(NBcs,NNum,Size).
bitset_size(Bcs,Num,Size) :- 
	NBcs is Bcs >> 1,
	bitset_size(NBcs,Num,Size).

bitset_size(0,0)  :- !.
bitset_size(Bcs,'$infty')  :-  Bcs < 0,!.
bitset_size(Bcs,Result) :-
	bitset_size(Bcs,0,Result).

:- pop_prolog_flag(multi_arity_warnings).

/*
:- mode 
	int_to_bitcode(+,-),			
	set_to_bitcode(+,-),			
	bitcode_to_listofbitcode(+,?),		
	bitcode_to_set(+,?),			
	bitcode_to_set(+,+,?),		
	llist_to_lbitcode(+,-),			
	lbitcode_to_llist(+,-).			
*/

%-------------------------------------------------------------------------
% Transforms an integer into its bitcode. The definition (rather than the
% predicate) is usually used for efficiency.
%-------------------------------------------------------------------------

int_to_bitcode(X,Y) :-  Y is 1 << X .

%-------------------------------------------------------------------------
% set_to_bitcode(+,-)
% set_to_bitcode(LNum,Bcs)
% Transforms a list of integers into its bitcode representation
%-------------------------------------------------------------------------
:- push_prolog_flag(multi_arity_warnings,off).

set_to_bitcode(LNum,Bcs):-
	set_to_bitcode(LNum,0,Bcs).

set_to_bitcode([],Bcs,Bcs)  .
set_to_bitcode([H|T],Acc,Bcs) :-  
	Hc is 1 << H,			%	int_to_bitcode(H,Hc),
	Acc0 is Hc \/ Acc,
	set_to_bitcode(T,Acc0,Bcs).

:- pop_prolog_flag(multi_arity_warnings).

%-------------------------------------------------------------------------
% llist_to_lbitcode(+,-)
% llist_to_lbitcode(LLNum,LBcs)
% Transfomrs each list of numbers in LLNum into its bitcode representation
%-------------------------------------------------------------------------
llist_to_lbitcode([],[]).
llist_to_lbitcode([H|T], [Hc|Tc] ) :-
	set_to_bitcode(H, Hc),
	llist_to_lbitcode(T, Tc ) .

%-------------------------------------------------------------------------
% bitcode_to_set(+,-)
% bitcode_to_set(Bcs,LNum)
% Transforms a bitcode representation into its list of integers 
%-------------------------------------------------------------------------
:- push_prolog_flag(multi_arity_warnings,off).

bitcode_to_set(0,[])  :- ! .
bitcode_to_set(C,S) :-
	bitcode_to_set(C,0,S) .

bitcode_to_set(0,_,[]) :- ! .
bitcode_to_set(Code,Num,LNum):-
	( (Code /\ 1)  =\=  0 ->
	    LNum = [Num|Tail]
	;   LNum = Tail),
	NNum is Num + 1,
	NCode is Code >> 1, 
	bitcode_to_set(NCode,NNum,Tail) .

:- pop_prolog_flag(multi_arity_warnings).

%-------------------------------------------------------------------------
% lbitcode_to_llistS(+,-)
% lbitcode_to_llistS(LBcs,LLNum)
% Transfomrs each bitcode representation in LBcs into its list of integers
%-------------------------------------------------------------------------
lbitcode_to_llistS([],[]).
lbitcode_to_llistS([Hc|Tc], [H|T] ) :-
	bitcode_to_set(Hc, H),
	lbitcode_to_llistS(Tc, T ) .

%-------------------------------------------------------------------------
% ??
%-------------------------------------------------------------------------

lbitcode_to_llist(ps(InSHr,ExShr),ps(InSHr_ec,ExShr_ec)) :- !,
	bitcode_to_set(InSHr,InSHr_ec),
	lbitcode_to_llistS(ExShr,ExShr_ec) .
lbitcode_to_llist(List_i,List_o) :- lbitcode_to_llistS(List_i,List_o) .

%-------------------------------------------------------------------------
% bitcode_to_listofbitcode(+,-)
% bitcode_to_listofbitcode(Bcs,LBcs)
% Transforms a bitcode into the list of bitcodes which form it. It is like
% bitcode to_set but the output is in bitcode rather than integers
%-------------------------------------------------------------------------
:- push_prolog_flag(multi_arity_warnings,off).

bitcode_to_listofbitcode(0,[])  :- ! .
bitcode_to_listofbitcode(C,S) :-
	bitcode_to_listofbitcode(C,1,S) .

bitcode_to_listofbitcode(0,_,[]) :- ! .
bitcode_to_listofbitcode(Code,Num,List) :- 
	( (Code /\ 1)  =\=  0 ->
	    List = [Num|Tail]
	;   List = Tail),
	NNum is Num << 1,
	NCode is Code >> 1, 
	bitcode_to_listofbitcode(NCode,NNum,Tail) .

:- pop_prolog_flag(multi_arity_warnings).

%-------------------------------------------------------------------------
% bitcode_to_set_array(+,-)
% bitcode_to_set_array(Array1, Array2)
%-------------------------------------------------------------------------
% Each bitcode element in Array1 is transformed into its list of bitcodes
% and added to Array2.
%-------------------------------------------------------------------------
:- push_prolog_flag(multi_arity_warnings,off).

bitcode_to_set_array(array($(A0,A1,A2,A3),Size), array($(L0,L1,L2,L3),Size)) :-
	N is Size-2,
	bitcode_to_set_array(0, N, 0, A0, L0), 
	bitcode_to_set_array(1, N, 0, A1, L1), 
	bitcode_to_set_array(2, N, 0, A2, L2), 
	bitcode_to_set_array(3, N, 0, A3, L3).

bitcode_to_set_array(_K, _N, _M, $, $) :- ! .
bitcode_to_set_array(_K, 0, _M, Item, Item_lic) :- !,
	bitcode_to_listofbitcode( Item, Item_lic ) .
bitcode_to_set_array(K, N, M, $(A0,A1,A2,A3), $(L0,L1,L2,L3) ) :-
	N1 is N-2,
	M1 is (K+M)<<2,
	bitcode_to_set_array(0, N1, M1, A0, L0),
	bitcode_to_set_array(1, N1, M1, A1, L1),
	bitcode_to_set_array(2, N1, M1, A2, L2),
	bitcode_to_set_array(3, N1, M1, A3, L3).

:- pop_prolog_flag(multi_arity_warnings).
