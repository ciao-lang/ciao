:- module(assoc,
	[ empty_assoc/1,
	  assoc_to_list/2,
	  is_assoc/1,
	  min_assoc/3,
	  max_assoc/3,
	  gen_assoc/3,
	  get_assoc/3,
	  get_assoc/5,
	  get_next_assoc/4,
	  get_prev_assoc/4,
	  list_to_assoc/2,
	  ord_list_to_assoc/2,
	  map_assoc/2,
	  map_assoc/3,
	  map/3,
	  foldl/4,
	  put_assoc/4,
	  put_assoc/5,
	  add_assoc/4,
          update_assoc/5,
	  del_assoc/4,
	  del_min_assoc/4,
	  del_max_assoc/4
%	  tabla_tiempos/5,
	],
	[assertions, basicmodes, hiord, regtypes]).

%:- use_module(library(prolog_sys)).
%:- use_module(library(between)).

:- use_module(library(hiordlib), 
	[
	    map/3,
	    foldl/4
	]).

:- use_module(library(lists), 
	[
	    length/2,
	    append/3
	]).

:- doc(title,"Association between key and value").

:- doc(author,"Pablo Chico").
:- doc(author,"Manuel Carro").

:- doc(module,"This library implements a table.  It takes its name
          from the classical \"association lists\".  It allows storing
          a set of values and a key for each value, such that the
          values can later be accessed through these keys. These keys
          could not be ground terms, but they could not be
          instantiated later (so, this implementation unify with '=='
          instead of '='). The implementation uses a dynamically
          changing data structure for efficiency. When there are few
          elements the data structure used is a list of pairs. When
          the number of elements stored goes beyond some number, an
          AVL tree is used. There is a certain level of hysteresis so
          that no repeated data structure conversions occur when the
          number of elements is close to the threshold.").

:- push_prolog_flag(multi_arity_warnings,off).

%if change_tree(N) then when assoc consists of N or more elements, 
						%list --> avl_tree
change_tree(45). %45
change_list(25). %25

:- regtype assoc_table(Assoc) 
     # "@var{Assoc} is a associations beetwen keys and values.".

%% assoc_table(Assoc) :- is_assoc(Assoc).

%% MCL: I am afraid is_assoc/1 does not define a regular type (a reg. type
%% is quite restricted).  Let us use a simpler definition.
%% Also, I'd remove the list and tree atoms at the end and use two
%% different wrapper functors, assoc_list/2 and assoc_avl/2 instead.

assoc_table(assoc_table(L, N, list)) :- 
        int(N),
        list(L).
assoc_table(assoc_table(AVL, N, tree)):-
        int(N),
        avl_shape(AVL).

:- regtype avl_shape(Avl) 
     # "@var{Avl} has the shape of a binary tree with data to maintain
       the balance of an AVL tree.".

avl_shape(nil).
avl_shape(avl(_Key, _Value, Left, Right, Bal)):-
        balance(Bal),
        avl_shape(Left),
        avl_shape(Right).

:- regtype balance(Avl).

balance(-1).
balance(0).
balance(1).
        

:- pred is_assoc(+Assoc) : assoc_table(Assoc)
     # "True if @var{Assoc} is an @tt{assoc_table}.".

is_assoc(assoc_table(L,N,list)) :-
	change_tree(MAX),
	N =< MAX,
	is_ord_pairs(L),
	length(L,N).

is_assoc(assoc_table(avl(K,_,L,R,H),N,tree)) :-
	change_list(MIN),
	N >= MIN,
	is_AVL(L,_,K,HL,NumL),
	is_AVL(R,K,_,HR,NumR),
	H is HR - HL,
	is_balanced(H,HL,HR,_),
	N is NumL + NumR + 1.

is_ord_pairs([]).
is_ord_pairs([K-_|Rest]) :-
	is_ord_pairs_aux(K,Rest).

is_ord_pairs_aux(_,[]).
is_ord_pairs_aux(KPrev,[K-_|Rest]) :-
 	compare(<,KPrev,K),
 	is_ord_pairs_aux(K,Rest).
 


is_AVL(nil,_,_,0,0).
is_AVL(avl(K,_,L,R,H),KMin,KMax,Deep,N) :-
	greaterthan(K,KMin),
	lessthan(K,KMax),
	is_AVL(L,KMin,K,HL,NumL),
	is_AVL(R,K,KMax,HR,NumR),
	H is HR - HL,
	is_balanced(H,HL,HR,Deep),
	N is NumL + NumR + 1.

greaterthan(_,X) :- var(X).
greaterthan(X,Y) :- nonvar(Y), compare(>, X, Y).

lessthan(_,X) :- var(X).
lessthan(X,Y) :- nonvar(Y), compare(<, X, Y).

is_balanced(-1,HL,_,H) :- H is HL + 1.
is_balanced(0,HL,_,H) :- H is HL + 1.
is_balanced(1,_,HR,H) :- H is HR + 1.

:- pred empty_assoc(+Assoc) : assoc_table(Assoc)
   # "True if @var{Assoc} is an empty @tt{assoc_table}.".

:- pred empty_assoc(-Assoc) : assoc_table(Assoc)
   # "@var{Assoc} is an empty @tt{assoc_table}.".

empty_assoc(assoc_table([],0,list)).

:- regtype key(K)   # "@var{K} is a valid key in a @tt{assoc_table}.".
key(_).   

:- regtype value(V) # "@var{V} is a valid value in a @tt{assoc_table}.".
value(_).

:- pred get_assoc(+K,+Assoc,+V) : 
     (key(K), assoc_table(Assoc), value(V))
     # "True if @var{V} is the value associated to the key @var{K} in
       the assoc_table @var{Assoc}.".

:- pred get_assoc(+K,+Assoc,-V) : 
     (key(K), assoc_table(Assoc), value(V))
     # "@var{V} is the value associated to the key @var{K} in the
       assoc_table @var{Assoc}.".


get_assoc(K,assoc_table(Assoc,_,Type),V) :- 
	(
	    Type == tree ->
	    get_assoc_avl(Assoc,K,V)
	;
	    get_assoc_list(Assoc,K,V)
	).

%get_assoc if assoc is a list
get_assoc_list([K1-V|_Rest],K2,V) :- K1 == K2, !.
get_assoc_list([K1-_|Rest],K2,V) :- 
	compare(>,K2,K1), 
	get_assoc_list(Rest,K2,V).

%get_assoc if assoc is a avl_tree
get_assoc_avl(avl(K,V,L,R,_), Key, Val) :-
	compare(Rel, Key, K),
	get_assoc_avl_cmp(Rel, Key, Val, V, L, R).

get_assoc_avl_cmp(<, Key, Val, _, L, _) :- get_assoc_avl(L, Key, Val).
get_assoc_avl_cmp(=, _, Val, Val, _, _).
get_assoc_avl_cmp(>, Key, Val, _, _, R) :- get_assoc_avl(R, Key, Val).

:- regtype is_member(M) # "@var{M} is no or yes(value).".
is_member(no).
is_member(yes(_)).

:- pred del_min_assoc(+Assoc,-K,-V,-NewAssoc) : 
     (assoc_table(Assoc), key(K), value(V), assoc_table(NewAssoc))
     # "@var{Assoc} and @var{NewAssoc} define the same finite function
       except that @var{Assoc} associates @var{K} with @var{V} and
       @var{NewAssoc} doesn't associate @var{K} with any value and
       @var{K} precedes all other keys in @var{Assoc}.".

del_min_assoc(Assoc,K,V,NewAssoc) :-
	min_assoc(Assoc,K,V),
	del_assoc(K,Assoc,_,NewAssoc).

:- pred del_max_assoc(+Assoc,-K,-V,-NewAssoc) : 
     (assoc_table(Assoc), key(K), value(V), assoc_table(NewAssoc))
     # "@var{Assoc} and @var{NewAssoc} define the same finite function
       except that @var{Assoc} associates @var{K} with @var{V} and
       @var{NewAssoc} doesn't associate @var{K} with any value and
       @var{K} is preceded by all other keys in @var{Assoc}.".

del_max_assoc(Assoc,K,V,NewAssoc) :-
	max_assoc(Assoc,K,V),
	del_assoc(K,Assoc,_,NewAssoc).

:- pred del_assoc(+K,+Assoc1,-V,-Assoc2) : 
     (key(K), assoc_table(Assoc1), value(V), assoc_table(Assoc2))
     # "Delete in @var{Assoc1} the key @var{K} to give
       @var{Assoc2}. If the key @var{K} does not belong to the
       @var{Assoc1} then @var{Member} is unified with no and
       @var{Assoc1} and @var{Assoc2} are unified.  Otherwise
       @var{Assoc2} is the result of deleting the key @var{K} and
       its associated @var{Value}, and @var{Member} is unified with
       yes(Value).".


del_assoc(K,assoc_table(Assoc_Old,N_Old,Type_Old),V,assoc_table(Assoc_New,N_New,Type_New)) :- 
	change_list(N_Limit),
	compare(Rel,N_Old,N_Limit),
	del_assoc_cmp(Rel,Type_Old,Type_New,Assoc_Old,K,Assoc_New,V),
	new_N(yes(V),delete,N_Old,N_New).

del_assoc_cmp(=,Type_Old,list,Assoc_Old,K,Assoc_New,V) :-
	!,
	(
	    Type_Old == tree ->
	    del_assoc_avl(Assoc_Old,K,Assoc_Temp,V,_),
	    assoc_to_list_avl(Assoc_Temp,[],Assoc_New)
	;
	    del_assoc_list(Assoc_Old,K,Assoc_New,V)
	).
	    

del_assoc_cmp(_,list,list,Assoc_Old,K,Assoc_New,V) :-
	del_assoc_list(Assoc_Old,K,Assoc_New,V).


del_assoc_cmp(_,tree,tree,Assoc_Old,K,Assoc_New,V) :-
	del_assoc_avl(Assoc_Old,K,Assoc_New,V,_).

%del_assoc if assoc is a list
del_assoc_list([K1-V1|Rest],K2,Result,Member) :- 
	compare(Rel,K1,K2),
	del_assoc_list_cmp(Rel,[K1-V1|Rest],K2,Result,Member).

del_assoc_list_cmp(<,[K1-V1|Rest1],K2,[K1-V1|Rest2],Member) :- 
	del_assoc_list(Rest1,K2,Rest2,Member).  
del_assoc_list_cmp(=,[_-V|Rest],_,Rest,V).
del_assoc_list_cmp(>,L,_,L,no).

%del_assoc if assoc is a avl_tree
del_assoc_avl(avl(K,V,L,R,H), Key, Result, Member, Eq) :-
	compare(Rel, Key, K),
	del_assoc_avl_cmp(Rel, avl(K,V,L,R,H), Key, Result, Member, Eq).

del_assoc_avl_cmp(<, avl(K,V,L,R,H), Key, Result, Member, Eq1) :- 
	del_assoc_avl(L, Key, ABB, Member, Eq2),
	New_H is H + 1,
	new_FE_5(Eq2,New_H,avl(K,V,ABB,R,H),Result,delete,Eq1).

del_assoc_avl_cmp(>, avl(K,V,L,R,H), Key, Result, Member, Eq1) :- 
	del_assoc_avl(R, Key, ABB, Member, Eq2),
	New_H is H - 1,
	new_FE_5(Eq2,New_H,avl(K,V,L,ABB,H),Result,delete,Eq1).

del_assoc_avl_cmp(=, avl(_,V,nil,nil,_), _, nil, V,yes).
del_assoc_avl_cmp(=, avl(K,V,avl(LK,LV,nil,nil,_),nil,-1), _, Result, V, Eq) :-
	del_assoc_avl(avl(LK,LV,nil,avl(K,V,nil,nil,0),1), K, Result, _, Eq).
del_assoc_avl_cmp(=, avl(K,V,L,R,H), _, Result, V, Eq) :-
	change_by_follow_higher(R,K,Higher_K,Higher_V,Change),
	del_assoc_avl(Change,K,ABB,_,Eq2),
	New_H is H - 1,
	new_FE_5(Eq2,New_H,avl(Higher_K,Higher_V,L,ABB,H),Result,delete,Eq).

change_by_follow_higher(avl(K,V,nil,nil,_),K_del,K,V,avl(K_del,V,nil,nil,0)).
change_by_follow_higher(avl(K,V,nil,avl(RK,RV,nil,nil,RH),_),K_del,K,V,avl(RK,RV,avl(K_del,V,nil,nil,RH),nil,-1)).
change_by_follow_higher(avl(K,V,L,R,H), K_del,K_Change,V_Change,avl(K,V,Result,R,H)) :-
	change_by_follow_higher(L,K_del,K_Change,V_Change,Result).
	    

:- pred get_assoc(+K,+Assoc,-Old,-NewAssoc,+New) : (key(K),
     assoc_table(Assoc), value(Old), assoc_table(NewAssoc), value(New))
     # "@var{NewAssoc} is an @tt{assoc_table} identical to @var{Assoc}
       except that the value associated with @var{Key} is @var{New}
       instead of @var{Old}.".

get_assoc(K,Assoc,Old_V,NewAssoc,New_V) :- 
	put_assoc(K,Assoc,New_V,NewAssoc,yes(Old_V)).

:- pred put_assoc(+K,+Assoc,+V,-NewAssoc) : 
     (key(K), assoc_table(Assoc), value(V), assoc_table(NewAssoc))
     # "The value @var{V} is inserted in @var{Assoc} associated to the
        key @var{K} and the result is @var{NewAssoc}. This can be used
        to insert and change associations.".

put_assoc(Key, Assoc, Value, NewAssoc) :-
	put_assoc(Key, Assoc, Value, NewAssoc, _).

:- pred put_assoc(+K,+Assoc1,+V,-Assoc2,-Member) : 
     (key(K), assoc_table(Assoc1), value(V), assoc_table(Assoc2),
     is_member(Member))
     # "The value @var{V} is inserted in @var{Assoc1} associated to
       the key @var{K} and the result is @var{Assoc2}. If the key
       @var{K} doesn't belong to the @var{Assoc1} then @var{Member} is
       unified with no. Otherwise, @var{Assoc2} is the result of
       substituting the association @var{K}-@var{OldValue} by
       @var{K}-@var{V} and @var{Member} is unified with
       yes(OldValue).".


put_assoc(K, assoc_table(Assoc_Old,N_Old,Type_Old),
          V, assoc_table(Assoc_New,N_New,Type_New), Member) :- 
	change_tree(N_Limit),
	compare(Rel,N_Old,N_Limit),
	put_assoc_cmp(Rel,Type_Old,Type_New,Assoc_Old,K,Assoc_New,V,Member),
	new_N(Member,put,N_Old,N_New).

put_assoc_cmp(=,Type_Old,Type_New,Assoc_Old,K,Assoc_New,V,Member) :-
	!,
	(
	    Type_Old == list ->
	    put_assoc_list(Assoc_Old,K,Assoc_Temp,V,Member),
	    (
		Member == no ->
		ord_list_to_assoc_avl(Assoc_Temp,nil,Assoc_New),
		Type_New = tree
	    ;
		Type_New = list,
		Assoc_New = Assoc_Temp
	    )
	;
	    put_assoc_avl(Assoc_Old,K,Assoc_New,V,Member,_),
	    Type_New = tree
	).
	    

put_assoc_cmp(_,list,list,Assoc_Old,K,Assoc_New,V,Member) :-
	put_assoc_list(Assoc_Old,K,Assoc_New,V,Member).


put_assoc_cmp(_,tree,tree,Assoc_Old,K,Assoc_New,V,Member) :-
	put_assoc_avl(Assoc_Old,K,Assoc_New,V,Member,_).


%put_assoc if assoc is a list
put_assoc_list([],K,[K-V],V,no).
put_assoc_list([K1-V1|Rest],K2,Result,V2,Member) :- 
	compare(Rel,K1,K2),
	put_assoc_list_cmp(Rel,[K1-V1|Rest],K2,Result,V2,Member).

put_assoc_list_cmp(<,[K1-V1|Rest1],K2,[K1-V1|Rest2],NewVal,Member) :- 
	put_assoc_list(Rest1,K2,Rest2,NewVal,Member).  
put_assoc_list_cmp(=,[K-OldVal|Rest],K,[K-NewVal|Rest],NewVal,yes(OldVal)).
put_assoc_list_cmp(>,[K1-V1|Rest],K2,[K2-NewVal,K1-V1|Rest],NewVal,no).

%put_assoc if assoc is a avl_tree
put_assoc_avl(nil,K,avl(K,V,nil,nil,0),V,no,yes).
put_assoc_avl(avl(K,V,L,R,H), Key, Result, Val, Member, Eq) :-
	compare(Rel, Key, K),
	put_assoc_avl_cmp(Rel, avl(K,V,L,R,H), Key, Result, Val, Member, Eq).

put_assoc_avl_cmp(<, avl(K,V,L,R,H), Key, Result, Val, Member, Eq1) :- 
	put_assoc_avl(L, Key, ABB, Val, Member, Eq2),
	New_H is H - 1,
	new_FE_5(Eq2,New_H,avl(K,V,ABB,R,H),Result,insert,Eq1).
put_assoc_avl_cmp(=, avl(K,V,L,R,H), _, avl(K,Val,L,R,H), Val, yes(V),no).
put_assoc_avl_cmp(>, avl(K,V,L,R,H), Key, Result, Val, Member, Eq1) :- 
	put_assoc_avl(R, Key, ABB, Val, Member, Eq2),
	New_H is H + 1,
	new_FE_5(Eq2,New_H,avl(K,V,L,ABB,H),Result,insert,Eq1).

%Figure out FE
new_FE_5(no,_,AVL,AVL,_,no).
new_FE_5(yes,Site,ABB,AVL,Op,Eq) :- new_FE_4(Site,Op,ABB,AVL,Eq).

new_FE_4(0,insert,avl(K,V,L,R,_),avl(K,V,L,R,0),no).
new_FE_4(0,delete,avl(K,V,L,R,_),avl(K,V,L,R,0),yes).
new_FE_4(-1,insert,avl(K,V,L,R,_),avl(K,V,L,R,-1),yes).
new_FE_4(-1,delete,avl(K,V,L,R,_),avl(K,V,L,R,-1),no).
new_FE_4(1,insert,avl(K,V,L,R,_),avl(K,V,L,R,1),yes).
new_FE_4(1,delete,avl(K,V,L,R,_),avl(K,V,L,R,1),no).
new_FE_4(-2,Type,avl(K,V,avl(LK,LV,LL,LR,LH),R,_),AVL,Eq) :- 
	get_rotation(-2,LH,ROT),
	rotation(ROT,avl(K,V,avl(LK,LV,LL,LR,LH),R,-2),AVL),
	AVL = avl(_,_,_,_,H),
	continue_balancing(Type,H,Eq).

new_FE_4(2,Type,avl(K,V,L,avl(RK,RV,RL,RR,RH),_),AVL,Eq) :- 
	get_rotation(2,RH,ROT),
	rotation(ROT,avl(K,V,L,avl(RK,RV,RL,RR,RH),2),AVL),
	AVL = avl(_,_,_,_,H),
	continue_balancing(Type,H,Eq).

continue_balancing(delete,0,yes) :- !.
continue_balancing(_,_,no).

get_rotation(2,-1,rdi).
get_rotation(2,1,rsii).
get_rotation(2,0,rsib).
get_rotation(-2,1,rdd).
get_rotation(-2,-1,rsdi).
get_rotation(-2,0,rsdb).

rotation(rsii,avl(K,V,L,avl(RK,RV,RL,RR,_),_),avl(RK,RV,avl(K,V,L,RL,0),RR,0)).
rotation(rsib,avl(K,V,L,avl(RK,RV,RL,RR,_),_),avl(RK,RV,avl(K,V,L,RL,1),RR,-1)).
rotation(rsdi,avl(K,V,avl(LK,LV,LL,LR,_),R,_),avl(LK,LV,LL,avl(K,V,LR,R,0),0)).
rotation(rsdb,avl(K,V,avl(LK,LV,LL,LR,_),R,_),avl(LK,LV,LL,avl(K,V,LR,R,-1),1)).
rotation(rdi,avl(K,V,L,avl(RK,RV,avl(RLK,RLV,RLL,RLR,HRL),RR,_),_),
	 avl(RLK,RLV,avl(K,V,L,RLL,HL),avl(RK,RV,RLR,RR,HR),0)) :-
         height(HRL,HL,HR).
rotation(rdd,avl(K,V,avl(LK,LV,LL,avl(LRK,LRV,LRL,LRR,HLR),_),R,_),
	 avl(LRK,LRV,avl(LK,LV,LL,LRL,HL),avl(K,V,LRR,R,HR),0)) :-
         height(HLR,HL,HR).

height(0,0,0).
height(-1,0,1).
height(1,-1,0).

%Figure out number of elements
new_N(no,delete,N,N).
new_N(no,put,N_Old,N_New) :- N_New is N_Old + 1.
new_N(yes(_),put,N,N).
new_N(yes(_),delete,N_Old,N_New) :- N_New is N_Old - 1.


:- pred add_assoc(+K,+Assoc1,+V,-Assoc2) : 
     (key(K), assoc_table(Assoc1), value(V), assoc_table(Assoc2))
     # "This is similar to @pred{put_value/5} but @var{Key} must not
       appear in @var{Assoc1} (@var{Member} in put_value/5 is
       known to be no).  An error is thrown otherwise.".

add_assoc(K,OldAssoc,Val,NewAssoc) :- 
        put_assoc(K,OldAssoc,Val,NewAssoc,no) ->
        true
; 
	error("add_assoc: key already appears in table").


:- pred update_assoc(+K,+Assoc1,+V,-Assoc2,-OldVar) : (key(K),
     assoc_table(Assoc1), value(V), assoc_table(Assoc2), value(OldVar))
     # "This is similar to @pred{put_assoc/5} but @var{Key} must not
       appear in @var{Assoc1} (@var{Member} in put_value/5 is
       known to be no).  An error is thrown otherwise.".

update_assoc(K,OldAssoc,V,NewAssoc,OldVal) :- 
        put_assoc(K,OldAssoc,V,NewAssoc,yes(OldVal)) ->
	true
;     
	error("update_assoc: key does not appear in table").

:- pred max_assoc(+Assoc, -Key, -Value) : 
     (assoc_table(Assoc), key(Key), value(Value))
     # "@var{Key} and @var{Value} are the @tt{key} and @tt{value} of
       the element with the largest @tt{key} in @var{Assoc}.".

max_assoc(assoc_table(L,_,list),KMax,V) :-
	max_assoc_list(L,KMax,V).
max_assoc(assoc_table(AVL,_,tree),KMax,V) :-
	max_assoc_avl(AVL,KMax,V).

max_assoc_list([K1-V],K2,V) :- K2 = K1.
max_assoc_list([_-_|Rest],KMax,V) :- 
	max_assoc_list(Rest,KMax,V).

max_assoc_avl(avl(K1,V,_,nil,_),K2,V) :- K2 = K1.
max_assoc_avl(avl(_,_,_,R,_),KMax,V) :-
	max_assoc_avl(R,KMax,V).

:- pred min_assoc(+Assoc, -Key, -Value) : 
     (assoc_table(Assoc), key(Key), value(Value))
     # "@var{Key} and @var{Value} are @tt{key} and @tt{value} of the
       element with the smallest @tt{key} in @var{Assoc}.".

min_assoc(assoc_table([K1-V|_],_,list),K2,V) :- K2 = K1.
min_assoc(assoc_table(AVL,_,tree),KMin,V) :-
	min_assoc_avl(AVL,KMin,V).

min_assoc_avl(avl(K1,V,nil,_,_),K2,V) :- K2 = K1.
min_assoc_avl(avl(_,_,L,_,_),KMin,V) :-
	min_assoc_avl(L,KMin,V).

:- pred get_next_assoc(+K, +Assoc, -NextK, -NextV) : 
     (key(K), assoc_table(Assoc), key(NextK), value(NextV))
     # "@var{NextK} and @var{NextV} are the next @tt{key} and
       associated @tt{value} after @var{K} in @var{Assoc}.".

get_next_assoc(K, assoc_table(L,_,list), NextK, NextV) :- 
	get_next_assoc_list(L,K,NextK,NextV).

get_next_assoc(K, assoc_table(AVL,_,tree), NextK, NextV) :- 
	get_next_assoc_avl(AVL,K,_,_,NextK,NextV),
	nonvar(NextK).

get_next_assoc_list([K1-_,NextK1-NextV|_],K2,NextK2,NextV) :- K1 == K2, NextK2 = NextK1.
get_next_assoc_list([K_Act-_|Rest],K,NextK,NextV) :-
	compare(<,K_Act,K),
	get_next_assoc_list(Rest,K,NextK,NextV).

get_next_assoc_avl(avl(K1,_,_,nil,_),K2,NextK1,NextV,NextK2,NextV) :- K1 == K2, NextK2 = NextK1, !.
get_next_assoc_avl(avl(K1,_,_,R,_),K2,_,_,NextK,NextV) :- K1 == K2, 
	!, min_assoc_avl(R,NextK,NextV).
get_next_assoc_avl(avl(K_Act,V_Act,L,R,_),K,TmpK,TmpV,NextK,NextV) :-
	(
	    compare(<,K_Act,K) ->
	    get_next_assoc_avl(R,K,TmpK,TmpV,NextK,NextV)
	;
	    get_next_assoc_avl(L,K,K_Act,V_Act,NextK,NextV)	    
	).


:- pred get_prev_assoc(K, Assoc, PrevK, PrevV) : 
     (key(K), assoc_table(Assoc), key(PrevK), value(PrevV))
     # "@var{PrevK} and @var{PrevV} are the previous @tt{key} and
       associated @tt{value} after @var{K} in @var{Assoc}.".

get_prev_assoc(K, assoc_table(L,_,list), PrevK, PrevV) :- 
	get_prev_assoc_list(L,K,PrevK,PrevV).

get_prev_assoc(K, assoc_table(AVL,_,tree), PrevK, PrevV) :- 
	get_prev_assoc_avl(AVL,K,_,_,PrevK,PrevV),
	nonvar(PrevK).

get_prev_assoc_list([PrevK1-PrevV,K1-_|_],K2,PrevK2,PrevV) :- K1 == K2, PrevK2 = PrevK1.
get_prev_assoc_list([K_Act-_|Rest],K,PrevK,PrevV) :-
	compare(<,K_Act,K),
	get_prev_assoc_list(Rest,K,PrevK,PrevV).

get_prev_assoc_avl(avl(K1,_,nil,_,_),K2,PrevK1,PrevV,PrevK2,PrevV) :- K1 == K2, PrevK2 = PrevK1, !.
get_prev_assoc_avl(avl(K1,_,L,_,_),K2,_,_,PrevK,PrevV) :- K1 == K2,
	!, max_assoc_avl(L,PrevK,PrevV).
get_prev_assoc_avl(avl(K_Act,V_Act,L,R,_),K,TmpK,TmpV,PrevK,PrevV) :-
	(
	    compare(<,K_Act,K) ->
	    get_prev_assoc_avl(R,K,K_Act,V_Act,PrevK,PrevV)
	;
	    get_prev_assoc_avl(L,K,TmpK,TmpV,PrevK,PrevV)	    
	).

:- pred map_assoc(+Pred, +Assoc) 
     # "@var{Assoc} is an association tree, and for each @var{Key}, if
       @var{Key} is associated with @var{Value} in @var{Assoc},
       Pred(Value) is true.".

:- meta_predicate map_assoc(pred(1), ?).

map_assoc(Pred,Assoc) :- 
	assoc_to_list(Assoc,L),
	map_list(L,Pred).

map_list([],_).
map_list([_-V|Rest],Pred) :- 
	Pred(V),
	map_list(Rest,Pred).

:- pred map_assoc(+Pred, +Assoc, -NewAssoc) 
     # "@var{Assoc} and @var{NewAssoc} are association trees of the
       same shape, and for each @var{Key}, if @var{Key} is associated
       with @var{Old} in @var{Assoc} and with @var{new} in
       @var{NewAssoc}, Pred(Old,New) is true.".

:- meta_predicate map_assoc(pred(2), ?, ?).

map_assoc(Pred,Assoc,AssocNew) :- 
	assoc:map(Assoc,(_(_,Old,New) :- Pred(Old,New)),AssocNew).

:- pred map(+Assoc1,+Pred,-Assoc2)
     # "Applies @var{Pred} with arity 3 to each value of the
       assoc_table @var{Assoc1} obtaining the new assoc_table
       @var{Assoc2} in which only the values can have changed.".

:- meta_predicate map(?, pred(3), ?).

map(assoc_table(Assoc,N,Type),Pred,assoc_table(Result,N,Type)) :- 
	(
	    Type == tree ->
	    map_avl(Assoc,Pred,Result)
	;
	    hiordlib:map(Assoc,(_(K-V,K-R) :- Pred(K,V,R)),Result)
	).

map_avl(nil,_,nil).
map_avl(avl(K,Old_Val,Old_L,Old_R,H),Pred,avl(K,New_Val,New_L,New_R,H)) :- 
	map_avl(Old_L,Pred,New_L),
	Pred(K,Old_Val,New_Val), 
	map_avl(Old_R,Pred,New_R).

:- pred foldl(+Assoc,+DS,+Pred,-NDS) 
     # "Applies @var{Pred} with arity 4 to each value of the
       assoc_table @var{Assoc}. If @var{Pred} is satisfied, it updates
       the data-structure DS. Otherwise it fails.".

:- meta_predicate foldl(?, ?, pred(4), ?).

foldl(assoc_table(Assoc,_,Type),DS,Pred,NDS) :- 
	(
	    Type == tree ->
	    foldl_avl(Assoc,DS,Pred,NDS)
	;
	    hiordlib:foldl(Assoc,DS,(_(K-V,DS0,NDS0) :- Pred(K,V,DS0,NDS0)),NDS)
	).

foldl_avl(nil,DS,_,DS).
foldl_avl(avl(K,V,L,R,_),DS,Pred,NDS) :- 
	foldl_avl(L,DS,Pred,DS0),
	Pred(K,V,DS0,DS1),
	foldl_avl(R,DS1,Pred,NDS).

:- regtype ord_pairs(P) # "@var{P} is a ordered list of elements of the
   form @tt{key}-@tt{value}.".

%% MCL: This is an approximation.  ord_pairs is not a regular type (it can
%% be a property, though)
ord_pairs([]).
ord_pairs([_-_|Rest]) :-
	ord_pairs(Rest).


 %% Not a regular type!
 %% :- regtype ord_pairs_aux(K, Rest).
 %% 
 %% ord_pairs_aux(_,[]).
 %% ord_pairs_aux(KPrev,[K-_|Rest]) :-
 %% 	KPrev @< K,
 %% 	ord_pairs_aux(K,Rest).
 %% 

:- pred assoc_to_list(+Assoc,-L) : (assoc_table(Assoc), ord_pairs(L))
     # "Transforms @var{Assoc} into @var{L} where each pair of @var{L}
       was a association in @var{Assoc}.".

assoc_to_list(assoc_table(A,_,Type),L) :-
	(
	    Type == list ->
	    L = A
	;
	    assoc_to_list_avl(A,[],L)
	).
	
assoc_to_list_avl(nil,L,L).
assoc_to_list_avl(avl(K,V,L,R,_),List,Resul) :-
	assoc_to_list_avl(L,List,L_Left),
	append(L_Left,[K-V],L_Temp),
	assoc_to_list_avl(R,L_Temp,Resul).

:- pred ord_list_to_assoc(+L,-Assoc) : 
     (ord_pairs(L),assoc_table(Assoc))
     # "Transforms @var{L}, a list of pairs (using the functor
       @tt{-/2}) sorted by its first element, into the table
       @var{Assoc} where each pair of @var{L} will become a
       association in @var{Assoc}.".

ord_list_to_assoc(Pairs,assoc_table(Assoc,Long,Type)) :- 
	change_tree(N_Limit),
	length(Pairs,Long),
	(
	    Long > N_Limit ->
	    ord_list_to_assoc_avl(Pairs, nil, Assoc),
	    Type = tree
	;
	    Assoc = Pairs,
	    Type = list
	).

ord_list_to_assoc_avl([], AVL, AVL).
ord_list_to_assoc_avl([K-V|Pairs], AVL_Old, AVL_New) :-
	put_assoc_avl(AVL_Old, K, AVL_Temp, V, _, _),
	ord_list_to_assoc_avl(Pairs, AVL_Temp, AVL_New).

:- regtype pairs(P) # "@var{P} is a list of elements of the
   form @tt{key}-@tt{value}.".

pairs([]).
pairs([_-_|R]) :- pairs(R).

:- pred list_to_assoc(+L,-Assoc) : (pairs(L), assoc_table(Assoc))
     # "Transforms @var{L} into @var{Assoc} where each pair of @var{L}
       will be a association in @var{Assoc}.".

list_to_assoc([],Assoc) :- empty_assoc(Assoc).
list_to_assoc([K-V|Rest],Assoc) :-
	list_to_assoc(Rest,Tmp),
	put_assoc(K,Tmp,V,Assoc,_).

:- pred gen_assoc(-K,+Assoc,-V) : 
     (key(K), assoc_table(Assoc), value(V))
     # "Enumerate matching elements of Assoc in ascending order of
       their keys via backtracking.".

:- pred gen_assoc(-K,+Assoc,+V) : 
     (key(K), assoc_table(Assoc), value(V))
     # "Enumerate matching elements of Assoc in ascending order of
       their keys via backtracking whose value is @var{V}.".

gen_assoc(K,Assoc,V) :-
	assoc_to_list(Assoc,L),
	select_pairs(L,K,V).

select_pairs([K1-V|_],K2,V) :- K2 = K1.
select_pairs([_-_|Rest],K,V) :-
	select_pairs(Rest,K,V).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%%%%%%%%%%%%                   CALCULO DE N_LIMIT                        %%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%crear_lista(L,1,L).
%crear_lista(L1,N,L2) :- 
%	N > 1,
%	N2 is N - 1,
%	L_Temp = [N2-N2|L1],
%	crear_lista(L_Temp,N2,L2).

%acceder_lista(_,0) :- !.
%acceder_lista(List,N) :-
%	get_assoc_list(List,N,N),
%	N2 is N - 1,
%	acceder_lista(List,N2).
%
%acceder_AVL(_,0) :- !.
%acceder_AVL(AVL,N) :-
%	get_assoc_avl(AVL,N,N),
%	N2 is N - 1,
%	acceder_AVL(AVL,N2).

%insertar_nesimo_lista(N,List_Ori) :-
	%le insertamos el elemento n-esimo
%	put_assoc_list(List_Ori,N,List,N,_),
	%accedemos independientemente a los n elementos de la lista
%	acceder_lista(List,N).

%insertar_nesimo_avl(N,List_Ori) :-
	%ahora insertamos el n-esimo elemento pero trabajando con arboles
%	ord_list_to_assoc_avl(List_Ori,nil,AVL_Ori),
	%le insertamos el elemento n-esimo
%	put_assoc_avl(AVL_Ori,N,AVL,N,_,_),
	%accedemos independientemente a los n elementos de la lista
%	acceder_AVL(AVL,N).

%recorrer_N(Tiempo,N,N_Fin,Repeat,Resul,Llevo) :- 
%	crear_lista([],N,Lista_Ori),
%	statistics(Tiempo,[_,_]),
	%construimos una lista de asociacion de n - 1 elementos
%	between(1,Repeat,N_Lista),
%	insertar_nesimo_lista(N,Lista_Ori),
%	(
%	    N_Lista < Repeat ->
%	    fail
%	;
%	    statistics(Tiempo,[_,T_Lista])
%	),
%	between(1,Repeat,N_Avl),
%	insertar_nesimo_avl(N,Lista_Ori),
%	(
%	    N_Avl < Repeat ->
%	    fail
%	;
%	    statistics(Tiempo,[_,T_Avl])
%	),
%	Llevo2 = [N:T_Lista-T_Avl|Llevo],
%	(
%	    N == N_Fin ->
%	    Resul = Llevo2
%	;
%	    N1 is N + 1,
%	    recorrer_N(Tiempo,N1,N_Fin,Repeat,Resul,Llevo2)
%	).	
	
%tabla_tiempos(Tiempo,N_Ini,N_Fin,Repeat,Resul) :-
%	recorrer_N(Tiempo,N_Ini,N_Fin,Repeat,Resul,[]).
