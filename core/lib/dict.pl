/* Copyright (C) 1996-2002 UPM-CLIP */

:- module(dict,[dictionary/1, dictionary/5, dic_node/2,
		 dic_lookup/3, dic_lookup/4,
		 dic_get/3, dic_replace/4,
		 old_or_new/1,
		 non_empty_dictionary/1],
	       [assertions, nortchecks, isomodes]).

:- doc(title,"Dictionaries").

:- doc(author, "The CLIP Group").

:- doc(module,"This module provides predicates for implementing
   dictionaries. Such dictionaries are currently implemented as
   ordered binary trees of key-value pairs.").

:- true prop dictionary(D) + regtype # "@var{D} is a dictionary.".

dictionary(X) :- term(X).  % A free variable is a valid dictionary.

:- true prop non_empty_dictionary(D) + regtype # "@var{D} is a non-empty dictionary.".

non_empty_dictionary(dic(K,V,L,R)):-
	term(K),
	term(V),
	dictionary(L),
	dictionary(R).

:- true prop old_or_new(X) + regtype.

old_or_new(old).
old_or_new(new).

:- pred dictionary(D,K,V,L,R) =>  non_empty_dictionary(D)
      # "The dictionary node @var{D} has key @var{K}, value @var{V},
	 left child @var{L}, and right child @var{R}.".

dictionary(dic(K,V,L,R),K,V,L,R).

:- pred dic_node(D,N) : non_empty_dictionary(D) => dictionary(N)
      # "@var{N} is a sub-dictionary of @var{D}.".

dic_node([], _) :- !, fail. % variable
dic_node(Node, Node).
dic_node(dic(_,_,L,_), Node) :- dic_node(L, Node).
dic_node(dic(_,_,_,R), Node) :- dic_node(R, Node).

:- pred dic_lookup(D,K,V) => non_empty_dictionary(D)
      # "@var{D} contains value @var{V} at key @var{K}. If it was
         not already in @var{D} it is added.".

dic_lookup(Dic, Key, Val) :-
        dic_lookup(Dic, Key, Val, _Occ).


:- pred dic_lookup(D,K,V,O) => ( non_empty_dictionary(D), old_or_new(O))
      # "Same as @tt{dic_lookup(D,K,V)}. @var{O} indicates if it
         was already in @var{D} (@tt{old}) or not (@tt{new}).".

dic_lookup(Dic, Key, Val, Occ) :-
	var(Dic), !,
	Dic=dic(Key,Val,_,_),
        Occ = new.
dic_lookup(dic(K,V,L,R), Key, Val, Occ) :-
	compare(Rel, Key, K),
	dic_lookup_(Rel, Key, Val, V, L, R, Occ).

dic_lookup_(=, _, Val, Val, _, _, old).
dic_lookup_(<, Key, Val, _, L, _, Occ) :- dic_lookup(L, Key, Val, Occ).
dic_lookup_(>, Key, Val, _, _, R, Occ) :- dic_lookup(R, Key, Val, Occ).


:- pred dic_get(+D,K,V) : dictionary(D)  =>  non_empty_dictionary(D)
      # "@var{D} contains value @var{V} at key @var{K}. Fails if it
         is not already in @var{D}.".


dic_get(Dic, Key, Val) :-
	nonvar(Dic),
	Dic=dic(K,V,L,R),
	compare(X, Key, K),
	dic_get_(X, Key, Val, V, L, R).

dic_get_(=, _, Val, Val, _, _).
dic_get_(<, Key, Val, _, L, _) :- dic_get(L, Key, Val).
dic_get_(>, Key, Val, _, _, R) :- dic_get(R, Key, Val).


:- pred dic_replace(D,K,V,D1) 
 	: (dictionary(D), dictionary(D1)) => ( dictionary(D), dictionary(D1) )
       # "@var{D} and @var{D1} are identical except for the element
          at key @var{K}, which in @var{D1} contains value @var{V}, 
          whatever has (or whether it is) in @var{D}.".

dic_replace(Dic, Key, Val, Dic1) :-
	var(Dic), !,
	Dic1=dic(Key,Val,_,_).
dic_replace(dic(Key1,Val1,L1,R1), Key, Val, dic(Key1,Val2,L2,R2)) :-
	compare(X, Key, Key1),
	dic_replace_(X, Key, Val, Key1, Val1, L1, R1, Val2, L2, R2).

dic_replace_(=, _, Val, _, _, L, R, Val, L, R).
dic_replace_(<, Key, Val, _, Val1, L1, R, Val1, L2, R) :-
	dic_replace(L1, Key, Val, L2).
dic_replace_(>, Key, Val, _, Val1, L, R1, Val1, L, R2) :-
	dic_replace(R1, Key, Val, R2).
