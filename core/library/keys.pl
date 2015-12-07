
:- module(keys,
	[ keys_and_values/3,
	  keys_and_values/4,
	  key_lookup/4
	],
	[ assertions
	] ).

keys_and_values([K|Ks],[V|Vs],[K=V|KVs]):-
	keys_and_values(Ks,Vs,KVs).
keys_and_values([],[],[]).

keys_and_values([K|Ks],[V|Vs],[K=V|KVs],KVs0):-
	keys_and_values(Ks,Vs,KVs,KVs0).
keys_and_values([],[],KVs,KVs).

key_lookup(Key,[K=V|Dic],Value,Rest):-
	compare(R,Key,K),
	key_lookup_unscrambled(R,Key,K,V,Dic,Value,Rest).
%key_lookup(_Key,[],_Value,_Rest).           %% not found

key_lookup_unscrambled(=,_Key,_Key,Value,Dic,Value,Dic).
%key_lookup(<,_Key,_K,_V,_Dic,_Value,_Rest). %% not found
key_lookup_unscrambled(>,Key,K,V,Dic,Value,[K=V|Rest]):-
	key_lookup(Key,Dic,Value,Rest).
