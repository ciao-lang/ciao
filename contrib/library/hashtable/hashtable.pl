% TODO: May not work in 64-bits! see hashtab.h; ub4 is defined as 'long'
:- module(hashtable, _, [assertions, foreign_interface]).

:- use_foreign_source(library(hashtable/recycle)).
:- use_foreign_source(library(hashtable/lookupa)).
:- use_foreign_source(library(hashtable/hashtab)).

:- foreign_inline("
#include <string.h>
#include \"hashtab.h\"
").

:- true pred ht_create(in(LogSize), go(HTab)) :: c_long * address + (foreign, returns(HTab)).

:- true pred ht_destroy(in(HTab)) :: address + (foreign).

:- true pred ht_count(in(HTab), go(Count)) :: address * c_long + (foreign(ht_count_),
   returns(Count)).

:- foreign_inline(ht_count/2,
"long ht_count_(ht_tab *t)
{
  return ht_count(t);
}
").

% NOTE: Keys are stored as arrays of chars, but they are not \0-ended
% (so we cannot translate easily to atoms)

:- true pred ht_key(in(HTab), go(Key)) :: address * address +
   (foreign(ht_key_), returns(Key)).

:- foreign_inline(ht_key/2,
"void *ht_key_(ht_tab *t)
{
  return ht_key(t);
}
").

:- true pred ht_keyl(in(HTab), go(Keyl)) :: address * c_long +
   (foreign(ht_keyl_), returns(Keyl)).

:- foreign_inline(hkeyl_/2,
"long ht_keyl_(ht_tab *t)
{
  return ht_keyl(t);
}
").

:- true pred ht_stuff(in(HTab), go(Stuff)) :: address * address +
   (foreign(ht_stuff_), returns(Stuff)).

:- foreign_inline(ht_stuff/2,
"void * ht_stuff_(ht_tab *t)
{
  return ht_stuff(t);
}
").

% TODO: Missing predicates for (address,length)<->atom conversion

:- true pred ht_find(in(HTab), in(Key), in(Keyl), go(Find)) :: 
   address * address * c_long * c_long + (foreign, returns(Find)).

:- true pred ht_add(in(HTab), in(Key), in(Keyl), in(Stuff), go(Add)) ::
   address * address * c_long * address * c_long + (foreign, returns(Add)).

:- true pred ht_add2(in(HTab), in(Key), in(Stuff), go(Add)) ::
   address * atm * address * c_long + (foreign, returns(Add)).

:- foreign_inline(ht_add2/4,
"long ht_add2(ht_tab *t, char *key, void *stuff) {
  return ht_add(t, (ub1*)key, strlen((char *)key), stuff);
}
").

:- true pred ht_del(in(HTab), go(Del)) :: address * c_long + (foreign,
   returns(Del)).

:- true pred ht_first(in(HTab), go(First)) :: address * c_long + (foreign,
   returns(First)).

:- true pred ht_next(in(HTab), go(Next)) :: address * c_long + (foreign(ht_next_),
   returns(Next)).

:- foreign_inline(ht_next/2,
"long ht_next_(ht_tab *t)
{
  return ht_next(t);
}
").

:- true pred ht_nbucket(in(HTab), go(NBucket)) :: address * c_long +
   (foreign, returns(NBucket)).

:- true pred ht_stat(in(Htab)) :: address + (foreign).

% TODO: wrong type in ht_add2 (Stuff is not an address)
% add_rec(_,[]).
% add_rec(HashTable,[D|Ds]) :-
% 	ht_add2(HashTable, D, '', _X),
% 	add_rec(HashTable,Ds).

% main :-
% 	Dictionary=[one,two,tree,four,five,six,two,four],
% 	ht_create(8, HashTable),
% 	add_rec(HashTable,Dictionary),
% 	ht_destroy(HashTable).
