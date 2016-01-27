/*
--------------------------------------------------------------------
  Modified by Edison Mera to be suitable for ciao-1.11
  05-2004.  hashtab.h

--------------------------------------------------------------------
By Bob Jenkins, 1996.  hash.h.  Public Domain.

This implements a hash table.
* Keys are unique.  Adding an item fails if the key is already there.
* Keys and items are pointed at, not copied.  If you change the value
  of the key after it is inserted then hfind will not be able to find it.
* The hash table maintains a position that can be set and queried.
* The table length doubles dynamically and never shrinks.  The insert
  that causes table doubling may take a long time.
* The table length splits when the table length equals the number of items
  Comparisons usually take 7 instructions.
  Computing a hash value takes 35+6n instructions for an n-byte key.

  ht_create  - create a hash table
  ht_destroy - destroy a hash table
   ht_count  - The number of items in the hash table
   ht_key    - key at the current position
   ht_keyl   - key length at the current position
   ht_stuff  - stuff at the current position
  ht_find    - find an item in the table
   ht_add    - insert an item into the table
   ht_del    - delete an item from the table
  ht_stat    - print statistics about the table
   ht_first  - position at the first item in the table
   ht_next   - move the position to the next item in the table
--------------------------------------------------------------------
*/

#ifndef STANDARD
#include "standard.h"
#endif

#ifndef HASHTAB
#define HASHTAB

/* PRIVATE TYPES AND DEFINITIONS */

struct ht_item
{
  ub1          *key;      /* key that is hashed */
  ub4           keyl;     /* length of key */
  void         *stuff;    /* stuff stored in this ht_item */
  ub4           hval;     /* hash value */
  struct ht_item *next;     /* next ht_item in list */
};
typedef  struct ht_item  ht_item;


struct ht_tab
{
  struct ht_item **table;   /* hash table, array of size 2^logsize */
  word           logsize; /* log of size of table */
  size_t         mask;    /* (hashval & mask) is position in table */
  ub4            count;   /* how many items in this hash table so far? */
  ub4            apos;    /* position in the array */
  struct ht_item  *ipos;    /* current item in the array */
  struct reroot *space;   /* space for the ht_items */
  ub4            bcount;  /* # ht_items useable in current block */
};
typedef  struct ht_tab  ht_tab;





/* PUBLIC FUNCTIONS */

/* ht_create - create a hash table
   ARGUMENTS:
     logsize - 1<<logsize will be the initial table length
   RETURNS:
     the new table
 */
ht_tab *ht_create(word logsize);


/* ht_destroy - destroy a hash table
   ARGUMENTS:
     t - the hash table to be destroyed.  Note that the items and keys
         will not be freed, the user created them and must destroy
         them himself.
   RETURNS:
     nothing
 */
void ht_destroy(ht_tab *t);


/* hcount, hkey, hkeyl, ht_stuff
     ARGUMENTS:
     t - the hash table
   RETURNS:
     ht_count - (ub4)    The number of items in the hash table
     ht_key   - (ub1 *)  key for the current item
     ht_keyl  - (ub4)    key length for the current item
     ht_stuff - (void *) stuff for the current item
   NOTE:
     The current position always has an item as long as there
       are items in the table, so hexist can be used to test if the
       table is empty.
     hkey, hkeyl, and hstuff will crash if hcount returns 0
 */
#define ht_count(t) ((t)->count)
#define ht_key(t)   ((t)->ipos->key)
#define ht_keyl(t)  ((t)->ipos->keyl)
#define ht_stuff(t) ((t)->ipos->stuff)



/* hfind - move the current position to a given key
   ARGUMENTS:
     t    - the hash table
     key  - the key to look for
     keyl - length of the key
   RETURNS:
     TRUE if the item exists, FALSE if it does not.
     If the item exists, moves the current position to that item.
 */
word  ht_find(ht_tab *t, ub1 *key, ub4 keyl);


/* ht_exists - EMM - like ht_find but does not move the cursor */
word ht_exists(ht_tab *t, ub1 *key, ub4 keyl);

/* hadd - add a new item to the hash table
          change the position to point at the item with the key
   ARGUMENTS:
     t     - the hash table
     key   - the key to look for
     keyl  - length of the key
     stuff - other stuff to be stored in this item
   RETURNS:
     FALSE if the operation fails (because that key is already there).
 */
word  ht_add(ht_tab *t, ub1 *key, ub4 keyl, void *stuff);


/* ht_del - delete the item at the current position
          change the position to the following item
  ARGUMENTS:
    t    - the hash table
  RETURNS:
    FALSE if there is no current item (meaning the table is empty)
  NOTE:
    This frees the item, but not the key or stuff stored in the item.
    If you want these then deal with them first.  For example:
      if (ht_find(tab, key, keyl))
      {
        free(ht_key(tab));
        free(ht_stuff(tab));
        ht_del(tab);
      }
 */
word  ht_del(ht_tab *t);


/* hfirst - move position to the first item in the table
  ARGUMENTS:
    t    - the hash table
  RETURNS:
    FALSE if there is no current item (meaning the table is empty)
  NOTE:
 */
word ht_first(ht_tab *t);

/* hnext - move position to the next item in the table
  ARGUMENTS:
    t    - the hash table
  RETURNS:
    FALSE if the position wraps around to the beginning of the table
  NOTE:
    To see every item in the table, do
      if (ht_first(t)) do
      {
        key   = ht_key(t);
        stuff = ht_stuff(t);
      }
      while (ht_next(t));
 */
/* word hnext(/o_ ht_tab *t _o/); */
#define ht_next(t) \
  ((!(t)->ipos) ? FALSE :  \
   ((t)->ipos=(t)->ipos->next) ? TRUE : ht_nbucket(t))

/* ht_nbucket - PRIVATE - move to first item in the next nonempty bucket
  ARGUMENTS:
    t    - the hash table
  RETURNS:
    FALSE if the position wraps around to the beginning of the table
  NOTE:
    This is private to hashtab; do not use it externally.
 */
word ht_nbucket(ht_tab *t);


/* hstat - print statistics about the hash table
  ARGUMENTS:
    t    - the hash table
  NOTE:
    items <0>:  <#buckets with zero items> buckets
    items <1>:  <#buckets with 1 item> buckets
    ...
    buckets: #buckets  items: #items  existing: x
    ( x is the average length of the list when you look for an
      item that exists.  When the item does not exists, the average
      length is #items/#buckets. )

    If you put n items into n buckets, expect 1/(n!)e buckets to
    have n items.  That is, .3678 0, .3678 1, .1839 2, ...
    Also expect "existing" to be about 2.
 */
void ht_stat(ht_tab *t);

#endif   /* HASHTABLE */
