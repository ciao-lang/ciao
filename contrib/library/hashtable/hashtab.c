/*
--------------------------------------------------------------------
  Modified by Edison Mera to be suitable for ciao-1.11
  05-2004.  hashtab.h

--------------------------------------------------------------------
By Bob Jenkins, 1996.  hashtab.c.  Public Domain.

This implements a hash table.
* Keys are unique.  Adding an item fails if the key is already there.
* Keys and items are pointed at, not copied.  If you change the value
  of the key after it is inserted then ht_find will not be able to find it.
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

#include <stdlib.h>
#include <string.h>

#include <ciao/configure.h>
#include <ciao/datadefs.h>

#ifndef STANDARD
#include "standard.h"
#endif
#ifndef LOOKUPA
#include "lookupa.h"
#endif
#ifndef HASHTAB
#include "hashtab.h"
#endif
#ifndef RECYCLE
#include "recycle.h"
#endif

#ifdef HSANITY
/* sanity check -- make sure ipos, apos, and count make sense */
static void  ht_sanity(ht_tab *t)
{
  ub4    i, end, counter;
  ht_item *h;

  /* test that apos makes sense */
  end = (ub4)1<<(t->logsize);
  if (end < t->apos)
    printf("error:  end %ld  apos %ld\n", end, t->apos);

  /* test that ipos is in bucket apos */
  if (t->ipos)
  {
    for (h=t->table[t->apos];  h && h != t->ipos;  h = h->next)
      ;
    if (h != t->ipos)
      printf("error:ipos not in apos, apos is %ld\n", t->apos);
  }

  /* test that t->count is the number of elements in the table */
  counter=0;
  for (counter=0, i=0;  i<end;  ++i)
    for (h=t->table[i];  h;  h=h->next)
      ++counter;
  if (counter != t->count)
    printf("error: counter %ld  t->count %ld\n", counter, t->count);
}
#endif /* HSANITY */


/*
 * ht_grow - Double the size of a hash table.
 * Allocate a new, 2x bigger array,
 * move everything from the old array to the new array,
 * then free the old array.
 */
static void ht_grow( t)
ht_tab  *t;    /* table */
{
  register ub4     oldsize = (ub4)1<<(t->logsize);
  register ub4     newsize = (ub4)1<<(++t->logsize);
  register ub4     newmask = newsize-1;
  register ub4     i;
  register ht_item **oldtab = t->table;
  register ht_item **newtab = (ht_item **)ht_alloc(newsize*sizeof(ht_item *));

  /* make sure newtab is cleared */
  for (i=0; i<newsize; ++i) newtab[i] = (ht_item *)0;
  t->table = newtab;
  t->mask = newmask;

  /* Walk through old table putting entries in new table */
  for (i=newsize>>1; i--;)
  {
    register ht_item *this, *that, **newplace;
    for (this = oldtab[i]; this;)
    {
      that = this;
      this = this->next;
      newplace = &newtab[(that->hval & newmask)];
      that->next = *newplace;
      *newplace = that;
    }
  }

  /* position the hash table on some existing item */
  ht_first(t);

  /* free the old array */
  ht_dealloc(oldtab,oldsize);
}

/* ht_create - create a hash table initially of size power(2,logsize)

   logsize: log base 2 of the size of the hash table
 */
ht_tab *ht_create(word logsize)
{
  ub4 i,len;
  ht_tab *t = (ht_tab *)ht_alloc(sizeof(ht_tab));
  len = ((ub4)1<<logsize);
  t->table = (ht_item **)ht_alloc(sizeof(ht_item *)*(ub4)len);
  for (i=0; i<len; ++i) t->table[i] = (ht_item *)0;
  t->logsize = logsize;
  t->mask = len-1;
  t->count = 0;
  t->apos = (ub4)0;
  t->ipos = (ht_item *)0;
  t->space = remkroot(sizeof(ht_item));
  t->bcount = 0;
  return t;
}

/* ht_destroy - destroy the hash table and free all its memory */
void ht_destroy( t)
ht_tab  *t;    /* the table */
{
/*   ht_item *h; */
  refree(t->space);
  ht_dealloc(t->table, (ub4)1<<(t->logsize));
  ht_dealloc(t, sizeof(ht_tab));
}

/* ht_count() is a macro, see hashtab.h */
/* ht_key() is a macro, see hashtab.h */
/* ht_keyl() is a macro, see hashtab.h */
/* ht_stuff() is a macro, see hashtab.h */

/* ht_find - find an item with a given key in a hash table */
word   ht_find( t, key, keyl )
ht_tab  *t;     /* table */
ub1   *key;   /* key to find */
ub4    keyl;  /* key length */
{
  ht_item *h;
  ub4    x = lookup(key,keyl,0);
  ub4    y;
  for (h = t->table[y=(x&t->mask)]; h; h = h->next)
  {
    if ((x == h->hval) && 
        (keyl == h->keyl) && 
        !memcmp(key, h->key, keyl))
    {
      t->apos = y;
      t->ipos = h;
      return TRUE;
    }
  }
  return FALSE;
}

/* ht_exists - EMM - like ht_find but does not move the cursor */
word  ht_exists( t, key, keyl )
ht_tab  *t;     /* table */
ub1   *key;   /* key to find */
ub4    keyl;  /* key length */
{
  ht_item *h;
  ub4    x = lookup(key,keyl,0);
  ub4    y;
  for (h = t->table[y=(x&t->mask)]; h; h = h->next)
  {
    if ((x == h->hval) && 
        (keyl == h->keyl) && 
        !memcmp(key, h->key, keyl))
      return TRUE;
  }
  return FALSE;
}


/*
 * ht_add - add an item to a hash table.
 * return FALSE if the key is already there, otherwise TRUE.
 */
word ht_add( t, key, keyl, stuff)
ht_tab  *t;      /* table */
ub1   *key;    /* key to add to hash table */
ub4    keyl;   /* key length */
void  *stuff;  /* stuff to associate with this key */
{
  register ht_item  *h,**hp;
  register ub4     y, x = lookup(key,keyl,0);
  /* make sure the key is not already there */
  for (h = t->table[(y=(x&t->mask))]; h; h = h->next)
  {
    if ((x == h->hval) &&
        (keyl == h->keyl) &&
        !memcmp(key, h->key, keyl))
    {
      t->apos = y;
      t->ipos = h;
      return FALSE;
    }
  }

  /* find space for a new item */
  h = (ht_item *)renew(t->space);

  /* make the hash table bigger if it is getting full */
  if (++t->count > (ub4)1<<(t->logsize))
  {
    ht_grow(t);
    y = (x&t->mask);
  }

  /* add the new key to the table */
  h->key   = key;
  h->keyl  = keyl;
  h->stuff = stuff;
  h->hval  = x;
  hp = &t->table[y];
  h->next = *hp;
  *hp = h;
  t->ipos = h;
  t->apos = y;

#ifdef HSANITY
  ht_sanity(t);
#endif  /* HSANITY */

  return TRUE;
}

/* ht_del - delete the item at the current position */
word ht_del(ht_tab *t)
{
  ht_item  *h;    /* item being deleted */
  ht_item **ip;   /* a counter */

  /* check for item not existing */
  if (!(h = t->ipos)) return FALSE;

  /* remove item from its list */
  for (ip = &t->table[t->apos]; *ip != h; ip = &(*ip)->next)
    ;
  *ip = (*ip)->next;
  --(t->count);

  /* adjust position to something that exists */
  if (!(t->ipos = h->next)) ht_nbucket(t);

  /* recycle the deleted ht_item node */
  redel(t->space, h);

#ifdef HSANITY
  ht_sanity(t);
#endif  /* HSANITY */

  return TRUE;
}

/* ht_first - position on the first element in the table */
word ht_first(ht_tab *t)
{
  t->apos = t->mask;
  (void)ht_nbucket(t);
  return (t->ipos != (ht_item *)NULL);
}

/* ht_next() is a macro, see hashtab.h */

/*
 * ht_nbucket - Move position to the first item in the next bucket.
 * Return TRUE if we did not wrap around to the beginning of the table
 */
word ht_nbucket(ht_tab *t)
{
  ub4  oldapos = t->apos;
  ub4  end = (ub4)1<<(t->logsize);
  ub4  i;

  /* see if the element can be found without wrapping around */
  for (i=oldapos+1; i<end; ++i)
  {
    if (t->table[i&t->mask])
    {
      t->apos = i;
      t->ipos = t->table[i];
      return TRUE;
    }
  }

  /* must have to wrap around to find the last element */
  for (i=0; i<=oldapos; ++i)
  {
    if (t->table[i])
    {
      t->apos = i;
      t->ipos = t->table[i];
      return FALSE;
    }
  }

  return FALSE;
}

void ht_stat(ht_tab *t)
{
  ub4     i,j;
  double  total = 0.0;
  ht_item  *h;
  ht_item  *walk, *walk2, *stat = (ht_item *)0;

  /* in stat, keyl will store length of list, hval the number of buckets */
  for (i=0; i<=t->mask; ++i)
  {
    for (h=t->table[i], j=0; h; ++j, h=h->next)
      ;
    for (walk=stat; walk && (walk->keyl != j); walk=walk->next)
      ;
    if (walk)
    {
      ++(walk->hval);
    }
    else
    {
      walk = (ht_item *)renew(t->space);
      walk->keyl = j;
      walk->hval = 1;
      if (!stat || stat->keyl > j) {walk->next=stat; stat=walk;}
      else
      {
        for (walk2=stat;
             walk2->next && (walk2->next->keyl<j);
             walk2=walk2->next)
          ;
        walk->next = walk2->next;
        walk2->next = walk;
      }
    }
  }

  /* figure out average list length for existing elements */
  for (walk=stat; walk; walk=walk->next)
  {
    total+=(double)walk->hval*(double)walk->keyl*(double)walk->keyl;
  }
  if (t->count) total /= (double)t->count;
  else          total  = (double)0;

  /* print statistics */
  printf("\n");
  for (walk=stat; walk; walk=walk->next)
  {
    printf("items %ld:  %ld buckets\n", walk->keyl, walk->hval);
  }
  printf("\nbuckets: %ld  items: %ld  existing: %g\n\n",
         ((ub4)1<<t->logsize), t->count, total);

  /* clean up */
  while (stat)
  {
    walk = stat->next;
    redel(t->space, stat);
    stat = walk;
  }
}


