/*
 *  support.c
 *
 *  General runtime support routines.
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 */

#include <unistd.h>
#include <string.h>

#include <ciao/datadefs.h>
#include <ciao/support_macros.h>
#include <ciao/wamsupport.h>
#include <ciao/streams_basic.h>
#include <ciao/bignum.h>
#include <ciao/support.h>
#include <ciao/alloc.h>
#include <ciao/wam_alloc.h>
#include <ciao/stacks.h>
#include <ciao/bignum.h>
#include <ciao/locks.h>
#include <ciao/start.h>
#include <ciao/initial.h>
#include <ciao/profile_hooks.h>
#include <ciao/task_areas.h>
#include <ciao/tasks.h>
#include <ciao/indexing.h>
#include <ciao/io_basic.h>

/* local declarations */

static CBOOL__PROTO(cunify_args_aux, 
		    int arity,
		    tagged_t *pt1,
		    tagged_t *pt2,
		    tagged_t *x1,
		    tagged_t *x2);
static CBOOL__PROTO(cunify_aux, tagged_t x1, tagged_t x2);
static CVOID__PROTO(numstack_overflow);
static definition_t **find_subdef_chain(definition_t *f, intmach_t clause_no);
static definition_t *parse_1_definition(tagged_t tagname, tagged_t tagarity);
#if defined(USE_ATOM_LEN)
static sw_on_key_node_t *atom_gethash(sw_on_key_t *sw, 
				      tagged_t key, 
				      char *str,
				      uintmach_t str_len);
#else
static sw_on_key_node_t *atom_gethash(sw_on_key_t *sw, 
				      tagged_t key, 
				      char *str);
#endif


/*-----------------------------------------------------------*/

intmach_t goal_from_thread_id_hook_(THREAD_ID id)
{
  return TRUE;
}

intmach_t (*eng_goal_from_thread_id)(THREAD_ID id)=goal_from_thread_id_hook_;

void failc(char *mesg)
{
  extern char source_path[];

  // Give an error.  We check if we were able to allocate memory at all for
  // the user_eror stream (since we are using the same routines to allocate
  // all memory, either tagged or not, we may have failed to allocate
  // memory for streams).  This should not be necessary once we separate
  // the memory management 

  if (!stream_user_error) {
    fprintf(stderr, "{ERROR (%s): %s}\n", source_path, mesg);
    fprintf(stderr, 
"{Ciao was probably compiled in a machine with a different memory model.}\n");
    fprintf(stderr, 
"{Please recompile Ciao in this machine and try again.}\n");
  } else {
    // Issue a simple message if a single thread is running.
    if (num_tasks_created() > 1) {
      THREAD_ID thrid = Thread_Id;   // Local Id

      intmach_t goal_id = eng_goal_from_thread_id(thrid);
      if (goal_id != 0) {
        ENG_PRINTF(stream_user_error,
		   "{ERROR (%s, goal 0x%" PRIxm ", thread 0x%" PRIxm "): %s}\n",
		   source_path, (uintmach_t)goal_id,
		   (uintmach_t)thrid, mesg);
      } else {
        ENG_PRINTF(stream_user_error,
		   "{ERROR (%s, thread 0x%" PRIxm "): %s}\n",
		   source_path,
		   (uintmach_t)thrid, mesg);
      }
    } else {
        ENG_PRINTF(stream_user_error, "{ERROR: %s}\n", mesg);
    }
  }
  if (!wam_initialized){
    printf("Wam not initialized, exiting!!!\n");
    at_exit(-1);
  }
}

/*-----------------------------------------------------------*/

/* segfault patch -- jf */
CVOID__PROTO(trail_push_check, tagged_t x) {
  CIAO_REG_1(tagged_t *, tr);
  tr = w->trail_top;

  TrailPush(tr,x);
  w->trail_top = tr;
  if (ChoiceYounger(w->node,TrailOffset(tr,CHOICEPAD)))
    choice_overflow(Arg,CHOICEPAD);
}

/*------------------------------------------------------------*/

/* insert atom in global table */
/*  MCL: there is an implicit assumption that the table is not full */

#if defined(USE_ATOM_LEN)
static sw_on_key_node_t *atom_gethash(sw_on_key_t *sw,
				      tagged_t key,
				      char *str,
				      uintmach_t str_len)
#else
static sw_on_key_node_t *atom_gethash(sw_on_key_t *sw,
				      tagged_t key,
				      char *str)
#endif
{
  sw_on_key_node_t *hnode;
#if defined(ATOMGC)
  sw_on_key_node_t *first_erased = NULL;
#endif
  intmach_t i;
  tagged_t t0;

  for (i=0, t0=key & sw->mask;
       ;
       i+=sizeof(sw_on_key_node_t), t0=(t0+i) & sw->mask) {
    hnode = SW_ON_KEY_NODE_FROM_OFFSET(sw, t0);
#if !defined(ATOMGC)
    if ((hnode->key==key 
#if defined(USE_ATOM_LEN)
         && hnode->value.atomp->atom_len == str_len
#endif
         && strcmp(hnode->value.atomp->name, str)==0) ||
        !hnode->key)
      return hnode;
#else
    if ((hnode->key == key) 
#if defined(USE_ATOM_LEN)
        && hnode->value.atomp->atom_len == str_len
#endif
        && (strcmp(hnode->value.atomp->name, str) == 0))
      return hnode;
    else if (!hnode->key)
      return first_erased ? first_erased : hnode;
    else if (hnode->key == 1 && !first_erased)
      first_erased = hnode;
#endif
  }
}

tagged_t init_atom_check(char *str)
{
  sw_on_key_node_t *hnode;
  unsigned int hashcode = 0;
  intmach_t count, size;
  intmach_t current_mem = total_mem_count;
  char *c = str;

#if defined(USE_ATOM_LEN)
  uintmach_t atom_len = 0;
#endif
  
  while (*c) {
    hashcode = (hashcode<<1) + *((unsigned char *)c++);
#if defined(USE_ATOM_LEN)
    atom_len++;
#endif
  }

  hashcode = (hashcode<<3)+4;	/* low bits are masked away; ensure it is
				   not 0 --- it cannot be 1, either, which is
				   very important for atom GC */
/*
  while ((hnode=incore_gethash(ciao_atoms, (tagged_t)hashcode)) &&
	 hnode->key==(tagged_t)hashcode &&
	 strcmp(hnode->value.atomp->name, str)!=0)
    hashcode += 233509<<3;         233509 is prime, and so is
				   233509&0x1ffff, 233509&0x0ffff, ...,
				   233509&0x00007
*/

#if defined(USE_ATOM_LEN)
  hnode = atom_gethash(ciao_atoms, (tagged_t)hashcode, str, atom_len);
#else
  hnode = atom_gethash(ciao_atoms, (tagged_t)hashcode, str);
#endif

#if defined(ATOMGC)
  if (hnode->key && hnode->key != 1) /* if ATOMGC, '1' marks freed position */
#else
  if (hnode->key)
#endif
    return MakeAtom(hnode->value.atomp->index);

  if ((count=ciao_atoms->count) > MaxAtomCount) {
    SERIOUS_FAULT("the atom table is full");
  }

  /* Check for a full table, and expand if needed */

  if ((count+1)<<1 > (size=SwitchSize(ciao_atoms))) {
    sw_on_key_t *new_table = new_switch_on_key(size<<1, NULL);
    intmach_t i;
    sw_on_key_node_t *h1, *h2;

#if defined(ATOMGC) && defined(DEBUG)
    /*printf("Reallocing atom table (count = %d)\n", count);*/
#endif

    for (i=0; i<count; i++){
#if defined(ATOMGC)   /* Actually, if the table is full, no entry should be
                         null... */
       /* size *= 2; */
      if ((h1 = atmtab[i]) != NULL) { /* There may be holes when doing GC */
#if defined(USE_ATOM_LEN)
        atmtab[i] = h2 = atom_gethash(new_table, 
                                      h1->key, 
                                      str,
                                      h1->value.atomp->atom_len);
#else
        atmtab[i] = h2 = atom_gethash(new_table, h1->key, str);
#endif
        h2->key = h1->key;
        h2->value.atomp = h1->value.atomp;
      }
#else
      h1 = atmtab[i];
#if defined(USE_ATOM_LEN)
      atmtab[i] = h2 = atom_gethash(new_table, 
                                    h1->key, 
                                    str,
                                    h1->value.atomp->atom_len);
#else
      atmtab[i] = h2 = atom_gethash(new_table, h1->key, str);
#endif
      h2->key = h1->key;
      h2->value.atomp = h1->value.atomp;
#endif
    }

    atmtab = checkrealloc_ARRAY(sw_on_key_node_t *,
				count,
				2*count,
				(tagged_t *)atmtab);

#if defined(ATOMGC)      /* Clean up the upper part of the new atom table */
    for (i = count; i < 2*count; i++)
      atmtab[i] = NULL;
    new_table->next_index = count;
#endif

    checkdealloc_FLEXIBLE(sw_on_key_t,
			  sw_on_key_node_t,
			  size,
			  ciao_atoms);
    new_table->count = count;
#if defined(USE_ATOM_LEN)
    hnode = atom_gethash(new_table, (tagged_t)hashcode, str, atom_len);
#else
    hnode = atom_gethash(new_table, (tagged_t)hashcode, str);
#endif
    ciao_atoms = new_table;
    size = size << 1;
  }
  hnode->key = (tagged_t)hashcode;

#if defined(ATOMGC)
    size = size >> 1;     /* atmtab size is one half of ciao_atoms size */
    count = ciao_atoms->next_index;
    while(atmtab[count])                  /* There must be one free entry */
      count =  (count + 1) % size;
    /*ciao_atoms->next_index+1 == size ? 0 : ciao_atoms->next_index+1;*/
    /* next_index should point to a free entry in the table */
    ciao_atoms->next_index = count;
#endif

#if defined(USE_ATOM_LEN)
  hnode->value.atomp = new_atom_check(str, atom_len, count);
#else
  hnode->value.atomp = new_atom_check(str, count);
#endif
  atmtab[count] = hnode;

  ciao_atoms->count++;

  INC_MEM_PROG(total_mem_count - current_mem);

  return MakeAtom(count);
}

/* make large object on the heap */
CFUN__PROTO(make_large, tagged_t,
	    tagged_t *ptr)
{
  tagged_t *h = w->global_top;
  tagged_t f = *ptr;
  intmach_t ar = LargeArity(f);
  intmach_t i;

  for (i=0; i<ar; i++)
    *h++ = *ptr++;
  *h++ = f;

  w->global_top = h;
  return Tag(STR, h-ar-1);
}

#if BC_SCALE == 2
/* Make large object on the heap from bytecode.

   If the object is a bignum, we use the canonized length and convert
   it to a NUM if possible (this may happend with BC_SCALE == 2).
 */
CFUN__PROTO(bc_make_large, tagged_t, tagged_t *ptr) {
  intmach_t ar;
  tagged_t f = ptr[0];

  if (FunctorIsFloat(f)) { /* float */
    // fprintf(stderr, "BC_MakeLarge->float\n");
    ar = LargeArity(f);
  } else { /* bignum */
    intmach_t len = bn_canonized_length((bignum_t *)ptr);
    ar = len + 1;
    /* TODO: factorize */
    if (ar==2 && IntIsSmall((intmach_t)ptr[1])) {
      // fprintf(stderr, "BC_MakeLarge->small\n");
      return MakeSmall(ptr[1]);
    }
    // fprintf(stderr, "BC_MakeLarge->bignum\n");
    f = MakeLength(len);
  }

  /* Copy large into the heap */
  tagged_t *h = w->global_top;
  *h++ = f; ptr++;
  for (intmach_t i=1; i<ar; i++) *h++ = *ptr++;
  *h++ = f;
  w->global_top = h;
  return Tag(STR, h-ar-1);
}

/* (assume t deref) */
CBOOL__PROTO(bc_eq_large, tagged_t t, tagged_t *ptr) {
  intmach_t ar;
  tagged_t f = ptr[0];

  if (FunctorIsFloat(f)) { /* float */
    // fprintf(stderr, "eq_large->float\n");
    ar = LargeArity(f);
  } else { /* bignum */
    intmach_t len = bn_canonized_length((bignum_t *)ptr);
    ar = len + 1;
    /* TODO: factorize */
    if (ar==2 && IntIsSmall((intmach_t)ptr[1])) {
      // fprintf(stderr, "eq_large->small\n");
      return t == MakeSmall(ptr[1]);
    }
    // fprintf(stderr, "eq_large->bignum\n");
  }

  /* Compare large from the heap */
  if (!TagIsSTR(t)) return FALSE;
  for (intmach_t i=ar; i>0; i--) {
    if (ptr[i-1] != *TagToArg(t,i-1)) return FALSE;
  }
  return TRUE;
}
#endif

#define NumstackBlockSize(b) ((char *)b->end - (char *)b)

CVOID__PROTO(numstack_init)
{
  intmach_t lsize = 1020;

  Numstack_First = (numstack_t *)checkalloc(lsize);
  Numstack_First->next = NULL;
  Numstack_First->end = (tagged_t *)((char *)Numstack_First + lsize);

  Numstack_End = NULL;
}

static CVOID__PROTO(numstack_overflow)
{
  numstack_t *next;

  if (!Numstack_End) {
    while ((next=Numstack_First->next)) {
      checkdealloc((tagged_t *)Numstack_First,
                   NumstackBlockSize(Numstack_First));
      Numstack_First = next;
    }

    Numstack_Last = Numstack_First;
  } else {
    intmach_t lsize = 2*NumstackBlockSize(Numstack_Last);

    Numstack_Last->next = next = (numstack_t *)checkalloc(lsize);
    next->next = NULL;
    next->end = (tagged_t *)((char *)next + lsize);
    Numstack_Last = next;
  }
  Numstack_Top = (tagged_t *)(Numstack_Last+1);
  Numstack_End = Numstack_Last->end;
}

CFUN__PROTO(bn_call,
	    tagged_t,
	    bn_fun_t f,
	    tagged_t x, tagged_t y,
	    bcp_t liveinfo)
{
  bignum_size_t req;
  tagged_t xx[2], yy[2];

  if (f != bn_from_float) {
    /* bn_from_float is the unique call that accepts floats,
       everything else must be converted to bignums */
    if (IsFloat(x) || IsFloat(y)) {
      SERIOUS_FAULT("bn_call: called with floats");
    }
  }

  bignum_t *bx = (bignum_t *)0;
  bignum_t *by = (bignum_t *)0;
  if (TagIsSTR(x)) {
    bx = (bignum_t *)TagToSTR(x);
  } else if (TagIsSmall(x)) {
    xx[0] = MakeFunctorFix;
    xx[1] = GetSmall(x);
    bx = (bignum_t *)xx;
  }

  if (TagIsSTR(y)) {
    by = (bignum_t *)TagToSTR(y);
  } else if (TagIsSmall(y)) {
    yy[0] = MakeFunctorFix;
    yy[1] = GetSmall(y);
    by = (bignum_t *)yy;
  }

  tagged_t r;
  if (liveinfo) {
    req = (*f)(bx, by, (bignum_t *)w->global_top, (bignum_t *)(Heap_End-LIVEINFO__HEAP(liveinfo)));
    if (req != 0) {
      /* TODO: why is the Numstack used here? */
      while (Numstack_Top+req > Numstack_End) {
        numstack_overflow(Arg);
      }
      if ((*f)(bx, by, (bignum_t *)Numstack_Top, (bignum_t *)Numstack_End))
        SERIOUS_FAULT("miscalculated size of bignum");
      explicit_heap_overflow(Arg,req+LIVEINFO__HEAP(liveinfo), (short)LIVEINFO__ARITY(liveinfo));
      if (bn_plus((bignum_t *)Numstack_Top, (bignum_t *)0, (bignum_t *)w->global_top, (bignum_t *)(Heap_End-LIVEINFO__HEAP(liveinfo))))
        SERIOUS_FAULT("miscalculated size of bignum");
    }
    FinishInt(w->global_top, r);
  } else {
    while (!Numstack_End || (*f)(bx, by, (bignum_t *)Numstack_Top, (bignum_t *)Numstack_End)) {
      numstack_overflow(Arg);
    }
    FinishInt(Numstack_Top, r);
  }
  return r;
}

CFUN__PROTO(make_integer_check,
	    tagged_t,
	    intmach_t i,
	    bcp_t liveinfo)
{
  tagged_t *h;

  if (IntIsSmall(i)) return MakeSmall(i);

  if (liveinfo) { /* compute final value */
    h = w->global_top;
    if (HeapDifference(h, Heap_End) < (intmach_t)LIVEINFO__HEAP(liveinfo)+3) {
      explicit_heap_overflow(Arg, LIVEINFO__HEAP(liveinfo)+3, (short)LIVEINFO__ARITY(liveinfo));
      h = w->global_top;
    }
    w->global_top = h+3;
  } else { /* compute intermediate value */
    h = Numstack_Top;
    if (h+3 > Numstack_End) {
      numstack_overflow(Arg);
      h = Numstack_Top;
    }
    Numstack_Top = h+3;
  }

  HeapPush(h, MakeFunctorFix);
  HeapPush(h, (tagged_t)i);
  HeapPush(h, MakeFunctorFix);
  return Tag(STR, h-3);
}

CFUN__PROTO(make_float_check, tagged_t,
	    flt64_t i,
	    bcp_t liveinfo)
{
  tagged_t *h;
  union {
    flt64_t i;
    tagged_t p[sizeof(flt64_t)/sizeof(tagged_t)];
  } u;

  if (liveinfo) { /* compute final value */
    h = w->global_top;
    if (HeapDifference(h, Heap_End) < (intmach_t)LIVEINFO__HEAP(liveinfo)+4) {
      explicit_heap_overflow(Arg,LIVEINFO__HEAP(liveinfo)+4, (short)LIVEINFO__ARITY(liveinfo));
      h = w->global_top;
    }
    w->global_top = h+4;
  } else { /* compute intermediate value */
    h = Numstack_Top;
    if (h+4 > Numstack_End) {
      numstack_overflow(Arg);
      h = Numstack_Top;
    }
    Numstack_Top = h+4;
  }

  HeapPush(h, MakeFunctorFloat);
  u.i = i;
#if LOG2_bignum_size == 5
  HeapPush(h, u.p[0]);
  HeapPush(h, u.p[1]);
#elif LOG2_bignum_size == 6
  HeapPush(h, u.p[0]);
  HeapPush(h, 0); /* dummy, for BC_SCALE==2 */
#endif
  HeapPush(h, MakeFunctorFloat);
  return Tag(STR, h-4);
}

CFUN__PROTO(make_integer, tagged_t, intmach_t i) {
  tagged_t *h = w->global_top;

  HeapPush(h, MakeFunctorFix);
  HeapPush(h, (tagged_t)i);
  HeapPush(h, MakeFunctorFix);
  w->global_top = h;
  return Tag(STR, h-3);
}

CFUN__PROTO(make_float, tagged_t, flt64_t i) {
  union {
    flt64_t i;
    tagged_t p[sizeof(flt64_t)/sizeof(tagged_t)];
  } u;
  tagged_t *h;

  h = w->global_top;
  HeapPush(h, MakeFunctorFloat);
  u.i = i;
#if LOG2_bignum_size == 5
  HeapPush(h, u.p[0]);
  HeapPush(h, u.p[1]);
#elif LOG2_bignum_size == 6
  HeapPush(h, u.p[0]);
  HeapPush(h, 0); /* dummy, for BC_SCALE==2 */
#endif
  HeapPush(h, MakeFunctorFloat);
  w->global_top = h;
  return Tag(STR, h-4);
}

/*-------------------------------------------------------*/

/* Inserts the definition of a predicate, either asserted, compiled,
   consulted, or qloaded. */

definition_t *new_functor(tagged_t tagpname, int arity)
{
  definition_t *func;
  intmach_t i;

  /* How to get the printable name (i.e., accessing to the atom part):

  if ((tagpname & 3) == 0)
    printf("New predicate head %s, arity %d\n", GetString(tagpname), arity);
    */

  func = checkalloc_TYPE(definition_t);
  /* Initialize all fields to 0 */
  for (i=0; i<sizeof(definition_t); i++) {
    ((char *)func)[i] = 0;
  }
  
  func->printname = tagpname;
  func->arity = arity;
  SetEnterInstr(func, ENTER_UNDEFINED);
  func->code.undinfo = NULL;
  return func;
}

/*-------------------------------------------------------*/

/* Inserts the definition of module. */

module_t *new_module(tagged_t mod_atm)
{
  module_t *mod;

  mod = checkalloc_TYPE(module_t);
  mod->printname = mod_atm;
  mod->properties.is_static = FALSE;
  return mod;
}

/*------------------------------------------------------------*/

void expand_sw_on_key(sw_on_key_t **psw,
		      try_node_t *otherwise,
		      bool_t deletep)
{
  sw_on_key_node_t *h1, *h2;
  intmach_t size = SwitchSize(*psw);
  sw_on_key_t *newsw = new_switch_on_key(size<<1,otherwise);
  intmach_t j;

  for (j=size-1; j>=0; --j) {
    h1 = &(*psw)->node[j];
    if (h1->key) {
      newsw->count++;
      h2 = incore_gethash(newsw,h1->key);
      h2->key = h1->key;
      h2->value.try_chain = h1->value.try_chain;
    }
  }

  if (deletep) {
    checkdealloc_FLEXIBLE(sw_on_key_t,
			  sw_on_key_node_t,
			  size,
			  *psw);
  } else {
    leave_to_gc(TABLE, (char *)(*psw));
  }

  (*psw) = newsw;
}

void add_definition(sw_on_key_t **swp,
		    sw_on_key_node_t *node,
		    tagged_t key,
		    definition_t *def)
{
  node->key=key;
  node->value.def=def;
  if (((*swp)->count+=1)<<1 > SwitchSize(*swp))
    expand_sw_on_key(swp,NULL,FALSE);
}

definition_t *insert_definition(sw_on_key_t **swp,
				tagged_t tagpname,
				int arity,
				bool_t insertp)
{
  sw_on_key_node_t *keyval;
  definition_t *value = NULL;
  tagged_t key=SetArity(tagpname,arity);

  /* Lock here -- we do not want two different workers to add predicates
     concurrently. */

  Wait_Acquire_slock(prolog_predicates_l);
  keyval = (sw_on_key_node_t *)incore_gethash((*swp),key);

  if (keyval->key)                                    /* Already existent */
    value = keyval->value.def;
  else if (insertp){                                      /* New predicate */
    value=new_functor(tagpname, arity);
    add_definition(swp, keyval, key, value);
  }

  Release_slock(prolog_predicates_l);

  return value;
}

void add_module(sw_on_key_t **swp,
		sw_on_key_node_t *node,
		tagged_t key,
		module_t *mod)
{
  node->key=key;
  node->value.mod=mod;
  if (((*swp)->count+=1)<<1 > SwitchSize(*swp))
    expand_sw_on_key(swp,NULL,FALSE);
}

module_t *insert_module(sw_on_key_t **swp,
			tagged_t mod_atm,
			bool_t insertp)
{
  sw_on_key_node_t *keyval;
  module_t *value = NULL;
  tagged_t key = mod_atm;

  /* Lock here -- we do not want two different workers to add modules
     concurrently. */

  Wait_Acquire_slock(prolog_modules_l);
  keyval = (sw_on_key_node_t *)incore_gethash((*swp),key);

  if (keyval->key)                                    /* Already existent */
    value = keyval->value.mod;
  else if (insertp){                                      /* New module */
    value = new_module(mod_atm);
    add_module(swp, keyval, key, value);
  }

  Release_slock(prolog_modules_l);

  return value;
}

/*------------------------------------------------------------*/

/* Create a most general term for a given functor or small int. */
CFUN__PROTO(make_structure, tagged_t,
	    tagged_t functor)
{
  intmach_t ar = Arity(functor);
  tagged_t *h = w->global_top;

  if (ar==0 || !TagIsATM(functor))
    return functor;
  else if (functor==functor_list) {
    ConstrHVA(h);
    ConstrHVA(h);
    w->global_top = h;
    return Tag(LST,HeapOffset(h,-2));
  } else {
    HeapPush(h,functor);
    do {
      ConstrHVA(h);
    } while (--ar);
    w->global_top = h;

    return Tag(STR,h-Arity(functor)-1);
  }
}

definition_t *find_definition(sw_on_key_t **swp,
			      tagged_t term, tagged_t **argl,
			      bool_t insertp)
{
  int arity;

  if (TagIsStructure(term)) {
    tagged_t f = TagToHeadfunctor(term);

    *argl = TagToArg(term,1);
    term = SetArity(f,0);
    arity = Arity(f);
  } else
    if (TagIsLST(term)) {
      *argl = TagToLST(term);
      term = atom_list;
      arity = 2;
    }
    else
      arity = 0;

  return insert_definition(swp,term,arity,insertp);
}

/* Enter here with a *definition name* as generated by the compiler. */
definition_t *parse_definition(tagged_t complex)
{
  tagged_t a,b;

  if (TagIsSTR(complex) && (TagToHeadfunctor(complex)==functor_slash)) {
    DerefArg(a,complex,1);
    DerefArg(b,complex,2);
    return parse_1_definition(a,b);
  }
  else return NULL;
}

static definition_t **find_subdef_chain(definition_t *f, intmach_t clause_no)
{
  incore_info_t *d = f->code.incoreinfo;
  emul_info_t *ep;

  if (clause_no != 0 && clause_no <= d->clauses_tail->number)
    for (ep = d->clauses.ptr; --clause_no; ep = ep->next.ptr)
      ;
  else {
    /* TODO: (JFMC) assumes that &(d->clauses_tail->next) == d->clauses_tail */
    ep = (emul_info_t *)d->clauses_tail;
    if (!IS_CLAUSE_TAIL(ep) || ep->next.ptr == NULL) {
      ALLOC_CLAUSE_TAIL(ep);
      ep->next.number = d->clauses_tail->number + 1;
      d->clauses_tail->ptr = ep;
      d->clauses_tail = &ep->next;
    }
  }
  return &ep->subdefs;
}

static definition_t *parse_1_definition(tagged_t tagname, tagged_t tagarity)
{
  int arity;

  if (!TagIsSmall(tagarity))
    return NULL;
  arity = GetSmall(tagarity);
  if (TagIsSTR(tagname) && (TagToHeadfunctor(tagname)==functor_minus))
    /* "internal" predicate */
    {
      definition_t *f, *f1, **pf;
      tagged_t tmp;
      intmach_t i;
      intmach_t subdef_no, clause_no;

      DerefArg(tmp,tagname,2);
      subdef_no = GetSmall(tmp);
      DerefArg(tagname,tagname,1);

      DerefArg(tmp,tagname,2);
      clause_no = GetSmall(tmp);
      DerefArg(tagname,tagname,1);

      f = parse_definition(tagname);
      if (f==NULL)
	return NULL;
      i = f->predtyp;
      if (i > ENTER_FASTCODE_INDEXED)
	return NULL;
      pf = find_subdef_chain(f, clause_no);

      if (!(*pf))
	f = *pf = new_functor((tagged_t)f|3, arity);
      else
	{
	  for (i=1, f1 = *pf;
	       !(f1->printname&2);
	       i++, f1 = (definition_t *)TagToPointer(f1->printname))
	    if (i==subdef_no) break;
	
	  if (i==subdef_no) return f1;
	  f1->printname = (tagged_t)(f=new_functor(f1->printname, arity))|1;
	}
      return f;
    }

  if (TagIsATM(tagname)) 
    return insert_definition(predicates_location,tagname,arity,TRUE);
  else return NULL;
}

static CBOOL__PROTO(cunify_args_aux,
		    int arity, tagged_t *pt1, tagged_t *pt2,
		    tagged_t *x1, tagged_t *x2);
static CBOOL__PROTO(cunify_aux, tagged_t x1, tagged_t x2);

/* Unify the argument lists of two compund terms.
 * pt1 - first argument list.
 * pt2 - second argument list.
 * arity - number of arguments.
 */
CBOOL__PROTO(cunify_args, 
	     int arity,
	     tagged_t *pt1,
	     tagged_t *pt2)
{
  tagged_t x1, x2;
  bool_t result =
    (cunify_args_aux(Arg,arity,pt1,pt2,&x1,&x2) && cunify_aux(Arg,x1,x2));
  intmach_t i = w->value_trail;

  if (i<InitialValueTrail) {
    pt2 = (tagged_t *)w->node;
    do {
      pt1 = (tagged_t *)pt2[i++];
      *pt1 = pt2[i++];
    } while (i<InitialValueTrail);
    w->value_trail = (intmach_t)InitialValueTrail;
  }

  return result;
}

static CBOOL__PROTO(cunify_args_aux, 
		    int arity,
		    tagged_t *pt1,
		    tagged_t *pt2,
		    tagged_t *x1,
		    tagged_t *x2)
{
  tagged_t t1 = ~0;
  tagged_t t2 = ~0;
  tagged_t t3;

  /* Terminating unification of complex structures: Forward args of pt2 to
     args of pt1 using choice stack as value cell.  When done, reinstall
     values. */

  if (ChoiceYounger(ChoiceOffset(w->node,2*CHOICEPAD-w->value_trail),w->trail_top))
				/* really: < 2*arity */
    choice_overflow(Arg,2*CHOICEPAD);
  for (; arity>0; --arity) {
    t1 = *pt1, t2 = *pt2;
    if (t1 != t2) {
      DerefHeapSwitch(t1,t3,goto noforward;);
      DerefHeapSwitch(t2,t3,goto noforward;);
      if (t1!=t2 && IsComplex(t1&t2)) {
        /* replace smaller value by larger value,
           using choice stack as value trail */
        tagged_t *b = (tagged_t *)w->node;
        intmach_t i = w->value_trail;

        if (t1>t2)
          b[--i] = *pt1,
            b[--i] = (tagged_t)pt1,
            *pt1 = t2;
        else
          b[--i] = *pt2,
            b[--i] = (tagged_t)pt2,
            *pt2 = t1;
        w->value_trail = i;
      noforward:
        if (arity>1 && !cunify_aux(Arg,t1,t2))
          return FALSE;
      } else if (t1 != t2)
        return FALSE;
    }
    (void)HeapNext(pt1);
    (void)HeapNext(pt2);
  }

  *x1 = t1, *x2 = t2;

  if (ChoiceYounger(ChoiceOffset(w->node,CHOICEPAD-w->value_trail),w->trail_top))
    choice_overflow(Arg,CHOICEPAD);
  return TRUE;
}

/* Unify two terms.
 * x1 - first term
 * x2 - second term
 */

/* NOTE: This is a recursive version of Robinson's 1965 unification
   algorithm without occurs check */

CBOOL__PROTO(cunify, tagged_t x1, tagged_t x2)
{
  bool_t result = cunify_aux(Arg,x1,x2);
  intmach_t i = w->value_trail;

  if (i<InitialValueTrail) {
    tagged_t *pt1, *pt2;

    pt2 = (tagged_t *)w->node;
    do {
      pt1 = (tagged_t *)pt2[i++];
      *pt1 = pt2[i++];
    } while (i<InitialValueTrail);
    w->value_trail = (intmach_t)InitialValueTrail;
  }

  return result;
}

static CBOOL__PROTO(cunify_aux, tagged_t x1, tagged_t x2)
{
  tagged_t u, v, t1;

 in:
  u=x1, v=x2;

  SwitchOnVar(u,t1,
	      {goto u_is_hva;},
	      {goto u_is_cva;},
	      {goto u_is_sva;},
	      ;);

				/* one non variable */
  SwitchOnVar(v,t1,
	      { BindHVA(v,u); goto win; },
	      { BindCVA(v,u); goto win; },
	      { BindSVA(v,u); goto win; },
	      ;);

				/* two non variables */
  if (!(v ^= u))		/* are they equal? */
    goto win;
  else if (v>=QMask)		/* not the same type? */
    goto lose;
  else if (!(u & TagBitComplex)) /* atomic? (& not LNUM)*/
    goto lose;
  else if (!(u & TagBitFunctor)) /* list? */
    {
      v ^= u;			/* restore v */
      if (cunify_args_aux(Arg,2,TagToCar(u),TagToCar(v),&x1,&x2))
	goto in;
      else
	goto lose;
    }
  else				/* structure. */
    {
      v ^= u;			/* restore v */
      if (TagToHeadfunctor(u) != (t1=TagToHeadfunctor(v)))
	goto lose;
      else if (t1&QMask)	/* large number */
	{
	  intmach_t i;
	
	  for (i = LargeArity(t1)-1; i>0; i--)
	    if (*TagToArg(u,i) != *TagToArg(v,i)) goto lose;
	  goto win;
	}
      if (cunify_args_aux(Arg,Arity(t1),TagToArg(u,1),TagToArg(v,1),&x1,&x2))
	goto in;
      else
	goto lose;
    }

 u_is_hva:
  SwitchOnVar(v,t1,
	      { if (u==v)
		  ;
		else if (YoungerHeapVar(TagToHVA(v),TagToHVA(u)))
		  BindHVA(v,u)
		else
		  BindHVA(u,v); },
	      { BindHVA(u,v); },
	      { BindSVA(v,u); },
	      { BindHVA(u,v); });
  goto win;

 u_is_cva:
  SwitchOnVar(v,t1,
	      { BindHVA(v,u); },
	      { if (u==v)
		  ;
		else if (YoungerHeapVar(TagToCVA(v),TagToCVA(u)))
		  { BindCVA(v,u); }
		else
		  { BindCVA(u,v); } },
	      { BindSVA(v,u); },
	      { BindCVA(u,v); });
  goto win;

 u_is_sva:
  for (; TagIsSVA(v); v = t1)
    {
      RefSVA(t1,v);
      if (v == t1)
	{
	  if (u==v)
	    ;
	  else if (YoungerStackVar(TagToSVA(v),TagToSVA(u)))
	    BindSVA(v,u)
	  else
	    BindSVA(u,v);
	  goto win;
	}
    }
  BindSVA(u,v);

 win:
  return TRUE;

 lose:
  return FALSE;
}

/* ------------------------------------------------------------------------- */
/* instance */
static CBOOL__PROTO(cinstance_args_aux,
		    int arity, tagged_t *pt1, tagged_t *pt2,
		    tagged_t *x1, tagged_t *x2, intmach_t *n);
static CBOOL__PROTO(cinstance_aux, tagged_t x1, tagged_t x2, intmach_t *n);

CBOOL__PROTO(cinstance_args, 
	     int arity,
	     tagged_t *pt1,
	     tagged_t *pt2,
	     intmach_t *n)
{
  tagged_t x1, x2;
  return cinstance_args_aux(Arg,arity,pt1,pt2,&x1,&x2,n) && cinstance_aux(Arg,x1,x2,n);
}

static CBOOL__PROTO(cinstance_args_aux, 
		    int arity,
		    tagged_t *pt1,
		    tagged_t *pt2,
		    tagged_t *x1,
		    tagged_t *x2,
		    intmach_t *n)
{
  tagged_t 
    t1 = ~0,
    t2 = ~0;

  /* Terminating unification of complex structures: Forward args of pt2 to
     args of pt1 using choice stack as value cell.  When done, reinstall
     values. */

  if (ChoiceYounger(ChoiceOffset(w->node,2*CHOICEPAD),w->trail_top))
				/* really: < 2*arity */
    choice_overflow(Arg,2*CHOICEPAD);
  for (; arity>0; --arity) {
    t1 = *pt1, t2 = *pt2;
    if (arity>1 && !cinstance_aux(Arg,t1,t2,n))
      return FALSE;
    (void)HeapNext(pt1);
    (void)HeapNext(pt2);
  }

  *x1 = t1, *x2 = t2;

  if (ChoiceYounger(ChoiceOffset(w->node,CHOICEPAD),w->trail_top))
    choice_overflow(Arg,CHOICEPAD);
  return TRUE;
}

CVOID__PROTO(pop_choicept);
CVOID__PROTO(push_choicept, try_node_t *alt);

CBOOL__PROTO(cinstance)
{
  tagged_t t1, /* t2,*/ *pt1, *pt2;
  int result;
  intmach_t n = 0;

#if 0
  t1 = X(0);
  t2 = X(1);
#endif

  push_choicept(Arg,fail_alt);	/* try, arity=0 */

  result = cinstance_aux(Arg,X(0),X(1),&n);

  pt1 = pt2 = TagToPointer(w->node->trail_top); /* untrail */
  while (!OffTrailtop(pt2,w->trail_top)) {
    t1 = TrailNext(pt2);	/* old var */
    *TagToPointer(t1) = t1;
  }
  w->trail_top = pt1;

  pop_choicept(Arg);		/* trust */

  return result;
}

static CBOOL__PROTO(cinstance_aux, 
		    tagged_t x1,
		    tagged_t x2,
		    intmach_t *n)
{
  tagged_t u, v, t1, nt;

 in:
  u=x1, v=x2;

  nt = MakeSmall(*n);
  SwitchOnVar(u,t1,
	      { goto u_is_hva; },
	      { goto lose; }, /* CVAs are not supported */
	      { goto u_is_sva; },
	      { goto one_non_var; });
  /* note that if deref(u) == deref(v), the following code must do nothing */
 u_is_hva:
  SwitchOnVar(v,t1,
              { BindHVA(v, nt); },
              { goto lose; }, /* CVAs are not supported */
	      { BindSVA(v, nt); },
	      { goto lose; });
  if (u != v) BindHVA(u, nt);
  goto var_win;

 u_is_sva:
  SwitchOnVar(v,t1,
              { BindHVA(v, nt); },
              { goto lose; }, /* CVAs are not supported */
	      { BindSVA(v, nt); },
	      { goto lose; });
  if (u != v) BindSVA(u, nt);
  goto var_win;

 var_win: 
  (*n)++;
  goto win;

 one_non_var:
				/* one non variable */
  SwitchOnVar(v,t1,
	      { BindHVA(v,u); goto win; },
	      { BindCVA(v,u); goto win; },
	      { BindSVA(v,u); goto win; },
	      ;);

				/* two non variables */
  if (!(v ^= u))		/* are they equal? */
    goto win;
  else if (v>=QMask)		/* not the same type? */
    goto lose;
  else if (!(u & TagBitComplex)) /* atomic? (& not LNUM)*/
    goto lose;
  else if (!(u & TagBitFunctor)) /* list? */
    {
      v ^= u;			/* restore v */
      if (cinstance_args_aux(Arg,2,TagToCar(u),TagToCar(v),&x1,&x2,n))
	goto in;
      else
	goto lose;
    }
  else				/* structure. */
    {
      v ^= u;			/* restore v */
      if (TagToHeadfunctor(u) != (t1=TagToHeadfunctor(v)))
	goto lose;
      else if (t1&QMask)	/* large number */
	{
	  intmach_t i;
	
	  for (i = LargeArity(t1)-1; i>0; i--)
	    if (*TagToArg(u,i) != *TagToArg(v,i)) goto lose;
	  goto win;
	}
      if (cinstance_args_aux(Arg,Arity(t1),TagToArg(u,1),TagToArg(v,1),&x1,&x2,n))
	goto in;
      else
	goto lose;
    }

 win:
  return TRUE;

 lose:
  return FALSE;
}

/* ------------------------------------------------------------------------- */
/* ground */
static CBOOL__PROTO(cground_args_aux, int arity, tagged_t *pt1, tagged_t *x1);
static CBOOL__PROTO(cground_aux, tagged_t x1);

static CBOOL__PROTO(cground_args_aux, 
		    int arity,
		    tagged_t *pt1,
		    tagged_t *x1)
{
  tagged_t 
    t1 = ~0;
  for (; arity>0; --arity) {
    t1 = *pt1;
    if (arity > 1 && !cground_aux(Arg,t1)) return FALSE;
    (void)HeapNext(pt1);
  }
  *x1 = t1;
  return TRUE;
}

CBOOL__PROTO(cground)
{
  return cground_aux(Arg,X(0));
}

static CBOOL__PROTO(cground_aux, tagged_t x1)
{
  tagged_t u, t1;

 in:
  u=x1;

  SwitchOnVar(u,t1,
	      { goto lose; },
	      { goto lose; }, /* CVAs are not supported */
	      { goto lose; },
	      { goto non_var; });

 non_var:
  if (TagIsATM(u)) goto win;
  if (TagIsSmall(u)) goto win;
  if (TagIsLST(u)) {
      if (cground_args_aux(Arg,2,TagToCar(u),&x1))
	goto in;
      else
	goto lose;
    }
  else				/* structure. */
    {
      t1=TagToHeadfunctor(u);
      if (t1&QMask)	/* large number */
	{
	  goto win;
	}
      if (cground_args_aux(Arg,Arity(t1),TagToArg(u,1),&x1))
	goto in;
      else
	goto lose;
    }

 lose:
  return FALSE;
 win:
  return TRUE;
}

/* ------------------------------------------------------------------------- */

/* Pre: !IsSmall(t) (small int's taken care of by GetInteger()) */
intmach_t get_integer(tagged_t t) {
  if (LargeIsFloat(t)) {
    return get_float(t);
  } else {
    return (intmach_t)*TagToArg(t,1);
  }
}

/* Pre: !IsSmall(t) (small int's taken care of by GetFloat()) */
flt64_t get_float(tagged_t t) {
  if (!LargeIsFloat(t)) {
    intmach_t ar = LargeArity(TagToHeadfunctor(t))-1;
    flt64_t f = (intmach_t)*TagToArg(t,ar);

    while (ar>1) {
      const bignum_t sbit = (bignum_t)1<<(8*sizeof(bignum_t)-1);
#if LOG2_bignum_size == 5
      const flt64_t norm2 = 4294967296.0; /* 2**32 */
      const flt64_t norm2m1 = 2147483648.0; /* 2**31 */
#elif LOG2_bignum_size == 6
      const flt64_t p32 = 4294967296.0; /* 2**32 */
      const flt64_t norm2 = p32*p32; /* 2**64 */
      const flt64_t norm2m1 = p32*2147483648.0; /* 2**63 */
#endif
      bignum_t u = *TagToArg(t,--ar);
      if (u & sbit) { /* trouble on some machines */
        f = f*norm2 + norm2m1 + (u - sbit);
      } else {
        f = f*norm2 + u;
      }
    }
    return f;
  } else { /* LargeIsFloat(t) */
    union {
      flt64_t i;
      tagged_t p[sizeof(flt64_t)/sizeof(tagged_t)];
    } u;
#if LOG2_bignum_size == 5
    u.p[0] = *TagToArg(t,1);
    u.p[1] = *TagToArg(t,2);
#elif LOG2_bignum_size == 6
    u.p[0] = *TagToArg(t,1);
#endif
    return u.i;
  }
}

bool_t float_is_finite(tagged_t t) {
  /* Assume IEEE comparison for floats */
  flt64_t f = get_float(t);
  f = f - f;
  return (f == f);
}

CBOOL__PROTO(prolog_show_nodes)
{
  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  show_nodes(w, ChoiceFromInt(X(0)), ChoiceFromInt(X(1)));
  return TRUE;
}

CBOOL__PROTO(prolog_show_all_nodes)
{
  show_nodes(w, w->node, InitialNode);
  return TRUE;
}


CBOOL__PROTO(start_node)
{
  DEREF(X(0),X(0));
  Unify_constant(ChoiceToInt(InitialNode),X(0));
  return TRUE;
}

#if defined(DEBUG_NODE)
void display_functor(definition_t *functor)
{
  if(functor)
    {
      if(IsString((functor)->printname))
	{
	  fprintf(stderr, "'%s'/", GetString((functor)->printname));
	}
      else
	{
	  fprintf(stderr, "_/");
	}
      fprintf(stderr, "%d", (functor)->arity);
    }
  else
    fprintf(stderr, "_F");
}
# define DisplayCPFunctor(cp) display_functor(cp->functor)
#else
# define DisplayCPFunctor(cp) fprintf(stderr, "_N")
#endif

CVOID__PROTO(show_nodes, node_t *cp_younger, node_t *cp_older)
{
  intmach_t number;
  try_node_t *next_alt;
#if !defined(DEBUG_NODE)
  fprintf(stderr, "/* functor information in nodes not available */\n");
#endif
  fprintf(stderr, "nodes(");
  fprintf(stderr,"0x%p:",cp_younger);
  DisplayCPFunctor(cp_younger);
  fprintf(stderr, ", ");
  fprintf(stderr, "[");
  if (cp_younger->next_alt)
    next_alt = cp_younger->next_alt;
  else
    next_alt = w->next_alt;
  number = next_alt->number;
  cp_younger = ChoiceCharOffset(cp_younger, -next_alt->node_offset);
  while(ChoiceYounger(cp_younger, cp_older)) {
    fprintf(stderr,"\n  ");
    fprintf(stderr,"0x%p:",cp_younger);
    DisplayCPFunctor(cp_younger);
    fprintf(stderr, "/%" PRIdm ",", number);
    number = cp_younger->next_alt->number;
    cp_younger = ChoiceCharOffset(cp_younger,
				  -cp_younger->next_alt->node_offset);
  }
  if (!ChoiceYounger(cp_older, cp_younger)) {
    fprintf(stderr,"\n  ");
    fprintf(stderr,"0x%p:",cp_older);
    DisplayCPFunctor(cp_older);
    fprintf(stderr, "/%" PRIdm "\n",number);
  }
  fprintf(stderr, "])\n");
}
