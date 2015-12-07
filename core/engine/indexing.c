/*
 *  indexing.c
 *
 *  Support for the incremental clause compiler.
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 */

#include <ciao/datadefs.h>
#include <ciao/support_macros.h>
#include <ciao/initial.h>

#include <ciao/locks.h>
#include <ciao/indexing.h>
#include <ciao/wamsupport.h>
#include <ciao/support.h>
#include <ciao/alloc.h>
#include <ciao/wam_alloc.h>
#include <ciao/objareas.h>

static void set_nondet(try_node_t *t,
                       incore_info_t *def,
                       bool_t first);
static void incore_insert(try_node_t  **t0, 
                          int effar,
                          emul_info_t *ref,
                          incore_info_t *def);
static void incore_puthash(sw_on_key_t **psw, 
                           int effar, 
                           emul_info_t *current, 
                           incore_info_t *def,
                           tagged_t k);
static try_node_t *incore_copy(try_node_t *from);
static void free_try(try_node_t **t);
static void free_sw_on_key(sw_on_key_t **sw);
static void free_emulinfo(emul_info_t *cl);
static void free_incoreinfo(incore_info_t **p);
static CVOID__PROTO(make_undefined, definition_t *f);
static void free_info(enter_instr_t enter_instr, char *info);
static void init_interpreted(definition_t *f);


#define ISNOTRY(T)		(((T)==NULL) || ((T)==fail_alt))

/* Indexing for the incore compiler. */

/* Patch bytecode information related to the last clause inserted */

static void set_nondet(try_node_t *t,
		       incore_info_t *def,
		       bool_t first)
{
  uintmach_t i;
  emul_info_t *cl;

  /* Check if we can use the last cached clause and number to insert */

#if defined(CACHE_INCREMENTAL_CLAUSE_INSERTION)

  /* If this is activated, patching is sped up by caching the last
     insertion peformed, and using the cache not to advance in the
     chain of clauses from the beginning.  Inserting always at the end
     is simply not possible because the clause numbers to be accessed
     do not come ordered. The cache is used only if we are inserting
     farther than the last clause inserted.

     We should check that the clause numbers have not changed ---
     i.e., that intermediate records are not erased --- as that would
     invalidate our count.
  */

  if (t->number >= def->last_inserted_num){/* farther than last insertion */
    cl = def->last_inserted_clause;
    i  = t->number - def->last_inserted_num;
  } else {
    i  = t->number - 1;
    cl = def->clauses.ptr;
  }

  for( ; i; i--)/* Skip until end of chain --- it is not NULL terminated! */
    cl = cl->next.ptr;

  def->last_inserted_clause = cl;
  def->last_inserted_num    = t->number;
#else
  for (i=t->number, cl=def->clauses.ptr; --i;) /* 1-based numbers */
    cl = cl->next.ptr;
#endif

 /* Patch previous emul_p "fail_alt" code */
  t->emul_p = BCoff(cl->emulcode, FTYPE_size(f_i));
  if (first) { /* maintain emul_p2 optimization */
    t->emul_p2 = BCoff(t->emul_p, p2_offset(BCOp(t->emul_p, FTYPE_ctype(f_o), 0)));
  }
}


static void incore_insert(try_node_t **t0,
			  int effar,
			  emul_info_t *ref,
			  incore_info_t *def)
{
  try_node_t **t1 = t0;
  try_node_t *t;

  /* Init the try_node to insert. */
  t = checkalloc_TYPE(try_node_t);
  t->node_offset = ArityToOffset(effar);
  /* Last "next" is num. of clauses: we are inserting at the end of the chain */
  t->number = ref->next.number;
  t->emul_p = BCoff(ref->emulcode, BCOp(ref->emulcode, FTYPE_ctype(f_i), 0)); /* initial p: det case */
#if defined(GAUGE)
  t->entry_counter = ref->counters;
#endif
  t->next = NULL;

  if (ISNOTRY(*t1)) {
    t->emul_p2 = BCoff(t->emul_p, p2_offset(BCOp(t->emul_p, FTYPE_ctype(f_o), 0)));
  } else {
    do {
      if ((*t1)->next==NULL)
        set_nondet(*t1,def,t0==t1);
      t1 = &(*t1)->next;
    } while (!ISNOTRY(*t1));
  }
  (*t1) = t;
}

static try_node_t *incore_copy(try_node_t *from)
{
  try_node_t *tcopy = fail_alt;
  try_node_t **to = &tcopy;

  for (; !ISNOTRY(from); from=from->next) {
    (*to) = checkalloc_TYPE(try_node_t);
    (*to)->node_offset = from->node_offset;
    (*to)->number = from->number;
    (*to)->emul_p = from->emul_p;
    (*to)->emul_p2 = from->emul_p2;
    (*to)->next = NULL;
#if defined(GAUGE)
    (*to)->entry_counter = from->entry_counter;
#endif
    to = &(*to)->next;
  }

  return tcopy;
}

/* get location of try chain for a key */
sw_on_key_node_t *incore_gethash(sw_on_key_t *sw,
				 tagged_t key)
{
  CIAO_REG_2(sw_on_key_node_t *, hnode);
  CIAO_REG_3(intmach_t, i);
  CIAO_REG_4(tagged_t, t0);

  for (i=0, t0=key & sw->mask;
       ;
       i+=sizeof(sw_on_key_node_t), t0=(t0+i) & sw->mask) {
    hnode = SW_ON_KEY_NODE_FROM_OFFSET(sw, t0);
    if (hnode->key==key || !hnode->key)
      return hnode;
  }
}

sw_on_key_t *new_switch_on_key(intmach_t size,
			       try_node_t *otherwise)
{
  intmach_t i;
  sw_on_key_t *sw;

  sw = checkalloc_FLEXIBLE(sw_on_key_t, sw_on_key_node_t, size);

  sw->mask = SizeToMask(size);
  sw->count = 0;
#if defined(ATOMGC)
  sw->next_index = 0;
#endif
  for (i=0; i<size; i++)
    sw->node[i].key = 0,
    sw->node[i].value.try_chain = otherwise;
  return sw;
}


/* I still could not make the "pointer to the last try_node" work with the
has table try nodes; I am passing a NULL pointer which is checked by
incore_insert() and not used.  */

static void incore_puthash(sw_on_key_t **psw,
			   int effar,
			   emul_info_t *current,
			   incore_info_t *def,
			   tagged_t k)
{
  intmach_t i;
  sw_on_key_node_t *h1;
  try_node_t *otherwise = NULL;
  intmach_t size = SwitchSize(*psw);

  if (k==ERRORTAG){		/* add an alt. to default and to every key */
    for (i=0; i<size; i++) {
      h1 = &(*psw)->node[i];
      if (h1->key)
        incore_insert(&h1->value.try_chain,effar,current,def);
      else if (!otherwise){
        incore_insert(&h1->value.try_chain,effar,current,def);
        otherwise = h1->value.try_chain;
      } else
        h1->value.try_chain = otherwise;
    }
  } else {
    h1 = incore_gethash(*psw,k);
    if (!h1->key) {
      h1->key = k;
      h1->value.try_chain = incore_copy(otherwise=h1->value.try_chain);
      incore_insert(&h1->value.try_chain,effar,current,def);
      if (((*psw)->count+=1)<<1 > size)
        expand_sw_on_key(psw,otherwise,TRUE);
    } else incore_insert(&h1->value.try_chain,effar,current,def);
  }
}

static void free_try(try_node_t **t)
{
  try_node_t *t1, *t2;

  for (t1=(*t); !ISNOTRY(t1); t1=t2) {
    t2=t1->next;
    checkdealloc_TYPE(try_node_t, t1);
  }
  (*t)=NULL;
}


static void free_sw_on_key(sw_on_key_t **sw)
{
  sw_on_key_node_t *h1;
  intmach_t i;
  intmach_t size = SwitchSize(*sw);
  bool_t otherwise = FALSE;

  for (i=0; i<size; i++) {
    h1 = &(*sw)->node[i];
    if (h1->key || !otherwise)
      free_try(&h1->value.try_chain);
    if (!h1->key)
      otherwise = TRUE;
  }

  checkdealloc_FLEXIBLE(sw_on_key_t,
			sw_on_key_node_t,
			size,
			*sw);
  (*sw)=NULL;
}

static void free_emulinfo(emul_info_t *cl)
{
  definition_t *def, *sibling;

  for (def=cl->subdefs; def!=NULL; def=sibling) {
    sibling = DEF_SIBLING(def);
    free_info(def->predtyp, (char *)def->code.intinfo);
    checkdealloc_TYPE(definition_t, def);
  }
  checkdealloc_FLEXIBLE_S(emul_info_t, objsize, cl);
}

static void free_incoreinfo(incore_info_t **p)
{
  emul_info_t *stop = (*p)->clauses_tail->ptr;
  emul_info_t *cl, *cl1;

  for (cl=(*p)->clauses.ptr; cl!=stop; cl=cl1) {
    cl1 = cl->next.ptr;
    free_emulinfo(cl);
  }
  checkdealloc_TYPE(incore_info_t, *p);
  (*p) = NULL;
}

/* Those have to do with garbage collecting of the abolished predicates.
   Should be made by only one worker?  Otherwise, access should be locked
   when doing this GC --- which means every predicate access should be
   locked! */

static intmach_t gcdef_count=0;             /* Shared, no locked */
static intmach_t gcdef_limit=0;             /* Shared, no locked */
typedef struct gcdef_ gcdef_t;
struct gcdef_ {                 /* Shared, no locked */
  enter_instr_t enter_instr;
  char *info;
};
static gcdef_t *gcdef_bin;

void leave_to_gc(enter_instr_t type, char *info) {
  intmach_t size;

  if (gcdef_limit==0) {
    size = 2;
    gcdef_limit = 2;
    gcdef_bin = checkalloc_ARRAY(gcdef_t, size);
  } else if (gcdef_count==gcdef_limit) {
    size = gcdef_count;
    gcdef_limit *= 2;
    gcdef_bin = checkrealloc_ARRAY(gcdef_t,
				   size,
				   size*2,
				   gcdef_bin);
  }

  gcdef_bin[gcdef_count].enter_instr = type;
  gcdef_bin[gcdef_count].info = info;
  gcdef_count++;
}

static CVOID__PROTO(make_undefined, definition_t *f)
{
  /*Wait_Acquire_slock(prolog_predicates_l);*/
  leave_to_gc(f->predtyp, (char *)f->code.intinfo);
  if (f->predtyp==ENTER_INTERPRETED) {
    /* erase as much as possible */
    instance_t *i, *j;

    for (i = f->code.intinfo->first; i; i=j) {
      j = i->forward;
      if (i->death==0xffff) {
        i->death = use_clock = def_clock;
        if (i->birth==i->death) { 

/* make_undefined() is called from abolish() and define_predicate(),
   which in turn are called directly from Prolog and do not put any
   lock.  When reloading Prolog code, the existent clauses (including
   those of concurrent predicates) are erased, so we better put a lock
   on those predicates.   MCL.
*/

          Cond_Begin(f->code.intinfo->clause_insertion_cond);
          expunge_instance(i);
          Broadcast_Cond(f->code.intinfo->clause_insertion_cond);
        }
      }
    }

    Cond_Begin(f->code.intinfo->clause_insertion_cond);
    (void)ACTIVE_INSTANCE(Arg,f->code.intinfo->first,use_clock,TRUE);
    Broadcast_Cond(f->code.intinfo->clause_insertion_cond);
  }

  /*f->properties.public = 0;*/
  f->properties.wait = 0;
  f->properties.multifile = 0;
  f->properties.dynamic = 0;
#if defined(THREADS)
  f->properties.concurrent = 0;
#endif
  SetEnterInstr(f,ENTER_UNDEFINED);

  /*Release_slock(prolog_predicates_l);*/
}

/* Really get rid of abolished predicate. */
CBOOL__PROTO(empty_gcdef_bin)
{
  gcdef_t *g;
  intmach_t current_mem = total_mem_count;

  while (gcdef_count>0)  {
    g = &gcdef_bin[--gcdef_count];
    free_info(g->enter_instr, g->info);
  }
  INC_MEM_PROG(total_mem_count - current_mem);

  return TRUE;
}

void relocate_gcdef_clocks(instance_clock_t *clocks)
{
  intmach_t i;

  for (i=0; i<gcdef_count; i++)
    if (gcdef_bin[i].enter_instr==ENTER_INTERPRETED)
      relocate_clocks(((int_info_t *)gcdef_bin[i].info)->first, clocks);
}

static void free_info(enter_instr_t enter_instr, char *info)
{
  switch(enter_instr)
    {
    case ENTER_COMPACTCODE_INDEXED:
    case ENTER_PROFILEDCODE_INDEXED:
      free_try(&((incore_info_t *)info)->lstcase);
      free_sw_on_key(&((incore_info_t *)info)->othercase);
    case ENTER_COMPACTCODE:
    case ENTER_PROFILEDCODE:
      free_try(&((incore_info_t *)info)->varcase);
      free_incoreinfo((incore_info_t **)(&info));
      break;
    case ENTER_INTERPRETED:
      {
	int_info_t *int_info = (int_info_t *)info;
	instance_t *n, *m;
	intmach_t size = SwitchSize(int_info->indexer);

 	for (n = int_info->first; n; n=m) {
          m=n->forward;
          n->rank = ERRORTAG;
          checkdealloc_FLEXIBLE_S(instance_t, objsize, n);
        }
        
	checkdealloc_FLEXIBLE(sw_on_key_t,
			      sw_on_key_node_t,
			      size,
			      int_info->indexer);
        
	checkdealloc_TYPE(int_info_t, info);
	break;
      }
    case TABLE:
      {
	sw_on_key_t *sw = (sw_on_key_t *)info;
	intmach_t size = SwitchSize(sw);
	checkdealloc_FLEXIBLE(sw_on_key_t,
			      sw_on_key_node_t,
			      size,
			      sw);
	break;
      }
    case EMUL_INFO:
      {
	free_emulinfo((emul_info_t *)info);
	break;
      }
    case OTHER_STUFF:
      {
	other_stuff_t *other = (other_stuff_t *)info;
	checkdealloc((tagged_t *)other->pointer, other->size);
	checkdealloc_TYPE(other_stuff_t, info);
	break;
      }
    default:
      break;
    }
}

/* JFMC: abolish/1 predicate calls abolish C function. */
CBOOL__PROTO(prolog_abolish)
{
  tagged_t *junk;
  definition_t *f;

  DEREF(X(0),X(0));
  f = find_definition(predicates_location,X(0),&junk,FALSE);
  return abolish(Arg, f);  
}

/* JFMC: abolish is now a C function instead of a predicate. */
/* Make a predicate undefined.  Also, forget spypoints etc. */
CBOOL__PROTO(abolish, definition_t *f)
{
  intmach_t current_mem = total_mem_count;
  /* MCL: abolish/1 must succeed even in the case of undefined predicates */
  if (!f) return TRUE; 
  if (/*f->predtyp == ENTER_C || */                               /* JFMC */
      f->predtyp > ENTER_INTERPRETED) return FALSE;
  if (f->predtyp != ENTER_UNDEFINED) {
    f->properties.spy = 0;
    f->properties.breakp = 0;
    make_undefined(Arg, f);
    num_of_predicates--;
  }
  INC_MEM_PROG(total_mem_count - current_mem);
  return TRUE;
}

/* Define an interpreted predicate.  It is open iff it is concurrent. */

static void init_interpreted(definition_t *f)
{
  f->code.intinfo = checkalloc_TYPE(int_info_t);

  /* By default, make it DYNAMIC.  
     set_property() may change this behavior later. MCL. */

  f->code.intinfo->behavior_on_failure = DYNAMIC;

  /*f->code.intinfo->clause_insertion_cond = create_dynamic_lock();*/
  Init_Cond(f->code.intinfo->clause_insertion_cond);

  /*  MCL added on 26 Nov 98 */
  f->code.intinfo->x2_pending_on_instance = NULL;
  f->code.intinfo->x5_pending_on_instance = NULL;

  f->code.intinfo->first = NULL;
  f->code.intinfo->varcase = NULL;
  f->code.intinfo->lstcase = NULL;
  f->code.intinfo->indexer = new_switch_on_key(2,NULL);
  SetEnterInstr(f,ENTER_INTERPRETED);
}

CBOOL__PROTO(define_predicate)
{
  definition_t *f;
  enter_instr_t type;
  intmach_t current_mem = total_mem_count;

  DEREF(X(0),X(0));
  if ((f=parse_definition(X(0)))==NULL)
    USAGE_FAULT("$define_predicate: bad 1st arg");

  /*if (f->properties.public) return FALSE;*/

  if (f->properties.multifile){   /* DCG */
    INC_MEM_PROG(total_mem_count - current_mem);
    return TRUE;
  }

  if (f->predtyp!=ENTER_UNDEFINED)
    make_undefined(Arg,f);
  
  num_of_predicates++;      /* Decremented by make_undefined(), if called */

  DEREF(X(1),X(1));
  type = (X(1)==atom_unprofiled ?  ENTER_COMPACTCODE :
    	  X(1)==atom_profiled   ? ENTER_PROFILEDCODE :
          ENTER_INTERPRETED);

  switch (type) {
  case ENTER_INTERPRETED:
    init_interpreted(f);
    break;
  default:
    {
      incore_info_t *d;
      
      d = checkalloc_TYPE(incore_info_t);
      
      d->clauses.ptr = NULL;
      d->clauses_tail = &d->clauses;
      d->varcase = fail_alt;
      d->lstcase = NULL;        /* Used by native preds to hold nc_info */
      d->othercase = NULL; /* Used by native preds to hold index_clause */
#if defined(CACHE_INCREMENTAL_CLAUSE_INSERTION)
      d->last_inserted_clause = NULL;
      d->last_inserted_num = ~0;
#endif
      f->code.incoreinfo = d;
    }
    f->properties.nonvar = 0;
    f->properties.var = 0;
    SetEnterInstr(f,type);
    break;
  }
  INC_MEM_PROG(total_mem_count - current_mem);
  return TRUE;
}

CBOOL__PROTO(erase_clause)
{
  intmach_t current_mem = total_mem_count;

  DEREF(X(0),X(0));
  free_emulinfo(TagToEmul(X(0)));
  INC_MEM_PROG(total_mem_count - current_mem);

  return TRUE;
}

CBOOL__PROTO(clause_number)
{
  definition_t *f;
  uintmach_t number;

  DEREF(X(0),X(0));
  if ((f=parse_definition(X(0)))==NULL)
    USAGE_FAULT("$clause_number: bad 1st arg");

  number = f->code.incoreinfo->clauses_tail->number;

  Unify_constant(MakeSmall(number),X(1));
  return TRUE;
}

CBOOL__PROTO(compiled_clause)
{
  definition_t *f;
  emul_info_t *ref;
  unsigned int type;
  tagged_t t1, key;
  incore_info_t *d;
  unsigned int bitmap;
  emul_info_t *ep, **epp;
  intmach_t current_mem = total_mem_count;

  DEREF(X(0),X(0));		/* Predicate spec */
  if ((f=parse_definition(X(0)))==NULL)
    USAGE_FAULT("$emulated_clause: bad 1st arg");
  DEREF(X(1),X(1));		/* Bytecode object */
  ref = TagToEmul(X(1));
  DEREF(X(2),X(2));		/* Mode */
  DEREF(X(3),X(3));		/* f(Type,Key[,Base,Woff,Roff]) */
  DerefArg(t1,X(3),1);
  type = GetSmall(t1);
  DerefArg(key,X(3),2);
  if (IsVar(key))
    key = ERRORTAG;
  else if (TagIsSTR(key))
    key = TagToHeadfunctor(key);

  				/* add a new clause. */
  d = f->code.incoreinfo;

  ep = (emul_info_t *)d->clauses_tail; /* TODO: (JFMC) assumes that &clauses_tail->next == clauses_tail */
  if (d->clauses.ptr != NULL && IS_CLAUSE_TAIL(ep)) {
    for (epp = &d->clauses.ptr; *epp != ep; epp = (emul_info_t **)*epp)
      ;
    ref->subdefs = ep->subdefs;
    ref->next.ptr = ep->next.ptr;
    *epp = ref;
    checkdealloc_FLEXIBLE(emul_info_t,
			  char,
			  CLAUSE_TAIL_INSNS_SIZE,
			  ep);
  } else {
    ref->next.number = d->clauses_tail->number + 1;
    d->clauses_tail->ptr = ref;
  }
  d->clauses_tail = &ref->next;

  bitmap = 
    (type&0x1 &&  !f->properties.nonvar ? 0x1 : 0) | /* var   */
    (type&0x8 &&  !f->properties.var    ? 0x2 : 0) | /* lst   */
    (type&0x16 && !f->properties.var    ? 0x4 : 0) ; /* other */
  if ((type&0x21) == 0x21)
    f->properties.nonvar = 1;
  if ((type&0x3e) == 0x3e)
    f->properties.var = 1;

  if (!(f->predtyp&1) && bitmap!=0x7) {
    SetEnterInstr(f,f->predtyp+1);
    d->lstcase = incore_copy(d->varcase);
    d->othercase = new_switch_on_key(2,incore_copy(d->varcase));
  }

  if (!(f->predtyp&1))
    incore_insert(&d->varcase,f->arity,ref,d);
  else {
    if (bitmap&0x1)
      incore_insert(&d->varcase,f->arity,ref,d);
    if (bitmap&0x2)
      incore_insert(&d->lstcase,f->arity,ref,d);
    if (bitmap&0x4)
      incore_puthash(&d->othercase,f->arity,ref,d,key);
  }
  INC_MEM_PROG(total_mem_count - current_mem);
  return TRUE;
}

sw_on_key_node_t *dyn_puthash(sw_on_key_t **swp, tagged_t k)
{
  sw_on_key_node_t *h1;

  h1 = incore_gethash(*swp,k);
  if (h1->key)
    return h1;
  else {
    h1->key = k;
    if (((*swp)->count+=1)<<1 <= SwitchSize(*swp))
      return h1;
    else {
      expand_sw_on_key(swp,NULL,TRUE);
      return incore_gethash(*swp,k);
    }
  }
}

CBOOL__PROTO(set_property)
{
  definition_t *f;
  tagged_t *junk;
  enter_instr_t type;

  DEREF(X(0),X(0));
  if (!(f = find_definition(predicates_location,X(0),&junk,FALSE)))
    return FALSE;
  type = f->predtyp;
  if ((type > ENTER_FASTCODE_INDEXED && type != ENTER_INTERPRETED) ||
      (type <= ENTER_FASTCODE_INDEXED && f->code.incoreinfo->clauses.ptr != NULL) ||
      (type == ENTER_INTERPRETED && f->code.intinfo->first))
    return FALSE;

  DEREF(X(1),X(1));
  /*
    if (X(1)==atom_public)
    f->properties.public = 1;
    else
    f->properties.public = 0;
  */
  if (X(1)==atom_wait) {
    f->properties.wait = 1;
    SetEnterInstr(f,type);
  } else if ( (X(1)==atom_dynamic) || (X(1) == atom_concurrent)){  /* MCL */
    f->properties.dynamic = 1;
    f->properties.concurrent = X(1) == atom_concurrent;            /* MCL */

    if (type != ENTER_INTERPRETED) {          /* Change it to interpreted */
      free_incoreinfo(&f->code.incoreinfo);
      init_interpreted(f);
    }

    /* In any case, set the runtime behavior */
    f->code.intinfo->behavior_on_failure =
#if defined(THREADS)
      f->properties.concurrent ? CONC_OPEN : DYNAMIC;
#else
      DYNAMIC;
#endif
  }
  else if (X(1)==atom_multifile)
    f->properties.multifile = 1;

  return TRUE;
}
