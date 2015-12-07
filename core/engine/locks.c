/*
 *  locks.c
 *
 *  Predicates for locks (concurrency).
 *
 *  Copyright (C) 1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 *
 *  Author:
 *    Manuel Carro
 */

#include <ciao/datadefs.h>
#include <ciao/support.h>
#include <ciao/support_macros.h>
#include <ciao/threads.h>
#include <ciao/locks.h>
#include <ciao/alloc.h>

/* '$lock'(<address>,type) <-- LOCK */
CFUN__PROTO(lock_to_term, tagged_t, LOCK *l)
{
  tagged_t *pt1 = w->global_top;

  HeapPush(pt1,functor_Dlock);
  HeapPush(pt1,PointerToTerm(l));
  HeapPush(pt1,MakeInteger(Arg, POSIX));

  w->global_top = pt1;

  return Tag(STR,HeapOffset(pt1,-3));
}

/* '$lock'(<address>,type) <-- SLOCK */
CFUN__PROTO(slock_to_term, tagged_t, SLOCK *s)
{
  tagged_t *pt1 = w->global_top;

  HeapPush(pt1,functor_Dlock);
  HeapPush(pt1,PointerToTerm(s));
  HeapPush(pt1,MakeInteger(Arg, SPIN));

  w->global_top = pt1;

  return Tag(STR,HeapOffset(pt1,-3));
}

/* '$lock'(<address>,type) --> LOCK */
void term_to_lock(tagged_t t, LOCK **l)
{
  tagged_t x1 = (tagged_t)NULL;

  DerefSwitch(t,x1,;);

  if (TagIsSTR(t) && (TagToHeadfunctor(t) == functor_Dlock)) {
    DerefArg(x1,t,1);
    *l = TagToLock(x1);
#if defined(DEBUG)
    tagged_t x2 = (tagged_t)NULL;
    DerefArg(x2,t,2);
    if (GetInteger(x2) != POSIX)
      printf("ERROR in term_to_lock. The lock is not a POSIX-lock");
#endif
  }
}


/* '$lock'(<address>,type) --> SLOCK */
void term_to_slock(tagged_t t, SLOCK **s)
{
  tagged_t x1 = (tagged_t)NULL;

  DerefSwitch(t,x1,;);

  if (TagIsSTR(t) && (TagToHeadfunctor(t) == functor_Dlock)) {
    DerefArg(x1,t,1);
    *s = TagToSLock(x1);
#if defined(DEBUG)
    tagged_t x2 = (tagged_t)NULL;
    DerefArg(x2,t,2);
    if (GetInteger(x2) != SPIN)
      printf("ERROR in term_to_slock. The lock is not a SPIN-lock");
#endif
  }
}


#if defined(HAVE_LIB_LOCKS) && defined(DEBUG)
#if defined(Win32)
bool_t lock_is_unset_win32(LOCK *p)
{
  fprintf(stderr,
          "testing lock unset in Win32: TryEnterCriticalSection may not be supported!\n");
  return FALSE;
}
#else
bool_t lock_is_unset(LOCK *p)
{
  int value;
  if ((value = pthread_mutex_trylock(p)) != EBUSY)
    pthread_mutex_unlock(p);
  return (value != EBUSY);
}
#endif
#endif

#if defined(USE_LOCKS)
#if defined(GENERAL_LOCKS)
 /* Implementation of general locks based on binary ones (Barz, 1983,
    SIGPLAN Notices) */

/* lock_atom/1: puts a lock on X(0), which must be an atom */

CBOOL__PROTO(prolog_lock_atom)
{
  ERR__FUNCTOR("concurrency:lock_atom", 1);
  tagged_t term;
  atom_t *atomptr;

  DEREF(term, X(0));

  if (TagIsATM(term)) {                                    /* Atom -- lock */
    atomptr = TagToAtom(term);
    Wait_Acquire_lock(atomptr->atom_lock_l);
    Wait_Acquire_slock(atomptr->counter_lock);
    atomptr->atom_lock_counter--;
    if (atomptr->atom_lock_counter > 0)
      Release_lock(atomptr->atom_lock_l);
    Release_slock(atomptr->counter_lock);
  } else BUILTIN_ERROR(TYPE_ERROR(STRICT_ATOM),X(0),1);

  return TRUE;
}

CBOOL__PROTO(prolog_unlock_atom)
{
  ERR__FUNCTOR("concurrency:unlock_atom", 1);
  tagged_t term;
  atom_t *atomptr;

  DEREF(term, X(0));

  if (TagIsATM(term)) {
    atomptr = TagToAtom(term);
    Wait_Acquire_slock(atomptr->counter_lock);
    atomptr->atom_lock_counter++;
    if (atomptr->atom_lock_counter == 1)
      Release_lock(atomptr->atom_lock_l);
    Release_slock(atomptr->counter_lock);
  } else BUILTIN_ERROR(TYPE_ERROR(STRICT_ATOM),X(0),1);

  return TRUE;
}

CBOOL__PROTO(prolog_lock_atom_state)
{
  ERR__FUNCTOR("concurrency:atom_lock_state", 2);
  tagged_t term, value;
  atom_t *atomptr;
  int          lock_value;

  DEREF(term, X(0));

  if (TagIsATM(term)) {
    atomptr = TagToAtom(term);
    DEREF(value, X(1));
    if (TagIsSmall(value)) {
      Wait_Acquire_slock(atomptr->counter_lock);
      atomptr->atom_lock_counter = GetSmall(value);
      Release_slock(atomptr->counter_lock);
      return TRUE;
    }
    else if (IsVar(value)) {
      Wait_Acquire_slock(atomptr->counter_lock);
      lock_value = atomptr->atom_lock_counter;
      Release_slock(atomptr->counter_lock);
      return cunify(Arg, X(1), MakeSmall(lock_value));
    }
    else BUILTIN_ERROR(UNINSTANTIATION_ERROR,X(1),2);
  } else BUILTIN_ERROR(TYPE_ERROR(STRICT_ATOM),X(0),1);
}

#else                                                    /* GENERAL_LOCKS */

/* lock_atom/1: puts a lock on X(0), which must be an atom */

CBOOL__PROTO(prolog_lock_atom_bin)
{
  ERR__FUNCTOR("concurrency:lock_atom", 1);
  tagged_t term;
  atom_t *atomptr;

  DEREF(term, X(0));

  if (TagIsATM(term)) {                                    /* Atom -- lock */
    atomptr = TagToAtom(term);
    Wait_Acquire_lock(atomptr->atom_lock_l);
  } else BUILTIN_ERROR(TYPE_ERROR(ATOM),X(0),1);

  return TRUE;
}

CBOOL__PROTO(prolog_unlock_atom_bin)
{
  ERR__FUNCTOR("concurrency:unlock_atom", 1);
  tagged_t term;
  atom_t *atomptr;

  DEREF(term, X(0));

  if (TagIsATM(term)) {
    atomptr = TagToAtom(term);
    Release_lock(atomptr->atom_lock_l);
  } else BUILTIN_ERROR(TYPE_ERROR(ATOM),X(0),1);

  return TRUE;
}

#endif                                                   /* GENERAL_LOCKS */

 /* Releases the lock on a predicate; this is needed to ensure that a clause
    will not be changed while it is being executed. */

CBOOL__PROTO(prolog_unlock_predicate)
{
  int_info_t *root = TagToRoot(X(0));

#if defined(DEBUG)
  if (debug_conc) {
    fprintf(stderr, "*** %d(%d) unlocking predicate, root is %x\n",
            (int)Thread_Id, (int)GET_INC_COUNTER, (unsigned int)root);

    if (root->behavior_on_failure == CONC_OPEN &&
        Cond_Lock_is_unset(root->clause_insertion_cond))
      fprintf(stderr,
      "WARNING: In unlock_predicate, root is %x, predicate is unlocked!!!!\n", 
              (unsigned int)root);
  }
#endif
  
  /* We have just finished executing an operation on a locked predicate;
     unlock the predicate and make sure the choicepoint is not marked as
     executing. */

  if (root->behavior_on_failure != DYNAMIC){
    SET_NONEXECUTING((TopConcChpt->term[InvocationAttr])); 
    Wait_For_Cond_End(root->clause_insertion_cond);
  }

  return TRUE;
}

#else                                                        /* !USE_LOCKS */

/* lock_atom/1: puts a lock on X(0), which must be an atom */

CBOOL__PROTO(prolog_lock_atom)
{
  return TRUE;
}
CBOOL__PROTO(prolog_lock_atom_state)
{
  return TRUE;
}
CBOOL__PROTO(prolog_unlock_atom)
{
  return TRUE;
}
/*
void init_dynamic_locks(void) {}
LOCK create_dynamic_lock(void){return NULL;}
*/
CBOOL__PROTO(prolog_unlock_predicate)
{
#if defined(DEBUG)
  if (debug_conc) fprintf(stderr, "Using fake unlock_predicate!!!!\n");
#endif
  return TRUE;
}

#endif

/* ------------------------------------------------------------------------- */

#if defined(DEBUG)

/* Counts mutually exclusive operations */
uintmach_t ops_counter = 0;
SLOCK ops_counter_l;

uintmach_t get_inc_counter(void)
{
  uintmach_t local_counter;
  Wait_Acquire_slock(ops_counter_l);
  local_counter = ops_counter++;
  Release_slock(ops_counter_l);
  return local_counter;
}

void reset_counter(void)
{
  Wait_Acquire_slock(ops_counter_l);
  ops_counter = 0;
  Release_slock(ops_counter_l);
}

#endif
