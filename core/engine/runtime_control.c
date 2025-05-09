/*
 *  runtime_control.c
 *
 *  Copyright (C) 1996-2002 UPM-CLIP
 *  Copyright (C) 2020-2024 The Ciao Development Team
 */

#include <ciao/eng.h>
#include <ciao/eng_registry.h>
#include <ciao/internals.h>
#include <ciao/dynamic_rt.h>
#include <ciao/runtime_control.h>
#include <ciao/eng_gc.h>
#include <ciao/eng_profile.h>
#include <ciao/basiccontrol.h>

#include <ciao/timing.h>
#include <ciao/io_basic.h>

#include <string.h>

/* ------------------------------------------------------------------
   THE BUILTIN C-PREDICATE       CURRENT_ATOM/1
   -----------------------------------------------------------------------*/

CBOOL__PROTO(current_atom) {
  DEREF(X(0),X(0));
  if (TaggedIsATM(X(0)))
    return TRUE;
  if (! IsVar(X(0)))
    MINOR_FAULT("current_atom/1: incorrect 1st arg");
#if defined(ATOMGC)
  { 
    /* There is an (improbable) case: the 0-th table entry is empty.
       Take it into account. */
    intmach_t size = HASHTAB_SIZE(ciao_atoms) >> 1;
    intmach_t i = 0;
    while (i < size && (atmtab[i] == NULL))
      i++;
    X(1) = MakeSmall(i);  /* Yes, I am not considering an empty symbol table */
  }
#else
  X(1) = TaggedZero;
#endif
  push_choicept(Arg,address_nd_current_atom);
  return nd_current_atom(Arg);
}

CBOOL__PROTO(nd_current_atom) {
  intmach_t i = GetSmall(X(1));

#if defined(ATOMGC)
 /* 

    Atom GC may leave holes in the atom table.  Therefore: 
    
    1- The "following" valid entry is not necessarily the next one; 
    we may have to skip a number of empty atom entries.

    2- Stopping when the number of indices read reaches the current number of
    atoms is not right.  We have to use instead the size of the table. 

  */

  intmach_t size = HASHTAB_SIZE(ciao_atoms) >> 1;

  /* Invariant: at entry, the current i points to a nonempty atom */
  CBOOL__UnifyCons(TagIndex(ATM,i),X(0));
  /* Go forward until the next non-empty atom; final stop when the end of
     the table has been reached.  */
  i++;
  while(i < size && (atmtab[i] == NULL))
    i++;
  
  if (i < size)                                  /* We got the next index */
    w->choice->x[1] = MakeSmall(i);
  else 
    pop_choicept(Arg);
#else
  w->choice->x[1] += MakeSmallDiff(1);
  CBOOL__UnifyCons(TagIndex(ATM,i),X(0));
    
  if (i+1 == ciao_atoms->count)
    pop_choicept(Arg);
#endif

  return TRUE;
}

/* --------------------------------------------------------------------------- */

/* TODO: alternatively: use the mem address, if it is stable (JF) */

/* Support for generating new atoms with "funny names", always different.
   Make sure that the generation works OK with concurrency.  */

/* This seems to be the right size: one character less, and time (at large)
   doubles; one character more, and comparison in the symbol table takes
   longer. */
#define NEW_ATOM_LEN 13
#define NUM_OF_CHARS 62
static char allowed_char_table[NUM_OF_CHARS + 1] =
"0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
static char new_atom_str[] = "!!!!!!!!!!!!!";
#define FIRST_CHAR 0
#define LAST_CHAR  (NUM_OF_CHARS-1)

static uintmach_t x = 13*17;

CBOOL__PROTO(prolog_new_atom) {
  ERR__FUNCTOR("runtime_control:new_atom", 1);
  intmach_t i;
  intmach_t previous_atoms_count;
  tagged_t new_atom;

  DEREF(X(0), X(0));
  if (!IsVar(X(0))) {
    BUILTIN_ERROR(ERR_uninstantiation_error,X(0),1);
  }

  Wait_Acquire_slock(atom_id_l);

  previous_atoms_count = ciao_atoms->count;
  do {
    for (i = 0; i < NEW_ATOM_LEN; i++) {
      x = (((new_atom_str[i] + x - FIRST_CHAR) * 13) + 300031);
      new_atom_str[i] = allowed_char_table[(x % NUM_OF_CHARS) + FIRST_CHAR];
      x = x / NUM_OF_CHARS;
    }
    new_atom = GET_ATOM(new_atom_str);
    /* Make sure no smart guy already inserted the atom we have in mind */
  } while(ciao_atoms->count == previous_atoms_count);

  Release_slock(atom_id_l);
  CBOOL__LASTUNIFY(X(0), new_atom);
}

/* ------------------------------------------------------------------------- */

CBOOL__PROTO(statistics) {
  stream_node_t *s = Output_Stream_Ptr;
  intmach_t used, free;
  frame_t *newa;

#if !defined(EMSCRIPTEN)
  inttime_t runtick0;
  inttime_t usertick0 = usertick();
  inttime_t systemtick0 = systemtick();
  runtick0=usertick0;
#endif
  inttime_t walltick0 = walltick();

  StreamPrintf(s,
             "memory used (total)    %10" PRIdm " bytes\n",
             total_mem_count);
  StreamPrintf(s, 
             "   program space (including reserved for atoms): %" PRIdm " bytes\n", 
             mem_prog_count);

  StreamPrintf(s,
             "   number of atoms and functor/predicate names: %" PRIdm "\n", 
             ciao_atoms->count);
  StreamPrintf(s,
             "   number of predicate definitions: %" PRIdm "\n", 
             num_of_predicates);

  used = HeapCharDifference(Heap_Start,w->heap_top);
  free = HeapCharDifference(w->heap_top,Heap_End);
  StreamPrintf(s, 
             "   global stack   %10" PRIdm " bytes:%" PRIdm " in use,%10" PRIdm " free\n",
             used+free, used, free);

  GetFrameTop(newa,w->choice,G->frame);
  used = StackCharDifference(Stack_Start,newa);
  free = StackCharDifference(newa,Stack_End);
  StreamPrintf(s,
             "   local stack    %10" PRIdm " bytes:%10" PRIdm " in use,%10" PRIdm " free\n",
             used+free, used, free);

  used = TrailCharDifference(Trail_Start,w->trail_top);
  free = TrailCharDifference(w->trail_top,w->choice)/2;
  StreamPrintf(s,
             "   trail stack    %10" PRIdm " bytes:%10" PRIdm " in use,%10" PRIdm " free\n",
             used+free, used, free);

  used = ChoiceCharDifference(Choice_Start,w->choice);
  free = ChoiceCharDifference(w->choice,w->trail_top)/2;
  StreamPrintf(s,
             "   control stack  %10" PRIdm " bytes:%10" PRIdm " in use,%10" PRIdm " free\n\n",
             used+free, used, free);

  StreamPrintf(s,
             " %10.6f sec. for %" PRIdm " global, %" PRIdm " local, and %" PRIdm " control space overflows\n",
             ((flt64_t)ciao_stats.ss_tick)/RunClockFreq(ciao_stats),
             ciao_stats.ss_global,
             ciao_stats.ss_local, ciao_stats.ss_control);
  StreamPrintf(s,
             " %10.6f sec. for %" PRIdm " garbage collections which collected %" PRIdm " bytes\n\n",
             ((flt64_t)ciao_stats.gc_tick)/RunClockFreq(ciao_stats),
             ciao_stats.gc_count,
             (intmach_t)ciao_stats.gc_acc);

#if !defined(EMSCRIPTEN) /* not supported by emscripten */
  StreamPrintf(s,
             " runtime:    %10.6f sec. %12" PRId64 " ticks at %12" PRId64 " Hz\n",
             (flt64_t)(runtick0-ciao_stats.starttick)/RunClockFreq(ciao_stats),
             runtick0-ciao_stats.starttick,
             RunClockFreq(ciao_stats));
  StreamPrintf(s,
             " usertime:   %10.6f sec. %12" PRId64 " ticks at %12" PRId64 " Hz\n",
             (flt64_t)(usertick0-ciao_stats.startusertick)/ciao_stats.userclockfreq,
             usertick0-ciao_stats.startusertick,
             ciao_stats.userclockfreq);
  StreamPrintf(s,
             " systemtime: %10.6f sec. %12" PRId64 " ticks at %12" PRId64 " Hz\n",
             (flt64_t)(systemtick0-ciao_stats.startsystemtick)/ciao_stats.systemclockfreq,
             systemtick0-ciao_stats.startsystemtick,
             ciao_stats.systemclockfreq);
#endif
  StreamPrintf(s,
             " walltime:   %10.6f sec. %12" PRId64 " ticks at %12" PRId64 " Hz\n\n",
             (flt64_t)(walltick0-ciao_stats.startwalltick)/ciao_stats.wallclockfreq,
             walltick0-ciao_stats.startwalltick,
             ciao_stats.wallclockfreq);

  //#define DESCRIBE_TAGS 1 /* debug, only for 64-bits */
#if defined(DESCRIBE_TAGS)
  // macos: sysctl machdep.virtual_address_size
  //        (machdep.virtual_address_size: 47)
  // linux: grep -i 'address sizes' /proc/cpuinfo
  //        address sizes	: 40 bits physical, 48 bits virtual
  StreamPrintf(s, "VirtualAddressMask: 0x%016llx\n", ((uintmach_t)1<<48)-1);
  StreamPrintf(s, "SMALLPTR_BASE: 0x%016llx\n", (uintmach_t)SMALLPTR_BASE);
  //
  StreamPrintf(s, "GC_ANYMASK: 0x%016llx\n", GC_ANYMASK); // TODO: needed for (all) non-pointers?
  StreamPrintf(s, "TAGMASK: 0x%016llx\n", TAGMASK);
  StreamPrintf(s, "QTAGMASK: 0x%016llx\n", QTAGMASK);
  StreamPrintf(s, " (HVA=0x%016llx)\n", Tagn(HVA,0));
  StreamPrintf(s, " (CVA=0x%016llx)\n", Tagn(CVA,0));
  StreamPrintf(s, " (SVA=0x%016llx)\n", Tagn(SVA,0));
  StreamPrintf(s, " (UBV=0x%016llx)\n", Tagn(UBV,0));
  StreamPrintf(s, " (LST=0x%016llx)\n", Tagn(LST,0));
  StreamPrintf(s, " (STR=0x%016llx)\n", Tagn(STR,0));
  StreamPrintf(s, "  POINTERMASK: 0x%016llx\n", POINTERMASK);
  StreamPrintf(s, " (ATM=0x%016llx)\n", Tagn(ATM,0));
  StreamPrintf(s, "  ArityMask: 0x%016llx\n", MakeMask(uintmach_t, ARITYSIZE, ARITYOFFSET));
  StreamPrintf(s, "  IndexMask: 0x%016llx\n", MakeMask(uintmach_t, ARITYOFFSET-tagged__atm_offset, tagged__atm_offset));
  StreamPrintf(s, "  (MAXARITY=%lld)\n", (uintmach_t)MAXARITY1 - 1);
  StreamPrintf(s, "  (MAXPROCARITY=%lld)\n", (uintmach_t)MAXPROCARITY1 - 1);
  StreamPrintf(s, "  (MaxAtomCount=%lld)\n", (uintmach_t)MaxAtomCount);
  StreamPrintf(s, " (NUM=0x%016llx)\n", Tagn(NUM,0));
  StreamPrintf(s, "  NumMask: 0x%016llx\n", MakeMask(uintmach_t, tagged__num_size, tagged__num_offset));
  StreamPrintf(s, "Max structure size: %.6f MB\n", (double)MAXARITY1*sizeof(tagged_t)/(1024*1024));
  StreamPrintf(s, "Min atom size: 0x%llx bytes\n", (uintmach_t)sizeof(atom_t));
  StreamPrintf(s, "Max alloc of min atoms: %.6f MB\n", (double)sizeof(atom_t)*MaxAtomCount/(1024*1024));
#endif

  return TRUE;
}
