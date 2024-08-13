/*
 *  eng_debug.c
 *
 *  Support for debugging and tracing the engine code
 *
 *  Copyright (C) 1996-2002 UPM-CLIP
 *  Copyright (C) 2002-2024 The Ciao Development Team
 */

#include <ciao/eng.h>
#include <ciao/eng_debug.h> /* already in eng.h */

#if defined(OPTIM_COMP)
#if defined(USE_LOWRTCHECKS)
#include <ciao/io_basic.h> /* DerefDisplayTerm */
#endif
#else
#include <ciao/io_basic.h>
#include <ciao/stream_basic.h>
#endif

#include <string.h>

#if defined(USE_TRACE_OUTPUT)
#include <ciao/os_defs.h> /* MAXPATHLEN */
#endif

/* TODO:[oc-merge] document all the tracing and debugging options
   (inscount intervals, etc.) */

/* TODO:[oc] backport

   When DEBUG compilation flag is enabled, you can activate the 
   memory checks at some program points using INSCOUNT_FROM,
   INSCOUNT_TO and INSCOUNT_STEP environment variables.
*/

#if defined(DEBUG_TRACE)
bool_t debug_predtrace = FALSE; /* trace predicate calls -- Shared */
#if !defined(OPTIM_COMP)
bool_t debug_instrace = FALSE; /* trace instructions -- Shared */
#endif
bool_t debug_gc = FALSE; /* debug garbage collection -- Shared */
bool_t debug_threads = FALSE; /* debug thread creation -- Shared */
bool_t debug_choicepoints = FALSE; /* debug choicepoints state -- Shared */
bool_t debug_concchoicepoints = FALSE; /* debug conc. chpt. state -- Shared */
bool_t debug_mem = FALSE; /* debug memory manegement -- Shared */
bool_t debug_conc = FALSE; /* debug concurrency -- Shared */
#if defined(OPTIM_COMP)
bool_t debug_setarg = FALSE; /* debug setarg -- Shared */
bool_t debug_atomgc = FALSE; /* debug atomgc -- Shared */
#endif
bool_t debug_dynlink = FALSE; /* debugging C linking  -- Shared */
#endif

#if defined(DEBUG_TRACE)
/* TODO: use an array of opts */
bool_t debug_trace__get_opt(const char *arg) {
  if (strcmp(arg, "--debug-trace-calls") == 0) { /* trace predicate calls */
    debug_predtrace = TRUE;
#if !defined(OPTIM_COMP)
  } else if (strcmp(arg, "--debug-trace-instr") == 0) { /* trace instructions */
    debug_instrace = TRUE;
#endif
  } else if (strcmp(arg, "--debug-chpt") == 0) { /* debug regular choicepoints */
    debug_choicepoints = TRUE;
  } else if (strcmp(arg, "--debug-conc-chpt") == 0) { /* debug concurrent choicepoints */
    debug_concchoicepoints = TRUE;
  } else if (strcmp(arg, "--debug-threads") == 0) { /* debug threads */
    debug_threads = TRUE;
  } else if (strcmp(arg, "--debug-gc") == 0) { /* debug garbage collection */
    debug_gc = TRUE;
  } else if (strcmp(arg, "--debug-mem") == 0) { /* debug memory management */
    debug_mem = TRUE;
  } else if (strcmp(arg, "--debug-conc") == 0) { /* debug concurrency */
    debug_conc = TRUE;
#if defined(OPTIM_COMP)
  } else if (strcmp(arg, "--debug-setarg") == 0) { /* debug setarg */
    debug_setarg = TRUE;
  } else if (strcmp(arg, "--debug-atomgc") == 0) { /* debug atomgc */
    debug_atomgc = TRUE;
#endif
  } else if (strcmp(arg, "--debug-dynlink") == 0) { /* debug dynlink */
    debug_dynlink = TRUE;
  } else {
    return FALSE;
  }
  return TRUE;
}
#endif

/* ------------------------------------------------------------------------- */

#if defined(USE_TRACE_OUTPUT) /* currently only if defined(OPTIM_COMP) */ 
#define TRACE_TO_FILE 1

stream_node_t *stream_trace; /* Shared */

void init_stream_trace(void) {
  FILE *stream_trace_file;
#if defined(TRACE_TO_FILE)
  char trace_filename[MAXPATHLEN]; /* problem with buffer overflow */
  strcpy(trace_filename, getenv("CIAOCACHE"));
  strcat(trace_filename, "/tmp/ciao__trace.txt");
  stream_trace_file = fopen(trace_filename, "w");
  if (stream_trace_file == NULL) {
    fprintf(stderr, "{error: cannot open trace file %s}\n", trace_filename);
    abort();
  }
#else
  stream_trace_file = stderr;
#endif
  stream_trace = new_stream(ERRORTAG, "a", stream_trace_file);
}

void finish_stream_trace(void) {
  if (TraceFile != stderr) fclose(TraceFile);
}
#endif

/* ------------------------------------------------------------------------- */

/* TODO: not used */
CBOOL__PROTO(set_trace_calls) {
  tagged_t x;
  DEREF(x,X(0));
  if (!TaggedIsSmall(x)) return FALSE;
#if defined(DEBUG_TRACE)
  debug_predtrace = (bool_t)GetSmall(x);
#endif
  CBOOL__PROCEED;
}

/* ------------------------------------------------------------------------- */
/* Condition for tracing (intervals) */

#if defined(OPTIM_COMP) && defined(USE_DEBUG_INSCOUNT)
intmach_t around(intmach_t x, intmach_t base, intmach_t radius) {
  if (((base - radius) <= x) && (x <= (base + radius))) return 1;
  return 0;
}

intmach_t debug_inscount_from = 0;
intmach_t debug_inscount_to = 0;
intmach_t debug_inscount_step = 0;

intmach_t debug_inscount = 0;

bool_t dump_cond(void) {
  bool_t c;
  c = FALSE;
  if (debug_inscount <= debug_inscount_to &&
      debug_inscount >= debug_inscount_from &&
      ((debug_inscount - debug_inscount_from) % debug_inscount_step) == 0) {
    c |= TRUE;
  }
  return c;
}

void init_debug_inscount(void) {
  debug_inscount_from = eng_cfg_getenv("INSCOUNT_FROM", -1);
  debug_inscount_to = eng_cfg_getenv("INSCOUNT_TO", -1);
  debug_inscount_step = eng_cfg_getenv("INSCOUNT_STEP", 0);
}
#endif

/* ------------------------------------------------------------------------- */
/* State consistency checks */

#if defined(OPTIM_COMP) && defined(USE_LOWRTCHECKS)

#define BADWORD_GCTAGS 1
#define BADWORD_OUTHEAP 2
#define BADWORD_OUTSTACK 4
#define BADWORD_ANY (BADWORD_OUTSTACK|BADWORD_OUTHEAP|BADWORD_GCTAGS)

#define BADWORD_SENSIBLE(MASK, WHAT) (((MASK)&(WHAT)) != 0)

CFUN__PROTO(badword, char *, tagged_t t, char *local_top, intmach_t mask) {
  char *reason;
  tagged_t *ptr;
  ptr = TaggedToPointer(t);
  if (BADWORD_SENSIBLE(mask, BADWORD_GCTAGS) && (t & GC_ANYMASK)) {
    reason = "gctags";
  } else if (BADWORD_SENSIBLE(mask, BADWORD_OUTHEAP) && (OnHeap(ptr) && (TaggedIsNUMorATM(t) || ptr > G->heap_top))) {
    reason = "outheap";
  } else if (BADWORD_SENSIBLE(mask, BADWORD_OUTSTACK) && (OnStack(ptr) && (TaggedIsNUMorATM(t) || ptr > (tagged_t *)local_top))) {
    reason = "outstack";
  } else {
    reason = NULL;
  }
  CFUN__PROCEED(reason);
}

void dump_tagged(tagged_t t) {
  TRACE_PRINTF("tag%d(0x%x)[%d]", TagOf(t), (unsigned int)TaggedToPointer(t), (int)(t & GC_ANYMASK));
}

intmach_t dump_maxwarn = 10;
intmach_t dump_warncount = 10;

CFUN__PROTO(proofread_warn, intmach_t, char *text, tagged_t *p, char *local_top, intmach_t mask, bool_t quiet) {
  tagged_t t, t2;
  char *reason;
  reason = CFUN__EVAL(badword, *p, local_top, mask);
  if (reason == NULL) CFUN__PROCEED(0);
  t = *p;
  
  if (!quiet) {
    if (dump_warncount < dump_maxwarn) {
      TRACE_PRINTF("* %s (%s): *(%p) = ", text, reason, p);
      dump_tagged(t);
      if (!TaggedIsNUMorATM(t)) {
        TRACE_PRINTF(" deref1:");
        t2 = *TaggedToPointer(t);
        dump_tagged(t2);
      }
      TRACE_PRINTF("\n");
      dump_warncount++;
    } else if (dump_warncount == dump_maxwarn) {
      TRACE_PRINTF("{proofread: too much warnings}\n");
      dump_warncount++;
    }
  }
  CFUN__PROCEED(1);
}

CFUN__PROTO(TESTINT, intmach_t, tagged_t *p, intmach_t mask, bool_t quiet) {
  intmach_t bad = 0;
  tagged_t t1;
  t1 = *p;
  if (IsHeapPtr(t1)) {
    bad |= CFUN__EVAL(proofread_warn, "h", p, (char *)G->local_top, mask, quiet);
  } else if (TaggedIsSVA(t1)) {
    bad |= CFUN__EVAL(proofread_warn, "s", p, (char *)G->local_top, mask, quiet);
  }
  CFUN__PROCEED(bad);
}

CFUN__PROTO(worker_integrity, intmach_t, arity_t arity, bool_t quiet) {
  frame_t *a;
  intmach_t bad = 0;
  tagged_t t1;
  tagged_t *pt1;
  choice_t *n, *n2;
  choice_t *aux_choice;
  frame_t *frame;
  char *old_local_top = 0;
  intmach_t i;
  extern bcp_t contcode;
  extern try_node_t *fail_alt;
  
  old_local_top = (char *)G->local_top;

  /* ensure that w->choice is fleshed out fully i.e. do a "neck" */
  /* arity of choicept could be greater than arity of clause */
  /* DO NOT modify Deep state -- we are still in "shallow mode" */
  /* Pre: !IsDeep() */
  CODE_MAYBE_NECK_TRY();
  
  /* Make a fake frame to store X regs */
  CODE_ALLOC(a);
  a->x[0] = TaggedZero;
  for (i=0; i<arity; i++)
    a->x[i+1] = X(i);
  CODE_CFRAME(a, CONTCODE(arity+1));

  if (!quiet) {
    TRACE_INSCOUNT(); TRACE_PRINTF("{proofread: checking heap[start:%p, top:%p, end:%p, size:%d, maxsize:%ld]}\n", w->heap_start, G->heap_top, w->heap_end, w->heap_end - w->heap_start, SMALLPTR_MASK);
  }
  dump_warncount = 0;
          
  /* Test heap */
  pt1 = Heap_Start;
  while (HeapYounger(G->heap_top,pt1)) {
    t1 = *pt1;
    pt1++;
    /* TODO: check structure!! it would be a good idea to do this
       check only for functor heads */
    if (BlobHF(t1)) {
      pt1 = HeapCharOffset(pt1, BlobFunctorSizeAligned(t1)+sizeof(functor_t));
    } else {
      bad |= CFUN__EVAL(TESTINT, pt1-1, BADWORD_ANY, quiet);
    }
  }

  /* Test the trail stack */
  if (!quiet) {
    TRACE_INSCOUNT(); TRACE_PRINTF("{proofread: checking trail[start:%p, top:%p]}\n", w->trail_start, G->trail_top);
  }
  dump_warncount = 0;

  pt1 = Trail_Start;
  while (TrailYounger(G->trail_top,pt1)) {
    t1 = *pt1;
    /* TODO: move this definition to a macro? */
    if (!IsHeapPtr(t1) && !TaggedIsSVA(t1)) {
      if (!quiet) {
        TRACE_INSCOUNT(); TRACE_PRINTF("{proofread: non heap term nor stack var in trail: %p (0x%x)}\n", pt1, (unsigned int)(t1));
      }
      bad |= TRUE;
    }
    /* during this test OUTSTACK in trail is not bad */
    bad |= CFUN__EVAL(TESTINT, pt1, BADWORD_OUTHEAP|BADWORD_GCTAGS, quiet);
    pt1 += TrailDir;
  }

  /* Test the root of logical global variables */
  bad |= CFUN__EVAL(TESTINT, &GLOBAL_VARS_ROOT, BADWORD_OUTHEAP|BADWORD_GCTAGS, quiet);

  /* Test the choice stack */
  if (!quiet) {
    TRACE_INSCOUNT(); TRACE_PRINTF("{proofread: checking choice[choice:%p, start:%p]}\n", w->choice, w->choice_start);
    TRACE_INSCOUNT(); TRACE_PRINTF("{proofread: checking stack[start:%p, top:%p, end:%p, size:%d, maxsize:%ld]}\n", w->stack_start, G->local_top, w->stack_end, w->stack_end - w->stack_start, SMALLPTR_MASK);
  }
  dump_warncount = 0;
  /* AA, HH and TR are free pointers;  BB is last used word. */
  aux_choice = ChoiceNext0(w->choice,0);
  aux_choice->next_alt = fail_alt;
  aux_choice->frame = G->frame;
  aux_choice->next_insn = G->next_insn;
  aux_choice->heap_top = G->heap_top;
  aux_choice->local_top = G->local_top; // JFKKNEW
  /* pointers in choice&env stks */
  for (n=aux_choice; n!=InitialChoice; n=n2) {
    if (n->next_alt == NULL) {
      /* TODO: hmmm... */
      if (!quiet) {
        TRACE_INSCOUNT(); TRACE_PRINTF("{proofread: error: next_alt = NULL, exiting integrity check}\n");
      }
      bad |= 1;
      goto end;
    }
    ChoiceForeachX(n, i, {
      bad |= CFUN__EVAL(TESTINT, &n->x[i], BADWORD_ANY, quiet);
    });

    /* TODO: write some test to check if next_insn is correct
    if ((unsigned)n->next_insn > (unsigned)0xc0000000) {
      if (!quiet) {
        TRACE_PRINTF("{strange next_insn %p... exiting integrity check}\n", n->next_insn);
      }
      bad |= 1;
      goto end;
    }
    */
    i = FrameSize(n->next_insn);
  
    frame = n->frame;
    if (frame == NULL) {
      if (!quiet) {
        TRACE_INSCOUNT(); TRACE_PRINTF("{proofread: error: frame == NULL, exiting integrity check}\n");
      }
      bad |= 1;
      goto end;
    }
    /* TODO: use the same macros to traverse the frame than in eng_gc */
    n2=ChoiceCont(n);
    while (frame >= n2->local_top) {
      pt1 = (tagged_t *)StackCharOffset(frame,i);
      while (pt1!=frame->x) {
        t1 = *(--pt1);
        bad |= CFUN__EVAL(TESTINT, pt1, BADWORD_ANY, quiet);
      }
      /* TODO: write some test to check if next_insn is correct
      if ((unsigned)frame->next_insn > (unsigned)0xc0000000) {
        if (!quiet) {
          TRACE_PRINTF("{strange next_insn %p... exiting integrity check}\n", frame->next_insn);
        }
        bad |= 1;
        goto end;
      }
      */
      if (frame == NULL) {
        if (!quiet) {
          TRACE_INSCOUNT(); TRACE_PRINTF("{proofread: error: frame == NULL, exiting integrity check}\n");
        }
        bad |= 1;
        goto end;
      }
      if (frame->next_insn == NULL) {
        if (!quiet) {
          TRACE_INSCOUNT(); TRACE_PRINTF("{proofread: error: frame->next_insn == NULL... exiting integrity check; frame=%p}\n", frame);
        }
        bad |= 1;
        goto end;
      }
      i = FrameSize(frame->next_insn);
      frame = frame->frame;
    } 
  }

 end:
  /* Destroy fake frame */

  /* TODO: this is not necessary since X regs has not been
     modified... right? */ 
  for (i=0; i<arity; i++)
    X(i) = a->x[i+1];

  DEALLOCATE(a);
  G->local_top = (frame_t *)old_local_top;

  /* Return result of integrity check */
  CFUN__PROCEED(bad);
}

#include <string.h>

char *proofread__from = NULL;
void proofread__showfrom(void) {
  if (!proofread__from) return;
  TRACE_INSCOUNT(); TRACE_PRINTF("{proofread: from '%s'}\n", proofread__from);
  proofread__from = NULL;
}

CBOOL__PROTO(proofread, char *text, arity_t arity, bool_t force) {
  intmach_t trail_free;

  INSCOUNT_NEXT();
  proofread__from = text;

  /* TODO: recover this integrity test? (and make it general) */
  /* fast integrity test */
  //  if ((unsigned)G->heap_top >= (unsigned)0x4fffffff) {
  //    TRACE_PRINTF("{no more addressable mem}\n");
  //    abort();
  //  }

  /* TODO: write a macro to test any stack */
  trail_free = (char *)w->choice - (char *)G->trail_top;
  if (trail_free < 0) {
    proofread__showfrom();
    TRACE_INSCOUNT(); TRACE_PRINTF("{proofread: error: trail/choice space is negative!!: choice: %p trail_top: %p (available: 0x%x)}\n", w->choice, G->trail_top, trail_free);
  }
#if defined(DEBUG__TRACE__LOW_STACKS)
  else if (trail_free < CHOICEPAD) {
    proofread__showfrom();
    TRACE_INSCOUNT(); TRACE_PRINTF("{proofread: warning: trail/choice space is near end: choice: %p trail_top: %p (available: 0x%x)}\n", w->choice, G->trail_top, trail_free);
  }
#endif

  if (!force) {
    if (!dump_cond()) CBOOL__FAIL;
  }

  /* slow and expensive integrity test */
  if (CFUN__EVAL(worker_integrity, arity, TRUE)) {
    /* the quiet call found errors, do it again verbosely */
    proofread__showfrom();
    CBOOL__LASTTEST(CFUN__EVAL(worker_integrity, arity, FALSE) ? TRUE : FALSE);
  }
  CBOOL__FAIL;
}

/* TODO: print arguments up to a determined depth */
CVOID__PROTO(dump_call, char *s, definition_t *func) {
  int i;

  RTCHECK(CBOOL__SUCCEED(proofread, "call level", FuncArity(func), FALSE));

  if (!dump_cond()) return;

  TRACE_INSCOUNT(); TRACE_PRINTF("%s: ", s);

  TRACE_PRINTF(GetString(FuncName(func)));
  if (FuncArity(func) > 0) {
    putc('(', TraceFile);
    for (i = 0; i < FuncArity(func); i++) {
      if (i > 0) putc(',', TraceFile);
      DerefDisplayTerm(X(i), stream_trace, TRUE);
    }
    putc(')', TraceFile);
  }
  putc('\n', TraceFile);
}
#endif

/* ------------------------------------------------------------------------- */
/* Safe print routines (works even with gc bits on) */

#if defined(DEBUG_TRACE) || defined(USE_LOWRTCHECKS) || defined(USE_DEBUG_INSCOUNT)
static void wr_functor_1(definition_t *func) {
#if defined(OPTIM_COMP)
  TRACE_PRINTF("%s/%ld", GetString(FuncName(func)), (long)FuncArity(func));
#else
  if (!(func->printname & 1)) {
    TRACE_PRINTF("%s/%d", GetString(func->printname), func->arity);
  } else {
    TRACE_PRINTF("(?)"); // TODO: deprecate subdef or generalize for modules
  }
#endif
} 
void wr_functor(char *s, definition_t *func) {
  TRACE_PRINTF("%s: ",s);
  wr_functor_1(func);
  putc('\n', TraceFile);
}
#endif

#if !defined(OPTIM_COMP) && defined(DEBUG_TRACE)

#define TRACE_CALLS_SHOW_ARG1  1
//#define TRACE_CALLS_SHOW_ARGS  1

CVOID__PROTO(wr_call, char *s, definition_t *func) {
  int i;

  fprintf(TraceFile, "%s: ",s);

  if (!(func->printname & 1)) {
#if defined(TRACE_CALLS_SHOW_ARG1) || defined(TRACE_CALLS_SHOW_ARGS) /* display args */
    fprintf(TraceFile, "%s", GetString(func->printname));
    if (func->arity > 0) {
      putc('(', TraceFile);
      DerefDisplayTerm(X(0), Output_Stream_Ptr, TRUE);
#if defined(TRACE_CALLS_SHOW_ARGS) /* display all args */
      for (i = 1; i < func->arity; i++) {
        fprintf(TraceFile, ",");
        DerefDisplayTerm(X(i), Output_Stream_Ptr, TRUE);
      }
#else
      for (i = 1; i < func->arity; i++) fprintf(TraceFile, ",_");
#endif
      putc(')', TraceFile);
    }
#else /* only display functor */
    fprintf(TraceFile, "%s/%d", GetString(func->printname), func->arity);
#endif
  } else {
    fprintf(TraceFile, "(?)"); // TODO: deprecate subdef?
  }

  putc('\n', TraceFile);
}

#else

int eng_debug__dummy[0]; /* prevent "no symbols" warnings in .a creation */

#endif
