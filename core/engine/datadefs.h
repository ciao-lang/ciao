/*
 *  datadefs.h
 *
 *  Common header files for the Ciao engine
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 */

#ifndef _DATADEFS_H
#define _DATADEFS_H

/* (Automatically generated during engine build) */
#include <ciao/version.h>
#include <ciao/configure.h>

#include <ciao/absmach_predef.h>
#include <ciao/os_threads.h>

/* =========================================================================== */
/* Margins and initial allocation parameters. */

// #define USE_OVERFLOW_EXCEPTIONS 1  /* Deactivated by default */

#define kCells   1024

#define STATICMAXATOM 1024     /* Avoid very long atoms inside the engine */

#if defined(USE_OVERFLOW_EXCEPTIONS)
#undef  USE_DYNAMIC_ATOM_SIZE   /* overflow excp. requires static atom size */      
#else     
#define USE_DYNAMIC_ATOM_SIZE   /* By default */
#endif

#if defined(USE_DYNAMIC_ATOM_SIZE) 
# define MAXATOM  Atom_Buffer_Length
#else
# define MAXATOM  STATICMAXATOM
#endif

#define CONTPAD 128             /* min. amount of heap at proceed */
#define CALLPAD (2*(MAXATOM) + CONTPAD) /* min. amount of heap at call */
#define EXCEPAD (CALLPAD*2) /* min. amount of heap at low-level throw */

/* TODO: When does CALLPAD really need dynamic MAXATOM? Avoid it if possible */
/* Static version of CALLPAD (should be the same value used in plwam) */
#define STATIC_CALLPAD (2*(STATICMAXATOM) + CONTPAD)

#define HARD_HEAPPAD CALLPAD 

#if defined(USE_OVERFLOW_EXCEPTIONS)

#define DEFAULT_SOFT_HEAPPAD  EXCEPAD
#define SOFT_HEAPPAD          w->misc->soft_heappad 
#define Heap_Limit            w->misc->heap_limit

#else 

#define DEFAULT_SOFT_HEAPPAD  HARD_HEAPPAD
#define SOFT_HEAPPAD          HARD_HEAPPAD

#endif

/* min. amount of stack at allocate */
#define STACKPAD (2*ARITYLIMIT + 16)

/* min. amount of trail/choice at try */
#define CHOICEPAD (2*ARITYLIMIT)

#define ATMTABSIZE  (4*kCells)  /* size of global atom table  */
#define QLOADSIZE   (2*kCells)  /* plenty at present */

#if (defined(ANDPARALLEL) || defined(PARBACK))
#define GLOBALSTKSIZE   (600*kCells-1) /* Was 6*kCells-1 (DCG) */
#define LOCALSTKSIZE    (300*kCells-1)
#define CHOICESTKSIZE   (300*kCells-1)
#define TRAILSTKSIZE    (300*kCells-1)
#else
#define GLOBALSTKSIZE   (16*kCells-1) /* Was 6*kCells-1 (DCG) */
#define LOCALSTKSIZE    (4*kCells-1)
#define CHOICESTKSIZE   (4*kCells-1)
#define TRAILSTKSIZE    (4*kCells-1)
#endif

#define XREGBANKSIZE    ARITYLIMIT

/* =========================================================================== */
/* Principal WAM areas and registers. */

/* Access macros for the principal WAM registers, bytecode offsets, etc. */

/*
  Macros CIAO_REG_n, used to declare a variable and its type, force
  the GCC compiler to store a variable in a register.  In principle,
  they are ranked in ascending order of effectiveness, according to
  empiric measurements.  This can of course change if compilers (GCC,
  in this case) change the way registers are assigned, and it makes
  sense to have an order because there are processors whose
  instruction set is not orthogonal to register names.  n currently
  goes from 1 to 4, but it can be enlarged for additional
  architectures.

  Undefined the macro MAP_TO_HW_REGISTERS to disable this mapping a
  fall back to what the compiler thinks is reasonable.

  Register assignments (for registers 1 to 3) have been made for i686.
*/

#if defined(__clang__)
/* clang/llvm does not yet support explicit registers variables */
#else
/* Temporarily DISABLED:

   It seems to be broken in GCC 6 (Ubuntu 17). A fix consinst on
   disabling the use of 'ebx' register:
   
     // #define CIAO_REG_2(Type, Var) register Type Var asm("ebx")
     #define CIAO_REG_2(Type, Var) Type Var

   However, performance (using ecrc benchmark) does not seem to be
   better on the general case (although it makes some particular
   benchmarks faster). E.g., geometrical average KLIPS

     32bit with HW regs    -> 17533.91836
     32bit without HW regs -> 19492.57422
     64bit without HW regs -> 23928.04371

   We will keep this optimization temporarily disabled until we find a
   better assignment.

   MCL & JF
*/
/* #define MAP_TO_HW_REGISTERS */
#endif

/* TODO: missing x86_64 */
#if defined(__GNUC__) && defined(MAP_TO_HW_REGISTERS) 
# if defined(i686) && (defined(LINUX) || defined(BSD))
#  define CIAO_REG_1(Type, Var) register Type Var asm("esi")
#  define CIAO_REG_2(Type, Var) register Type Var asm("ebx")
#  define CIAO_REG_3(Type, Var) register Type Var asm("edi")
#  define CIAO_REG_4(Type, Var) Type Var
# elif defined(i686) && defined(DARWIN)
#  define CIAO_REG_1(Type, Var) register Type Var asm("esi")
#  define CIAO_REG_2(Type, Var) register Type Var asm("edi")
#  define CIAO_REG_3(Type, Var) Type Var
#  define CIAO_REG_4(Type, Var) Type Var
# else
#  define CIAO_REG_1(Type, Var) Type Var
#  define CIAO_REG_2(Type, Var) Type Var
#  define CIAO_REG_3(Type, Var) Type Var
#  define CIAO_REG_4(Type, Var) Type Var
# endif
#else
# define CIAO_REG_1(Type, Var) Type Var
# define CIAO_REG_2(Type, Var) Type Var
# define CIAO_REG_3(Type, Var) Type Var
# define CIAO_REG_4(Type, Var) Type Var
#endif

#define Arg w

#define WToX0           (SIZEOF_FLEXIBLE_STRUCT(worker_t, tagged_t, 0)/sizeof(tagged_t))
#define Xb(I)           (*CharOffset(w,I)) /* I as bytecode operand */
#define X(I)            (w->term[I]) /* I as zero-based */
#define Xop(X)  (((X)+WToX0)*sizeof(tagged_t))
#define Xinv(X) (((X)/sizeof(tagged_t))-WToX0)

/* # X regs used for control in choicepoints for dynamic code */
/*
   X(0) -
   X(1) -
   X(2) - x2_next clause pointer / handle.
   X(3) -
   X(4) - clock (used even in conc. predicates, although ignored).
   X(5) - x5_next clause pointer / handle.
   ------ The next ones, only meaningful for concurrent predicates.
   X(6) - predicate root (needed in case there are no clause pointers - MCL).
   X(7) - blocking/non-blocking and exited/non exited (MCL).
   X(8) - pointer to previous dynamic concurrent choicepoint.
*/


#if defined(ANDPARALLEL) || defined(PARBACK)
#define LOCAL     FALSE
#define REMOTE    TRUE
#endif

#define X2_CHN 2
#define ClockSlot 4
#define X5_CHN 5
#define RootArg 6
#define InvocationAttr 7
#define PrevDynChpt 8
#define DynamicPreserved 9

/* NOTE: was 1<<0 and 1<<1; avoid reserved bits */
#define BLOCKIDX ((intmach_t)1<<tagged__num_offset)
#define EXECIDX  ((intmach_t)1<<(tagged__num_offset+1))

#define SET_BLOCKING(arg) (arg) = ((arg) | BLOCKIDX)
#define SET_NONBLOCKING(arg) (arg) = ((arg) & ~BLOCKIDX)
#define IS_BLOCKING(arg) ((arg) & BLOCKIDX)
#define IS_NONBLOCKING(arg) !((arg) & BLOCKIDX)

#define SET_EXECUTING(arg) (arg) = ((arg) | EXECIDX)
#define SET_NONEXECUTING(arg) (arg) = ((arg) & ~EXECIDX)
#define EXECUTING(arg) ((arg) & EXECIDX)
#define NONEXECUTING(arg) !((arg) & EXECIDX)


/* initial choicepoint */
#define InitialNode ChoiceCharOffset(Choice_Start,ArityToOffset(1))

/* initial value_trail size: leave room for an extra choicept */
#define InitialValueTrail (-(SIZEOF_FLEXIBLE_STRUCT(node_t, tagged_t, 0)/sizeof(tagged_t)))

#define EToY0           (SIZEOF_FLEXIBLE_STRUCT(frame_t, tagged_t, 0)/sizeof(tagged_t))
#define Yb(I)           (*CharOffset(E,I)) /* I as bytecode operand */
#define Y(I)            (E->term[I]) /* I as zero-based */
#define Yop(X)  (((X)+EToY0)*sizeof(tagged_t))
#define Yinv(X) (((X)/sizeof(tagged_t))-EToY0)

#define FrameSize(L) BCOp0((L), FTYPE_ctype(f_e), -FTYPE_size(f_e))
#define FrameSizeToCount(O)     ((O)/sizeof(tagged_t)-EToY0)

/* Private areas for a thread, related to the overall wam status */

#if defined(TABLING)
#define StackFReg                       (w->misc->stack_freg)
#define HeapFReg                        (w->misc->heap_freg)
#define FirstNodeTR(Node)               ((node_tr_t*)(Node)->local_top)
#define SetFirstNodeTR(Node,Value)      ((Node)->local_top = (frame_t*)(Value))
#define LastNodeTR                      (w->misc->last_node_tr)
#define FrozenChpt(Node)   ((Node)->global_top == (tagged_t*)&(HeapFReg))
#define NODE_TR_SIZE(NodeTR)            (((node_tr_t*)(NodeTR))->size)
#define NODE_TR_TRAIL_SG(NodeTR)        (((node_tr_t*)(NodeTR))->trail_sg)
#define NODE_TR_NEXT(NodeTR)            (((node_tr_t*)(NodeTR))->next)
#define NODE_TR_CHAIN(NodeTR)           (((node_tr_t*)(NodeTR))->chain)
#endif

#if defined(ANDPARALLEL)
#define Goal_List_Start                   (w->misc->goal_list_start)
#define Goal_List_Top                     (w->misc->goal_list_top)
#define Goal_Cache                        (w->misc->goal_cache)
#define Dep_Size                          (w->misc->dep_size_exec)
#define Dep_Id                            (w->misc->dep_id_exec)
#define Goal_List_Lock                    (w->misc->goal_list_l)
#define Event_Queue_Start                 (w->misc->event_queue_start)
#define Event_Queue_Top                   (w->misc->event_queue_top)
#define Event_Queue_Lock                  (w->misc->event_queue_l)
#define Last_Parallel_Exec                (w->misc->last_parallel_exec)
#define Cancel_Goal_Exec                  (w->misc->cancel_goal_exec)
#define Safe_To_Cancel                    (w->misc->safe_to_cancel)
#define Mutex_Lock                        (w->misc->mutex_l)
#define Mode                              (w->misc->mode)
#define Waiting_For_Work_Cond_Var         (w->misc->waiting_for_work_cv)
#define Waiting_For_Work_Lock             (w->misc->waiting_for_work_l)
#define Suspended_Waiting_For_Work        (w->misc->suspended_waiting_for_work)
#define Suspend                           (w->misc->suspend)
#if defined(Solaris)
#define Total_Suspending_Time             (w->misc->total_suspending_time)
#define Suspending_Time_Cont              (w->misc->suspending_time_cont)
#endif
#define Next_Wam                          (w->misc->next_wam)

#define Goal_List_Start_Of(x)             (x->misc->goal_list_start)
#define Goal_List_Top_Of(x)               (x->misc->goal_list_top)
#define Goal_Cache_Of(x)                  (x->misc->goal_cache)
#define Dep_Size_Of(x)                    (x->misc->dep_size_exec)
#define Dep_Id_Of(x)                      (x->misc->dep_id_exec)
#define Goal_List_Lock_Of(x)              (x->misc->goal_list_l)
#define Event_Queue_Start_Of(x)           (x->misc->event_queue_start)
#define Event_Queue_Top_Of(x)             (x->misc->event_queue_top)
#define Event_Queue_Lock_Of(x)            (x->misc->event_queue_l)
#define Last_Parallel_Exec_Of(x)          (x->misc->last_parallel_exec)
#define Cancel_Goal_Exec_Of(x)            (x->misc->cancel_goal_exec)
#define Safe_To_Cancel_Of(x)              (x->misc->safe_to_cancel)
#define Mutex_Lock_Of(x)                  (x->misc->mutex_l)
#define Mode_Of(x)                        (x->misc->mode)
#define Waiting_For_Work_Cond_Var_Of(x)   (x->misc->waiting_for_work_cv)
#define Waiting_For_Work_Lock_Of(x)       (x->misc->waiting_for_work_l)
#define Suspended_Waiting_For_Work_Of(x)  (x->misc->suspended_waiting_for_work)
#define Suspend_Of(x)                     (x->misc->suspend)
#if defined(Solaris)
#define Total_Suspending_Time_Of(x)       (x->misc->total_suspending_time)
#define Suspending_Time_Cont_Of(x)        (x->misc->suspending_time_cont)
#endif
#define Next_Wam_Of(x)                    (x->misc->next_wam)

/* Value types for controlling agents execution when expanding stack
   sets */
#define RELEASED                          0
#define TOSUSPEND                         1
#define SUSPENDED                         2
#define WAITING                           3

/* Accion types for releasing execution of deterministic parallel goals */
#define FORWARD_EXEC                      0
#define UNWIND                            1

#if defined(VISANDOR)
#define Count_Calls(x)                    (x->misc->my_count_calls)
#define Time_Each_Call(x)                 (x->misc->my_time_each_call)
#define Pcall_Level(x)                    (x->misc->pcall_level)
#define TmpEvPtr(x)                       (x->misc->tmpevptr)
#define NextEvent(x)                      (x->misc->nextevent)
#define FirstEvent(x)                     (x->misc->firstevent)
#define LastEvent(x)                      (x->misc->lastevent)

/* Event types for tracing tool VisAndOr */
#define FORK         1
#define START_GOAL   2
#define FINISH_GOAL  3
#define JOIN         4

#define START_TIME   5
#define STOP_TIME    6

#define AGENT_BUSY   7  /* working */
#define AGENT_IDLE   8  /* waiting for work */

// Never used by VisAndOr!
// #define CREATE_WAM   9
// #define CREATE_AGENT 10
#endif

#endif

#if defined(PARBACK)
#define Act_PF                            (w->misc->pf)
#define Goal_List_Start                   (w->misc->goal_list_start)
#define Goal_List_Top                     (w->misc->goal_list_top)
#define Cancel_Goal_List_Start            (w->misc->cancel_goal_list_start)
#define Cancel_Goal_List_Top              (w->misc->cancel_goal_list_top)
#define Back_Goal_List_Start              (w->misc->back_goal_list_start)
#define Back_Goal_List_Top                (w->misc->back_goal_list_top)
#define Goal_To_Cancel                    (w->misc->goal_to_cancel)
#define Safe_To_Cancel                    (w->misc->safe_to_cancel)
#define Mutex_Lock                        (w->misc->mutex_l)
#define Waiting_For_Work_Cond_Var         (w->misc->waiting_for_work_cv)
#define Waiting_For_Work_Lock             (w->misc->waiting_for_work_l)
#define Suspended_Waiting_For_Work        (w->misc->suspended_waiting_for_work)
#define Suspend                           (w->misc->suspend)
#define Next_Wam                          (w->misc->next_wam)

#define Goal_List_Start_Of(x)             (x->misc->goal_list_start)
#define Goal_List_Top_Of(x)               (x->misc->goal_list_top)
#define Cancel_Goal_Exec_Handler_Of(x)    (x->misc->cancel_goal_exec)
#define Safe_To_Cancel_Of(x)              (x->misc->safe_to_cancel)
#define Mutex_Lock_Of(x)                  (x->misc->mutex_l)
#define Waiting_For_Work_Cond_Var_Of(x)   (x->misc->waiting_for_work_cv)
#define Waiting_For_Work_Lock_Of(x)       (x->misc->waiting_for_work_l)
#define Suspended_Waiting_For_Work_Of(x)  (x->misc->suspended_waiting_for_work)
#define Suspend_Of(x)                     (x->misc->suspend)
#define Next_Wam_Of(x)                    (x->misc->next_wam)

/* Value types for controlling agents execution when expanding stack
   sets */
#define RELEASED                          0
#define TOSUSPEND                         1
#define SUSPENDED                         2
#define WAITING                           3

#endif

#define Heap_Start          w->heap_start
#define Heap_End            w->heap_end
#define Heap_Warn_Soft      w->heap_warn_soft
#define Heap_Warn           w->heap_warn
#define Int_Heap_Warn       w->int_heap_warn
#define Stack_Start         w->stack_start
#define Stack_End           w->stack_end
#define Stack_Warn          w->stack_warn
#define Choice_End          w->choice_end
#define Choice_Start        w->choice_start


#define USE_TAGGED_CHOICE_START

#if defined(USE_TAGGED_CHOICE_START)
#define Tagged_Choice_Start w->tagged_choice_start
#endif

#define Trail_Start         w->trail_start
#define Trail_End           w->trail_end


/* These are related to the I/O pointers */

#define Input_Stream_Ptr    w->streams->input_stream_ptr
#define Output_Stream_Ptr   w->streams->output_stream_ptr
#define Error_Stream_Ptr    w->streams->error_stream_ptr


/* These access the private stack for bignum operations */

#define Numstack_Top        w->numstack_top
#define Numstack_End        w->numstack_end
#define Numstack_First      w->numstack_first
#define Numstack_Last       w->numstack_last


/* These keep the current state of the debugger. */

#define Current_Debugger_State w->debugger_info->current_debugger_state
#define Current_Debugger_Mode  w->debugger_info->current_debugger_mode


/* Local space to generate atoms and other general string operations */

#define Atom_Buffer          w->atom_buffer
#define Atom_Buffer_Length   w->atom_buffer_length


/* Incore compiling */

#define Last_Insn            (w->last_insn)

/* Expanded worker (not always active) */

#define Expanded_Worker (w->misc->expanded_worker)

#define Next_Worker(w)  (w->misc->next_worker)

#define Stop_This_Goal(w) (w->misc->stop_this_goal)

/* Global variables */

#define GLOBVAR(i) w->misc->globalvar[i]

/* The local (per-thread) definitions for garbage collection */

#define Gc_Total_Grey    (w->misc->gc_total_grey)
#define Gcgrey           (w->misc->gcgrey)
#define Total_Found      (w->misc->total_found)
#define Cvas_Found       (w->misc->cvas_found)
#define Gc_Aux_Node      (w->misc->gc_aux_node)
#define Gc_Choice_Start  (w->misc->gc_choice_start)
#define Gc_Trail_Start   (w->misc->gc_trail_start)
#define Gc_Heap_Start    (w->misc->gc_heap_start)
#define Gc_Stack_Start   (w->misc->gc_stack_start)


/* Topmost choicepoint for calls to concurrent facts. */

#define TopConcChpt     w->misc->top_conc_chpt

/* Global registers */

#define GlobReg(Which) w->misc->globalreg[Which]

/* Throwing exceptions */

#define ErrArgNo w->misc->errargno
#define ErrCode w->misc->errcode
#define ErrFuncName w->misc->errfuncname
#define ErrFuncArity w->misc->errfuncarity
#define Culprit w->misc->culprit

#if defined(TABLING)
#define NodeLocalTop(Node)                                      \
  (((Node)->global_top != (tagged_t*)(&(HeapFReg))) ?           \
   (Node)->local_top : StackFReg)

#define NodeGlobalTop(Node)                                     \
  (((Node)->global_top != (tagged_t*)(&(HeapFReg))) ?           \
   (Node)->global_top : HeapFReg)

#else
#define NodeLocalTop(Node)  (Node)->local_top

#define NodeGlobalTop(Node) (Node)->global_top
#endif

/* If you edit this defn you must update ComputeE as well. */
#define ComputeA(A,B) \
{ \
  if (w->local_top) \
    (A) = w->local_top; \
  else if (!StackYounger((A) = NodeLocalTop(B),w->frame)) \
    (A) = StackCharOffset(w->frame,FrameSize(w->next_insn)); \
}

#define NewShadowregs(Gtop) \
{ \
  w->global_uncond = TagHVA(Gtop); \
  w->local_uncond = TagSVA(w->local_top); \
}

#define SetShadowregs(Chpt) \
{ \
  w->global_uncond = TagHVA(NodeGlobalTop(Chpt));     \
  w->local_uncond = TagSVA(NodeLocalTop(Chpt));      \
}

#define SaveGtop(Chpt,Gtop) \
  (Chpt)->global_top = (Gtop);

#define RestoreGtop(Chpt) \
{ \
  w->global_top = NodeGlobalTop(Chpt); \
}

#define SaveLtop(Chpt) \
  (Chpt)->local_top = w->local_top;

#define RestoreLtop(Chpt) \
  w->local_top = NodeLocalTop(Chpt);

#if defined(ANDPARALLEL) && defined(VISANDOR)
/* Event output macros for tracing tool VisAndOr */

#if defined(SYMM)
#define USCLK_EXISTS
#undef  NSCLK_EXISTS
#define TIME_INIT  usclk_init()
#define TIME       ((float)getusclk() - time_at_event_start*1e6)
#endif

/* #if defined(Solaris) */
#if defined(DARWIN) || defined(LINUX) || defined(Solaris) || defined(Win32) || defined(BSD)
#undef  USCLK_EXISTS
#define NSCLK_EXISTS
#define TIME_INIT   
#define TIME   ((gethrtime()/1000) - (time_at_event_start*1e6))
#endif

#define EVENT(Event, PPF, NFork) \
 { \
   if ( gen_event_file ) { \
     if ( (TmpEvPtr(w) = (NextEvent(w))++) == LastEvent(w) ) { \
          fprintf(stderr, "Agent 0x%p event table overflow.\n", w); \
          gen_event_file = FALSE; \
     } \
     else { \
          TmpEvPtr(w)->timestamp = (uintmach_t)TIME; \
          TmpEvPtr(w)->evtype = (Event); \
          TmpEvPtr(w)->ppf = (PPF); \
          TmpEvPtr(w)->nfork = (NFork); \
          TmpEvPtr(w)->wam = w; \
     } \
   } \
 }
#endif

/* EVENTS    ------------------------------------- */

#define SetEvent        (TestEvent ? 0 : (Heap_Warn_Soft = Heap_Start))
#define TestEvent       (Heap_Warn!=Heap_Warn_Soft)

/* MARGINS   ------------------------------------- */

/* Update heap margins (which depends on dynamic CALLPAD) */
#if defined(USE_DYNAMIC_ATOM_SIZE)
#define UpdateHeapMargins() { \
    int wake_count = WakeCount; \
    Int_Heap_Warn = (Int_Heap_Warn==Heap_Warn \
                     ? HeapOffset(Heap_End,-SOFT_HEAPPAD) \
                     : Heap_Start); \
    Heap_Warn = HeapOffset(Heap_End,-SOFT_HEAPPAD); \
    if (wake_count>=0) \
      Heap_Warn_Soft = HeapCharOffset(Heap_Start,-wake_count); \
    else \
      Heap_Warn_Soft = Int_Heap_Warn; \
}
#else
#define UpdateHeapMargins() {}
#endif

/* =========================================================================== */

#include <ciao/termdefs.h>

/* =========================================================================== */
/* Access macros for the various WAM areas. */

#define Offset(X,O)     ((tagged_t *)(X) + (O))
#define CharOffset(X,O) ((tagged_t *)((char *)(X) + (O)))

/* THE HEAP */

/* assuming heap growth in positive direction */

#define OnHeap(t)       (HeapYounger(Heap_End,t) && !HeapYounger(Heap_Start,t))
#define OffHeaptop(t,H)          (!HeapYounger(H,t))
#define HeapYounger(X,Y)        ((tagged_t *)(X)>(tagged_t *)(Y))
#define HeapDifference(X,Y)     ((tagged_t *)(Y) - (tagged_t *)(X))
#define HeapCharDifference(X,Y) ((char *)(Y) - (char *)(X))
#define HeapOffset(X,O)         ((tagged_t *)(X) + (O))
#define HeapCharOffset(X,O)     ((tagged_t *)((char *)(X) + (O)))
#define HeapNext(X)             (*(X)++)
#define HeapPush(H,X)           (*(H) = (X), (H)++) /* X often contains H */
#define HeapDecr(H)             (--(H))

/* THE FRAME STACK */

/* assuming stack growth in positive direction */

#define OnStack(t) (StackYounger(Stack_End,t) && !StackYounger(Stack_Start,t))
#define OffStacktop(t,H)         (!StackYounger(H,t))
#define StackYounger(X,Y)       ((tagged_t *)(X)>(tagged_t *)(Y))
#define StackDifference(X,Y)    ((tagged_t *)(Y) - (tagged_t *)(X))
#define StackCharDifference(X,Y)        ((char *)(Y) - (char *)(X))
#define StackOffset(X,O)        ((tagged_t *)(X) + (O))
#define StackCharOffset(X,O)    ((frame_t *)((char *)(X) + (O)))
#define StackNext(P)            (*(P)++)
#define StackDecr(P)            (--(P))

/* THE TRAIL */

/* assuming trail growth in positive direction */

#define OnTrail(t)  (TrailYounger(Trail_End,t) && !TrailYounger(Trail_Start,t))
#define OffTrailtop(t,P)        (!TrailYounger(P,t))
#define TrailYounger(X,Y)       ((tagged_t *)(X)>(tagged_t *)(Y))
#define TrailDifference(X,Y)    ((tagged_t *)(Y) - (tagged_t *)(X))
#define TrailCharDifference(X,Y)        ((char *)(Y) - (char *)(X))
#define TrailOffset(X,O)        ((tagged_t *)(X) + (O))
#define TrailCharOffset(X,O)    ((tagged_t *)((char *)(X) + (O)))
#define TrailPop(P)             (*--(P))
#define TrailNext(P)            (*(P)++)
#define TrailPush(P,X)          (*(P)++ = (X))
#define TrailPushCheck(P,X)     trail_push_check(Arg,X)

/* THE CHOICEPOINT STACK */

/* assuming choicestack growth in negative direction */

#define OnChoice(t) (ChoiceYounger(t,Choice_Start) && !ChoiceYounger(t,Choice_End))
#define OffChoicetop(t,B)       ChoiceYounger(t,B)
#define ChoiceYounger(X,Y)      ((tagged_t *)(X)<(tagged_t *)(Y))
#define ChoiceOffset(X,O)       ((tagged_t *)(X) - (O))
#define ChoiceCharOffset(X,O)   ((node_t *)((char *)(X) - (O)))
#define ChoiceDifference(X,Y)   ((tagged_t *)(X) - (tagged_t *)(Y))
#define ChoiceCharDifference(X,Y)       ((char *)(X) - (char *)(Y))
#define ChoicePrev(P)           (*(P)++)
#define ChoiceNext(P)           (*--(P))
#define ChoicePush(P,X)         (*--(P) = (X))

#if defined(USE_TAGGED_CHOICE_START)
#define ChoiceFromInt(Y) (ChoiceCharOffset(Tagged_Choice_Start,Y))
#define ChoiceToInt(Y)   (ChoiceCharDifference(Tagged_Choice_Start,Y))
#else
#define ChoiceFromInt(Y) ((node_t *)ChoiceOffset(Choice_Start,(GetSmall(Y))))
#define ChoiceToInt(Y)   (MakeSmall(ChoiceDifference(Choice_Start,Y)))
#endif

#define RefHeap(To,From) \
{ To = *(From); }

#define RefCar(To,From) \
{ To = *TagToCar(From); }

#define RefCdr(To,From) \
{ To = *TagToCdr(From); }

#define RefArg(To,From,I) \
{ To = *TagToArg(From,I); }

#define RefHeapNext(To,From) \
{ To = *(From)++; }

#define PushRefHeapNext(To,From) \
{ *(To)++ = *(From)++; }

#define RefStack(To,From) \
{ To = *(From); }

#define HeapPushRefStack(To,From) \
{ *(To)++ = *(From); }

#define RefHVA(To,From) \
{ To = *TagToHVA(From); }

#define RefCVA(To,From) \
{ To = *TagToCVA(From); }

#define RefSVA(To,From) \
{ To = *TagToSVA(From); }

#define LoadSVA(Y)              {Y = TagSVA(&Y); }
#define Load2SVA(X,Y)           {X = Y = TagSVA(&Y); }
#define PreLoadHVA(X,H)         {X = TagHVA(H); }
#define ConstrHVA(H)            {HeapPush(H,TagHVA(H)); }
#define LoadHVA(To,H)           {HeapPush(H,To = TagHVA(H)); }
#define Load2HVA(To1,To2,H)     {HeapPush(H,To1 = To2 = TagHVA(H)); }
#define LoadCVA(To,H)           {HeapPush(H,To = TagCVA(H)); }

/* =========================================================================== */
/* Definitions for symbol tables, databases, predicates. */

/* Object identifiers and emulator "pseudo instructions" - never seen
   by main dispatchers. */
typedef short enter_instr_t;

/* These six must be first, in this order. */
#define ENTER_COMPACTCODE 0
#define ENTER_COMPACTCODE_INDEXED 1
#define ENTER_PROFILEDCODE 2
#define ENTER_PROFILEDCODE_INDEXED 3
#define ENTER_FASTCODE 4
#define ENTER_FASTCODE_INDEXED 5
/* */
#define ENTER_UNDEFINED 6
#define ENTER_C 7
#define ENTER_INTERPRETED 8
#define BUILTIN_ABORT 9
#define BUILTIN_APPLY 10
#define BUILTIN_CALL 11
#define BUILTIN_SYSCALL 12
#define BUILTIN_NODEBUGCALL 13
#define BUILTIN_TRUE 14
#define BUILTIN_FAIL 15
#define BUILTIN_CURRENT_INSTANCE 16
#define BUILTIN_RESTORE 17
#define BUILTIN_COMPILE_TERM 18
#define BUILTIN_GELER 19
#define BUILTIN_INSTANCE 20
#define BUILTIN_DIF 21
#define SPYPOINT 22
#define WAITPOINT 23
#define BREAKPOINT 24
/* Other object identifiers -- these are used to inform the gc */
#define TABLE 25
#define EMUL_INFO 26
#define OTHER_STUFF 27

typedef struct instance_handle_ instance_handle_t;
typedef struct instance_ instance_t;
typedef struct int_info_ int_info_t;
typedef struct sw_on_key_ sw_on_key_t;

// uncomment to enable atom GC (incomplete)
// #define ATOMGC 1

/* OBJECT AREA ----------------------------------------------------*/ 

#define CACHE_INCREMENTAL_CLAUSE_INSERTION

typedef unsigned short int instance_clock_t;

#define ACTIVE_INSTANCE(ARG,I,TIME,CHAINP) \
  ((I)==NULL ? NULL : \
   ((TIME) >= (I)->birth && (TIME) < (I)->death) ? (I) : \
   active_instance(ARG,I,TIME,CHAINP))


 /* This one does not look at time creation of clauses. */

#define ACTIVE_INSTANCE_conc(ARG,I,ROOT)  \
     active_instance_conc(ARG,I,ROOT) 

CFUN__PROTO(active_instance, instance_t *, instance_t *i,int time,bool_t normal);

/* p->count = (intmach_t *)((char *)p + objsize) - p->counters */

#define NumberOfCounters(cl) \
  ((intmach_t *)((char *)cl + cl->objsize) - cl->counters)

/* Clauses of compiled predicates are stored as a linked list of
   records. The forward link "next" of the last clause contains the
   total number of clauses. */

typedef struct emul_info_ emul_info_t;

typedef union clause_link_ clause_link_t;
union clause_link_ {
  /* Clause pointer or number (used in emul_info to store links to
     the next clause or the total number of clauses) */
  uintmach_t number; /* clause number */
  emul_info_t *ptr;    /* clause pointer */   
};

struct emul_info_ {
  clause_link_t next;          /* next clause OR no. of clauses */
  definition_t *subdefs;
  intmach_t objsize;                             /* total # chars */
#if defined(GAUGE)
  intmach_t *counters;      /* Pointer to clause's first counter. */
#endif
  char emulcode[FLEXIBLE_SIZE];
};

/* TODO: (JFMC) give a better name to IS_CLAUSE_TAIL */

/* CLAUSE_TAIL */
#define CLAUSE_TAIL_INSNS_SIZE FTYPE_size(f_o) /* TODO: (JFMC) why? */
#define IS_CLAUSE_TAIL(EP) (EP->objsize == 0)
#define ALLOC_CLAUSE_TAIL(EP) {                                         \
    EP = checkalloc_FLEXIBLE(emul_info_t, char, CLAUSE_TAIL_INSNS_SIZE); \
    EP->subdefs = NULL;                                                 \
    EP->objsize = 0;                                                    \
  }

/* All invocations looking at an instance of an concurrent predicate will
   actually have a pointer to a pointer to the instance.  Every clause has a
   pointer to a queue of handles to itself.  Erasing a pointed to instance 
   will change the pointer to the instance itself (Confused? OK, ask me, I
   have a nice drawing on a napkin --- MCL) */

struct instance_handle_ {
  instance_t *inst_ptr;                    /* Pointer to the instance */
  tagged_t head;                            /* Goal called; allows indexing. */
  instance_handle_t *next_handle;             
  instance_handle_t *previous_handle;             
};

/* Terms recorded under a key or clauses of an interpreted predicate
   are stored as a doubly linked list of records.  The forward link is
   terminated by NULL; the backward link wraps around.  Each instance
   points back to the beginning of the list.  The rank field defines a
   total order on a list.  Two timestamps are associated with each
   instance to support proper semantics for dynamic code
   updates. (MCL, with help from the manual).  There are also two
   pointers (one for unindexed and other for indexed accesses) to
   queues which maintain the list of calls looking at each
   instance. */

struct instance_ {
  instance_t *forward;
  instance_t *backward;
  int_info_t *root;
  instance_t *next_forward;
  instance_t *next_backward;
  tagged_t key;
  tagged_t rank;
  instance_clock_t birth, death;                          /* Dynamic clause lifespan */

  instance_handle_t *pending_x2;       /* Seen by invocations looking @ here */
  instance_handle_t *pending_x5;       /* Seen by invocations looking @ here */

  int objsize;                                           /* total # chars */
  char emulcode[FLEXIBLE_SIZE];
};

/* Indexing information for dynamic predicates? First simple, common cases,
   then hash table indexing.  Includes information on openness/closeness of
   concurrent facts. (MCL) */

typedef enum {DYNAMIC, CONC_OPEN, CONC_CLOSED} int_behavior_t;

struct int_info_ {
  volatile int_behavior_t  behavior_on_failure;/* behavior if no clauses match. */

#if defined(CONDITIONAL_VARS)
  condition_t clause_insertion_cond;
#else
  /*  SLOCK clause_insertion_cond;            */
  condition_t clause_insertion_cond;
#endif

  instance_handle_t *x2_pending_on_instance;     /* Used when pred. is empty */
  instance_handle_t *x5_pending_on_instance;

  instance_t  *first  ;
  instance_t  *varcase;
  instance_t  *lstcase;
  sw_on_key_t *indexer;
};

typedef struct und_info_ und_info_t;
struct und_info_ {
  int unused;
};

extern stream_node_t *root_stream_ptr;

 /* Information about atoms */

typedef struct atom_ atom_t;
struct atom_ {        
  unsigned int has_squote:1;
  unsigned int has_dquote:1;
  unsigned int has_special:1;
  unsigned int index:29;
                               /* support for locking on atom names. MCL. */
#if defined(USE_THREADS)                    /* Do not waste space otherwise */
  LOCK atom_lock_l;                      /* May be held for a long time */
#if defined(GENERAL_LOCKS)
  volatile intmach_t atom_lock_counter;         /* For general semaphores */
  SLOCK counter_lock;                            /* Held for a short time */
#endif
#endif
  uintmach_t atom_len;
  char name[FLEXIBLE_SIZE];
};

/* For a given goal and an emulated predicate, alternatives that might match
   the goal are stored as a linked list of records */

/* try-retry-trust repr. as a linked list. */

struct try_node_ {
  try_node_t *next;                      /* Next alternative or NULL */
  bcp_t emul_p;                    /* write mode or not first alternative */
  bcp_t emul_p2;                          /* read mode, first alternative */
  short node_offset;                   /* offset from choicepoint to next */
  /*short number;*/
  uintmach_t number;                /* clause number for this alternative */
                             /* Gauge specific fields MUST come after this*/
#if defined(GAUGE)
  intmach_t *entry_counter;        /* Offset of counter for clause entry */
#endif
  };



#define ArityToOffset(A)  \
  (((A)+(SIZEOF_FLEXIBLE_STRUCT(node_t, tagged_t, 0)/sizeof(tagged_t))) * sizeof(tagged_t))
#define OffsetToArity(O)                                                \
  (((O)/sizeof(tagged_t))-(SIZEOF_FLEXIBLE_STRUCT(node_t, tagged_t, 0)/sizeof(tagged_t)))

#define SwitchSize(X) (((X)->mask / sizeof(sw_on_key_node_t))+1) 
#define SizeToMask(X) (((X)-1) * sizeof(sw_on_key_node_t))

typedef struct sw_on_key_node_ sw_on_key_node_t;
struct sw_on_key_node_ {
  /* NOTE: Make sure that the size of this structure is a power of 2
     (for efficiency) */
  tagged_t key;
  union {
    try_node_t *try_chain;         /* try-retry-trust as linked list */
    instance_t *instp;             /* int. clauses or recorded terms */
    sw_on_key_node_t *node;
    definition_t *def;                       /* predicate definition */
    module_t *mod;                              /* module definition */
    int_info_t *irootp;                             /* indexer info? */
    atom_t *atomp;               /* Info on atoms and main functors  */
    void *proc;                /* C function pointer (CFUN or CBOOL) */
  } value;
};

/* Indexing table are used in indexing on first argument in calls and in
   looking up predicates.  They are operated as hash tables with quadratic
   overflow handling.  Hash table access is performed by using some of the
   low order bits of the key as array index, and then searching for a hit or
   for a zero key indicating that the key is absent from the table. 
   
   MCL: changed to make room for erased atoms: now a key == 1 indicates
   that the entry has been erased (but the search chain continues).  New 
   atoms are placed in the first free entry.

   NOTE (MCL): data structure different from that in Sicstus 1.8!!
*/

struct sw_on_key_ {
  uintmach_t mask;                                          /* bitmask */
  intmach_t count;
#if defined(ATOMGC)
  intmach_t next_index;
#endif
  sw_on_key_node_t node[FLEXIBLE_SIZE];
};

/* Node from a byte offset */
#define SW_ON_KEY_NODE_FROM_OFFSET(Tab, Offset) \
  ((sw_on_key_node_t *)((char *)&(Tab)->node[0] + (Offset)))

typedef struct incore_info_ incore_info_t;
struct incore_info_ {
  clause_link_t clauses;                                  /* first clause */
  /* Pointer to either "next" field of last clause or "clauses"  */
  clause_link_t *clauses_tail;
#if defined(CACHE_INCREMENTAL_CLAUSE_INSERTION)
  emul_info_t *last_inserted_clause;        /* Pointer to the last clause */
  uintmach_t last_inserted_num;           /* Number of last ins. clause */
#endif
  try_node_t *varcase;
  try_node_t *lstcase;
  sw_on_key_t *othercase;
};

#define SetEnterInstr(F,I) \
{ \
  (F)->predtyp = (I); \
  (F)->enter_instr = \
    (F)->properties.spy ? SPYPOINT : \
      (F)->properties.wait ? WAITPOINT : \
        (F)->properties.breakp ? BREAKPOINT : \
          (F)->predtyp; \
}

typedef union definfo_ definfo_t;
union definfo_ {
  int_info_t *intinfo;
  und_info_t *undinfo;
  incore_info_t *incoreinfo;
  void *proc;
};

struct definition_ {
  enter_instr_t enter_instr;
  short arity; /*  */
  tagged_t printname;                           /* or sibling pointer | 1 */
                                                /* or parent pointer | 3 */
  struct {
    unsigned int spy:1;
    unsigned int breakp:1;
    /* unsigned int public:1; */
                      /* concurrent obeys declaration and implies dynamic */
    unsigned int concurrent:1;      /* 1 if wait on it, 0 if closed (MCL) */
    unsigned int wait:1;                             /* obeys declaration */
    unsigned int dynamic:1;                                       /* -""- */
    unsigned int multifile:1;                                     /* -""- */
    unsigned int nonvar:1;             /* seen P(X,...) :- var(X), !, ... */
    unsigned int var:1;             /* seen P(X,...) :- nonvar(X), !, ... */
  } properties;
  enter_instr_t predtyp;
  definfo_t code;
};

#define DEF_SIBLING(F) \
  ((F)->printname&2 ? NULL : (definition_t *)TagToPointer((F)->printname))

struct module_ {
  tagged_t printname;
  struct {
    unsigned int is_static:1;
  } properties;
};

/* Classified somewhere else */
extern sw_on_key_node_t **atmtab;
extern sw_on_key_t *ciao_atoms;
extern void *builtintab[];

typedef struct statistics_ statistics_t;
struct statistics_ {
  inttime_t ss_tick;                         /* time spent stack_shifting */
  intmach_t ss_global;                                 /* # global shifts */
  intmach_t ss_local;                                  /* # local shifts  */
  intmach_t ss_control;                         /* # control/trail shifts */
  inttime_t gc_tick;                             /* Total GC ticks (sec) */
  intmach_t gc_count;                            /* # garbage collections */
  intmach_t gc_acc;                         /* Total reclaimed heap space */

  inttime_t starttick;
  inttime_t lasttick;

  inttime_t startwalltick;
  inttime_t lastwalltick;
  inttime_t wallclockfreq;

  inttime_t startusertick;
  inttime_t lastusertick;
  inttime_t userclockfreq;

  inttime_t startsystemtick;
  inttime_t lastsystemtick;
  inttime_t systemclockfreq;
};

extern statistics_t ciao_statistics; /* Shared, I guess */

#if defined(GAUGE)
#define INCR_COUNTER(c)   ((*(c))++)
#endif

typedef struct other_stuff_ other_stuff_t;
struct other_stuff_ {     /* Usable type for passing data areas to gc etc */
  char *pointer;
  intmach_t size;
};

/* =========================================================================== */

#include <ciao/support.h>

#include <ciao/eng_debug.h>

/* =========================================================================== */
/* Global definitions for locks, terms, predicates, predicate tables,
   workers, etc. */
 
/*  INITIALIZATION INFO 
    definitions, structures, and global variables needed for
    initializations */

/* These should be local to a thread */

/* extern tagged_t heap_start, *heap_end, heap_warn, *heap_warn_soft,
   *stack_start, *stack_end, *stack_warn, tagged_choice_start, choice_start,
   *choice_end, trail_start, *trail_end; */

/* extern char *atom_buffer; */  /* Non-shared --- used by each worker */


/* I believe that atom_buffer_length can change dynamically; should be
   private to each worker, then.  Will that pose problems problems with the
   hashing functions? */

/* extern int atom_buffer_length; */  /* Non-shared --- used by each worker */


extern sw_on_key_t *prolog_predicates;    /* Shared -- never changes */
extern sw_on_key_t **predicates_location;                  /* Shared */
extern sw_on_key_t *prolog_modules;    /* Shared -- never changes */
extern sw_on_key_t **modules_location;                  /* Shared */

/* Database locks */
extern SLOCK    prolog_predicates_l;                      /* Pointer to it */
extern SLOCK    prolog_modules_l;                         /* Pointer to it */

/* Wait until new worker Id is generated */
extern SLOCK    worker_id_pool_l;
extern SLOCK    atom_id_l;
extern SLOCK    wam_list_l;

#if defined(PARBACK)
extern SLOCK wam_circular_list_l;   /* Mutex for adding stack sets into list */

extern worker_t *main_worker;  /* Pointer to main worker */

extern volatile int nagents;      /* Number of agents created */
extern SLOCK nagents_l;
#endif

#if defined(ANDPARALLEL)
extern SLOCK wam_circular_list_l;   /* Mutex for adding stack sets into list */
extern SLOCK stackset_expansion_l;  /* Mutex for stack set expansion */

extern worker_t *main_worker;  /* Pointer to main worker */
extern bool_t unwinding_done;         /* Has the unwinding been performed? */

extern int nagents;      /* Number of agents created */
extern SLOCK nagents_l;

/* Parallel goals */
extern int npargoals;
/* Parallel goals stolen */
extern int_par_t *npargoalstaken;
/* Backw exec over par local goal */
extern int_par_t *nlocalbacktr;
/* Backw exec over par rem top goal */
extern int_par_t *nrembacktr_top;
/* Backw exec over par rem trp goal */
extern int_par_t *nrembacktr_trapped;

extern SLOCK npargoals_l;
extern SLOCK npargoalstaken_l;
extern SLOCK nlocalbacktr_l;
extern SLOCK nrembacktr_top_l;
extern SLOCK nrembacktr_trapped_l;

extern bool_t measure;
#endif

#if defined(ANDPARALLEL) && defined(VISANDOR)
/* Event Tracing Flags etc for VisAndOr */
extern int nacagents;    /* Number of active agents */
extern int maxevents;    /* maximum number of events */
extern bool_t gen_event_file;
extern float time_at_event_start;
#endif

#if defined(DEBUG)
extern SLOCK    ops_counter_l;
#endif


extern bool_t wam_initialized;/* Non-shared? --- set by each worker to decide whether re-initialize or exit after a failure */

extern char *default_ciaoroot;
extern char *default_c_headers_dir;

 /* All atom & functor definitions are shared */

#if defined(MARKERS)
extern tagged_t atom_success;
extern tagged_t atom_failure;
#endif

extern tagged_t atom_share;
extern tagged_t atom_noshare;
extern tagged_t atom_nil;
extern tagged_t atom_list;
extern tagged_t atom_read;
extern tagged_t atom_write;
extern tagged_t atom_append;
extern tagged_t atom_socket;
extern tagged_t atom_symlink;
extern tagged_t atom_regular;
extern tagged_t atom_directory;
extern tagged_t atom_fifo;
extern tagged_t atom_stdout;
extern tagged_t atom_unknown;
extern tagged_t atom_prolog;
extern tagged_t atom_lessthan;
extern tagged_t atom_greaterthan;
extern tagged_t atom_equal;
extern tagged_t atom_off;
extern tagged_t atom_on;
extern tagged_t atom_error;
extern tagged_t atom_trace;
extern tagged_t atom_debug;
extern tagged_t atom_fail;
extern tagged_t atom_all;
extern tagged_t atom_terse;
extern tagged_t atom_verbose;
extern tagged_t atom_compiled;
extern tagged_t atom_interpreted;
extern tagged_t atom_builtin;
extern tagged_t atom_true;
extern tagged_t atom_false;
extern tagged_t atom_retry_hook;
extern tagged_t atom_unprofiled;
extern tagged_t atom_profiled;
/*extern tagged_t atom_public;*/
extern tagged_t atom_wait;
extern tagged_t atom_dynamic;
extern tagged_t atom_concurrent;
extern tagged_t atom_multifile;
extern tagged_t atom_user;
extern tagged_t atom_att;

extern tagged_t atom_self;
extern tagged_t atom_create;

extern tagged_t atom_default_ciaoroot;
extern tagged_t atom_default_c_headers_dir;

extern tagged_t atom_block;
extern tagged_t atom_no_block;

#if defined(GAUGE)
extern tagged_t atom_counter;
#endif

#if defined(USE_OVERFLOW_EXCEPTIONS)
extern tagged_t atom_undo_heap_overflow_excep;
#endif
extern tagged_t atom_heap_limit;

extern tagged_t functor_neck;
extern tagged_t functor_list;
extern tagged_t functor_cut;
extern tagged_t functor_minus;
extern tagged_t functor_slash;
extern tagged_t functor_and;
extern tagged_t functor_functor;
extern tagged_t functor_tagged;
extern tagged_t functor_emul_entry;
extern tagged_t functor_builtin;
extern tagged_t functor_Dref;
extern tagged_t functor_Dstream;
extern tagged_t functor_Dlock;
extern tagged_t functor_Dhandler;
extern tagged_t functor_Dsetarg;
extern tagged_t functor_large;
extern tagged_t functor_long;

extern tagged_t functor_active;
extern tagged_t functor_pending;
extern tagged_t functor_failed;
extern tagged_t functor_available;

extern tagged_t current_prompt;
extern tagged_t current_unknown;
/* extern tagged_t current_leash_mode; */
/* extern tagged_t current_maxdepth; */
/* extern tagged_t current_printdepth; */
/* extern tagged_t current_breaklevel; */
extern tagged_t current_compiling;
/* extern tagged_t current_character_escapes_flag; */
extern tagged_t current_ferror_flag;
/* extern tagged_t current_single_var_flag; */
/* extern tagged_t current_discontiguous_flag; */
/* extern tagged_t current_redefine_flag; */
extern tagged_t current_quiet_flag;
extern tagged_t current_gcmode;
extern tagged_t current_gctrace;
extern tagged_t current_gcmargin;
/*extern tagged_t current_debugger_state;*/  /* Now private */
/*extern tagged_t current_debugger_mode;*/   /* Now private */
extern tagged_t current_radix;

extern try_node_t *address_nd_repeat;
extern try_node_t *address_nd_current_instance;
extern try_node_t *address_nd_current_atom;
extern try_node_t *address_nd_current_predicate;
extern try_node_t *address_nd_predicate_property;
extern try_node_t *address_nd_current_stream;
extern try_node_t *address_nd_atom_concat;
#if defined(TABLING)
extern try_node_t *address_nd_fake_choicept;
#endif
#if defined(PARBACK)
extern try_node_t *address_nd_suspension_point;
extern bcp_t restart_point_insn;
#endif

extern definition_t *address_true;
extern definition_t *address_fail;

extern definition_t *address_call;
// #if defined(INTERNAL_CALLING)
// extern definition_t *address_internal_call;
// #endif
extern definition_t *address_interpret_goal;
extern definition_t *address_call_with_cont;
extern definition_t *address_interpret_compiled_goal;
extern definition_t *address_interpret_c_goal;
extern definition_t *address_undefined_goal;
extern definition_t *address_help; 
extern definition_t *address_restart; 
extern definition_t *address_trace;
extern definition_t *address_getct;
extern definition_t *address_getct1;
extern definition_t *address_get;
extern definition_t *address_get2;
extern definition_t *address_get1;
extern definition_t *address_get12;
extern definition_t *address_peek;
extern definition_t *address_peek2;
extern definition_t *address_get_byte1;
extern definition_t *address_get_byte2;
extern definition_t *address_peek_byte1;
extern definition_t *address_peek_byte2;
extern definition_t *address_skip;
extern definition_t *address_skip2;
extern definition_t *address_skip_line;
extern definition_t *address_skip_line1;
extern definition_t *address_error;

CBOOL__PROTO(nd_repeat);
CBOOL__PROTO(nd_current_atom);
CBOOL__PROTO(nd_current_clauses);
CBOOL__PROTO(nd_current_predicate);
CBOOL__PROTO(nd_predicate_property);
CBOOL__PROTO(nd_current_stream);
CBOOL__PROTO(nd_atom_concat);
#if defined(TABLING)
CBOOL__PROTO(nd_fake_choicept);
#endif
#if defined(PARBACK)
CBOOL__PROTO(nd_suspension_point);
#endif

definition_t *define_c_mod_predicate(char *module, char  *pname, int arity, cbool0_t procedure);
void undefine_c_mod_predicate(char *module, char *pname, int arity); 

module_t *define_c_static_mod(char *module_name);

/* =========================================================================== */
/* Term deref and switch */

#define DerefHeap(Xderef,Ptr) \
{ \
  tagged_t m_i; \
  tagged_t m_j; \
 \
  RefHeap(m_i,Ptr); \
  DerefHeapSwitch(m_i,m_j,{break;}) \
  Xderef = m_i; \
}

#define DerefCar(Xderef,Ptr) \
{ \
  tagged_t m_i; \
  tagged_t m_j; \
 \
  RefCar(m_i,Ptr); \
  DerefHeapSwitch(m_i,m_j,{break;}) \
  Xderef = m_i; \
}

#define DerefCdr(Xderef,Ptr) \
{ \
  tagged_t m_i; \
  tagged_t m_j; \
 \
  RefCdr(m_i,Ptr); \
  DerefHeapSwitch(m_i,m_j,{break;}) \
  Xderef = m_i; \
}

#define DerefArg(Xderef,Ptr,I) \
{ \
  tagged_t m_i; \
  tagged_t m_j; \
 \
  RefArg(m_i,Ptr,I); \
  DerefHeapSwitch(m_i,m_j,{break;}) \
  Xderef = m_i; \
}

#define DerefHeapNext(Xderef,Ptr) \
{ \
  tagged_t m_i; \
  tagged_t m_j; \
 \
  RefHeapNext(m_i,Ptr); \
  DerefHeapSwitch(m_i,m_j,{break;}) \
  Xderef = m_i; \
}


#define DEREF(Xderef,X) \
{ \
  tagged_t m_i; \
  tagged_t m_j; \
 \
  m_i = X; \
  DerefSwitch(m_i,m_j,;) \
  Xderef = m_i; \
}

//TODO:[merge-oc] DerefSw_HVA_CVA_SVA_Other?
#define SwitchOnVar(Reg,Aux,HVACode,CVACode,SVACode,NVACode) \
{ \
    for (;;) \
      { \
          if (!IsVar(Reg)) \
            NVACode \
          else if (Reg & TagBitSVA) \
            { RefSVA(Aux,Reg); \
              if (Reg!=Aux) { Reg=Aux; continue; } \
              else SVACode \
            } \
          else if (!(Reg & TagBitCVA)) \
            { RefHVA(Aux,Reg); \
              if (Reg!=Aux) { Reg=Aux; continue; } \
              else HVACode \
            } \
          else \
            { RefCVA(Aux,Reg); \
              if (Reg!=Aux) { Reg=Aux; continue; } \
              else CVACode \
            } \
          break; \
        } \
}

#define SwitchOnHeapVar(Reg,Aux,HVACode,CVACode,NVACode) \
{ \
    for (;;) \
      { \
          if (!IsVar(Reg)) \
            NVACode \
          else if (!(Reg & TagBitCVA)) \
            { RefHVA(Aux,Reg); \
              if (Reg!=Aux) { Reg=Aux; continue; } \
              else HVACode \
            } \
          else \
            { RefCVA(Aux,Reg); \
              if (Reg!=Aux) { Reg=Aux; continue; } \
              else CVACode \
            } \
          break; \
        } \
}



#define DerefSwitch(Reg,Aux,VarCode) \
{ \
  if (IsVar(Reg)) \
    do \
      if (Reg == (Aux = *TagToPointer(Reg))) \
        {VarCode;break;} \
    while (IsVar(Reg=Aux)); \
}

#define DerefHeapSwitch(Reg,Aux,VarCode) DerefSwitch(Reg,Aux,VarCode)


#define YoungerHeapVar(Q,R)     HeapYounger(Q,R)
#define YoungerStackVar(Q,R)    StackYounger(Q,R)

#if defined(PARBACK) || defined(ANDPARALLEL)
#define CondHVA(X)              (!OffHeaptop(X,w->global_uncond) || !OnHeap(TagToPointer(X)))
#define CondCVA(X)              (!OffHeaptop(TagHVA(TagToCVA(X)),w->global_uncond) || !OnHeap(TagToPointer(X)))
#define CondSVA(X)              (!OffStacktop(X,w->local_uncond) || !OnStack(TagToPointer(X)))
#else
#define CondHVA(X)              (!OffHeaptop(X,w->global_uncond))
#define CondCVA(X)              (!OffHeaptop(TagHVA(TagToCVA(X)),w->global_uncond))
#define CondSVA(X)              (!OffStacktop(X,w->local_uncond))
#endif
#define CondStackvar(X)         CondSVA(X)

/* segfault patch -- jf */
CVOID__PROTO(trail_push_check, tagged_t x);

/* segfault patch -- jf */
#define BindCVA_NoWake(U,V) \
{ \
  TrailPushCheck(w->trail_top,U); \
  *TagToCVA(U) = V; \
}

#define BindCVA(U,V)                            \
  {                                             \
    Wake;                                       \
    TrailPushCheck(w->trail_top,U);             \
    *TagToCVA(U) = V;                           \
  }

#define BindSVA(U,V)                            \
  {                                             \
    if (CondSVA(U))                             \
      TrailPushCheck(w->trail_top,U);           \
    *TagToSVA(U) = V;                           \
  }

#define BindHVA(U,V)                            \
  {                                             \
    if (CondHVA(U))                             \
      TrailPushCheck(w->trail_top,U);           \
    *TagToHVA(U) = V;                           \
  }

#define Wake \
{ \
  SetEvent, Heap_Warn_Soft = HeapCharOffset(Heap_Warn_Soft,-1); \
}

#define WakeCount (TestEvent ? HeapCharDifference(Heap_Warn_Soft,Heap_Start) : 0)

#define NULL_TRAIL_ENTRY MakeSmall(0)
#define IsCanceled(T) (T == NULL_TRAIL_ENTRY)
#define NullifyTrailEntry(P) *(P) = NULL_TRAIL_ENTRY

//TODO: nullify fake trail entries with a predicate which makes nothing.
#define PlainUntrail(TR,Ref,CONT)                                       \
  {                                                                     \
    Ref = TrailPop(TR);                                                 \
    if (!IsVar(Ref))                                                    \
      {if (!IsCanceled(Ref)) CONT}                                      \
    else                                                                \
      *TagToPointer(Ref) = Ref;                                         \
  } 

/* SERIOUS_FAULT - a fault that should not occur- indicating a corruption
                  such as following the STR tag not coming to a FNT tag
                  this kind of fault may not need to be testing in final
                  version but must in testing cause a total abort
   USAGE_FAULT   - a fault in the usage(incorrect parameters) of a 
                  builtin predicate - an error message is written.
   MINOR_FAULT   - a fault that should result in a error message being
                  written somewhere, but the builtin predicate just
                  fails and is not aborted
*/

/* =========================================================================== */
/* Faults and errors */

/* Exit code from wam() when aborting */
#define WAM_ABORT -32768 /* see exceptions.pl */
#define WAM_INTERRUPTED -32767

#include <setjmp.h>
#include <ciao/os_signal.h>

extern SIGJMP_BUF abort_env;

void failc(char *mesg);

/* Exit for child processes (avoid flushing and closign standard I/O from parent) */
#define _EXIT(Code) { fflush(NULL); _exit((Code)); }

#define SERIOUS_FAULT(Y) { \
    failc(Y); \
    SIGLONGJMP(abort_env, WAM_ABORT); \
  }
                          
#define MAJOR_FAULT(Y) { failc(Y); return FALSE; }

#define USAGE_FAULT(Y) { failc(Y); return FALSE; }

#define MINOR_FAULT(Y) { return FALSE; }

/* =========================================================================== */
/* Error codes, xref errhandle.pl, internals.pl //) */

/* OGRAMA: error classification ISO PROLOG */
/* JFMC: renamed macros */

/* For any change / addition to this list:
   
            PLEASE DO READ AND UPDATE THE CORRESPONDING FACTS IN
                        core/engine/internals.pl

    Error messages are given by core/lib/errhandle.pl .  Learn how these
    work before updating anything here.
*/
/* TODO:[oc-merge] generate automatically, update */

/* Errors identifiers cannot be zero (as 0 = -0) */
#define INSTANTIATION_ERROR     1
#define UNINSTANTIATION_ERROR   2
#define TYPE_ERROR(D)           (RANGE_PER_ERROR*error_start(type)+D)
#define DOMAIN_ERROR(D)         (RANGE_PER_ERROR*error_start(dom)+D)
#define EXISTENCE_ERROR(D)      (RANGE_PER_ERROR*error_start(exist)+D)
#define PERMISSION_ERROR(D,F)   (RANGE_PER_ERROR*error_start(perm)+D*10+F)
#define REPRESENTATION_ERROR(D) (RANGE_PER_ERROR*error_start(repres)+D)
#define EVALUATION_ERROR(D)     (RANGE_PER_ERROR*error_start(eval)+D)
#define RESOURCE_ERROR(D)       (RANGE_PER_ERROR*error_start(res)+D)
#define SYNTAX_ERROR            (RANGE_PER_ERROR*error_start(syntax))
#define SYSTEM_ERROR            (RANGE_PER_ERROR*error_start(system))
/* TODO:[oc-merge] add FOREIGN_ERROR in optim-comp */
#define FOREIGN_ERROR           (RANGE_PER_ERROR*error_start(foreign))
#define USER_EXCEPTION          (RANGE_PER_ERROR*error_start(user))

#define RANGE_PER_ERROR 100                    /* Enough number of errors */
#define error_start(KEY) error_start__##KEY
#define error_start__inst    0
#define error_start__type    1
#define error_start__dom     2
#define error_start__exist   3
#define error_start__perm    4
#define error_start__repres  5
#define error_start__eval    6
#define error_start__res     7
#define error_start__syntax  8
#define error_start__system  9
/* TODO:[oc-merge] add error_start(foreign) in optim-comp */
#define error_start__foreign 10
#define error_start__user    11

/* TYPE_ERRORS */
#define TYPE_ERRORS(KEY) TYPE_ERRORS__##KEY
#define TYPE_ERRORS__atom 0
#define TYPE_ERRORS__atomic 1
#define TYPE_ERRORS__byte 2
#define TYPE_ERRORS__character 3
#define TYPE_ERRORS__compound 4
#define TYPE_ERRORS__evaluable 5
#define TYPE_ERRORS__in_byte 6
#define TYPE_ERRORS__integer 7
#define TYPE_ERRORS__list 8
#define TYPE_ERRORS__number 9
#define TYPE_ERRORS__predicate_indicator 10
#define TYPE_ERRORS__variable 11
#define TYPE_ERRORS__callable 12
//--
/* TODO:[oc-merge] unfold */
#define STRICT_ATOM TYPE_ERRORS(atom)
#define ATOMIC TYPE_ERRORS(atomic)
/* TODO:[oc-merge] renamed from BYTE, backport to optim-comp */
#define TY_BYTE TYPE_ERRORS(byte)
/* TODO:[oc-merge] used? */
#define CHARACTER TYPE_ERRORS(character)
#define COMPOUND TYPE_ERRORS(compound)
#define EVALUABLE TYPE_ERRORS(evaluable)
#define IN_BYTE TYPE_ERRORS(in_byte)
#define INTEGER TYPE_ERRORS(integer)
#define LIST TYPE_ERRORS(list)
#define NUMBER TYPE_ERRORS(number)
#define PREDICATE_INDICATOR TYPE_ERRORS(predicate_indicator)
/* RH: Not ISO anymore (from corrigendum 2) */
/* #define VARIABLE TYPE_ERRORS(variable) */
#define CALLABLE TYPE_ERRORS(callable)

/* DOMAIN_ERRORS */
#define DOMAIN_ERRORS(KEY) DOMAIN_ERRORS__##KEY
#define DOMAIN_ERRORS__character_code_list 0
#define DOMAIN_ERRORS__source_sink 1
#define DOMAIN_ERRORS__stream 2
#define DOMAIN_ERRORS__io_mode 3
#define DOMAIN_ERRORS__non_empty_list 4
#define DOMAIN_ERRORS__not_less_than_zero 5
#define DOMAIN_ERRORS__operator_priority 6
#define DOMAIN_ERRORS__prolog_flag 7
#define DOMAIN_ERRORS__read_option 8
#define DOMAIN_ERRORS__flag_value 9
#define DOMAIN_ERRORS__close_option 10
#define DOMAIN_ERRORS__stream_option 11
#define DOMAIN_ERRORS__stream_or_alias 12
#define DOMAIN_ERRORS__stream_position 13
#define DOMAIN_ERRORS__stream_property 14
#define DOMAIN_ERRORS__write_option 15
#define DOMAIN_ERRORS__operator_specifier 16
//--
/* TODO:[oc-merge] unfold */
/* TODO:[oc-merge] not in optim-comp */
#define CHARACTER_CODE_LIST     DOMAIN_ERRORS(character_code_list)
/* TODO:[oc-merge] not in optim-comp */
#define SOURCE_SINK             DOMAIN_ERRORS(source_sink)
/* TODO:[oc-merge] not in optim-comp */
#define STREAM                  DOMAIN_ERRORS(stream)
#define IO_MODE DOMAIN_ERRORS(io_mode)
/* TODO:[oc-merge] not renamed in optim-comp */
#define NON_EMPTY_LIST DOMAIN_ERRORS(non_empty_list)
#define NOT_LESS_THAN_ZERO DOMAIN_ERRORS(not_less_than_zero)
#define OPERATOR_PRIORITY DOMAIN_ERRORS(operator_priority)
#define PROLOG_FLAG DOMAIN_ERRORS(prolog_flag)
#define READ_OPTION DOMAIN_ERRORS(read_option)
#define FLAG_VALUE DOMAIN_ERRORS(flag_value)
#define CLOSE_OPTION DOMAIN_ERRORS(close_option)
#define STREAM_OPTION DOMAIN_ERRORS(stream_option)
#define STREAM_OR_ALIAS DOMAIN_ERRORS(stream_or_alias)
#define STREAM_POSITION DOMAIN_ERRORS(stream_position)
#define STREAM_PROPERTY DOMAIN_ERRORS(stream_property)
#define WRITE_OPTION DOMAIN_ERRORS(write_option)
#define OPERATOR_SPECIFIER DOMAIN_ERRORS(operator_specifier)

/* EXISTENCE_ERRORS */
#define EXISTENCE_ERRORS(KEY) EXISTENCE_ERRORS__##KEY
#define EXISTENCE_ERRORS__procedure 0
#define EXISTENCE_ERRORS__source_sink 1
#define EXISTENCE_ERRORS__stream 2
//--
#define PROCEDURE EXISTENCE_ERRORS(procedure)
/* TODO:[oc-merge] unfold */
/* SOURCE_SINK and STREAM already defined */
/* #define SOURCE_SINK EXISTENCE_ERRORS(source_sink) */
/* #define STREAM EXISTENCE_ERRORS(stream) */

/* PERMISION_ERRORS: composed of type of action + object on which the action
   is defined */

/* PERMISSION_TYPE */
#define PERMISSION_TYPES(KEY) PERMISSION_TYPES__##KEY
#define PERMISSION_TYPES__access 0
#define PERMISSION_TYPES__create 1
#define PERMISSION_TYPES__input 2
#define PERMISSION_TYPES__modify 3
#define PERMISSION_TYPES__open 4
#define PERMISSION_TYPES__output 5
#define PERMISSION_TYPES__reposition 6
//--
/* TODO:[oc-merge] unfold */
#define ACCESS PERMISSION_TYPES(access)
#define CREATE PERMISSION_TYPES(create)
#define INPUT PERMISSION_TYPES(input)
#define MODIFY PERMISSION_TYPES(modify)
#define OPEN PERMISSION_TYPES(open)
#define OUTPUT PERMISSION_TYPES(output)
#define REPOSITION PERMISSION_TYPES(reposition)

/* OBJECTS */
#define PERMISSION_OBJECTS(KEY) PERMISSION_OBJECTS__##KEY
#define PERMISSION_OBJECTS__binary_stream 0
#define PERMISSION_OBJECTS__source_sink 1
#define PERMISSION_OBJECTS__stream 2
#define PERMISSION_OBJECTS__text_stream 3
#define PERMISSION_OBJECTS__flag 4
#define PERMISSION_OBJECTS__operator 5
#define PERMISSION_OBJECTS__past_end_of_stream 6
#define PERMISSION_OBJECTS__private_procedure 7
#define PERMISSION_OBJECTS__static_procedure 8
//--
/* TODO:[oc-merge] unfold */
#define BINARY_STREAM PERMISSION_OBJECTS(binary_stream)
/* SOURCE_SINK and STREAM already defined */
/* #define SOURCE_SINK PERMISSION_OBJECTS(binary_stream) */
/* #define STREAM PERMISSION_OBJECTS(binary_stream) */
#define TEXT_STREAM PERMISSION_OBJECTS(text_stream)
#define FLAG PERMISSION_OBJECTS(flag)
#define OPERATOR PERMISSION_OBJECTS(operator)
#define PAST_END_OF_STREAM PERMISSION_OBJECTS(past_end_of_stream)
#define PRIVATE_PROCEDURE PERMISSION_OBJECTS(private_procedure)
#define STATIC_PROCEDURE PERMISSION_OBJECTS(static_procedure)

/* REPRESENTATION_ERROR */

/* CHARACTER_CODE_LIST already defined */
#define REPRESENTATION_ERRORS(KEY) REPRESENTATION_ERRORS__##KEY
#define REPRESENTATION_ERRORS__character_code_list 0
#define REPRESENTATION_ERRORS__in_character_code 1
#define REPRESENTATION_ERRORS__max_arity 2
#define REPRESENTATION_ERRORS__character 3
#define REPRESENTATION_ERRORS__max_integer 4
#define REPRESENTATION_ERRORS__min_integer 5
#define REPRESENTATION_ERRORS__character_code 6
#define REPRESENTATION_ERRORS__nan_or_inf_to_integer 7
#define REPRESENTATION_ERRORS__max_atom_length 8
//--
/* TODO:[oc-merge] unfold */
/* #define CHARACTER_CODE_LIST REPRESENTATION_ERRORS(character_code_list) */
#define IN_CHARACTER_CODE REPRESENTATION_ERRORS(in_character_code)
#define MAX_ARITY REPRESENTATION_ERRORS(max_arity)
/* #define CHARACTER REPRESENTATION_ERRORS(character) */
#define MAX_INTEGER REPRESENTATION_ERRORS(max_integer)
#define MIN_INTEGER REPRESENTATION_ERRORS(min_integer)
#define CHARACTER_CODE REPRESENTATION_ERRORS(character_code)
/* TODO:[oc-merge] new */
#define NAN_OR_INF_TO_INTEGER REPRESENTATION_ERRORS(nan_or_inf_to_integer)
#define MAX_ATOM_LENGTH       REPRESENTATION_ERRORS(max_atom_length)  /* Unneeded with dynamic atom sizes */

/* EVALUATION_ERROR */
#define EVALUATION_ERRORS(KEY) EVALUATION_ERRORS__##KEY
#define EVALUATION_ERRORS__float_overflow 0
#define EVALUATION_ERRORS__int_overflow 1
#define EVALUATION_ERRORS__e_undefined 2
#define EVALUATION_ERRORS__e_underflow 3
#define EVALUATION_ERRORS__zero_divisor 4
//--
/* TODO:[oc-merge] unfold */
#define FLOAT_OVERFLOW EVALUATION_ERRORS(float_overflow)
#define INT_OVERFLOW EVALUATION_ERRORS(int_overflow)
#define E_UNDEFINED EVALUATION_ERRORS(e_undefined)
#define E_UNDERFLOW EVALUATION_ERRORS(e_underflow)
#define ZERO_DIVISOR EVALUATION_ERRORS(zero_divisor)

/* TODO:[oc-merge] new */
/* RESOURCE_ERROR */
#define RESOURCE_ERRORS(KEY) RESOURCE_ERRORS__##KEY
#define RESOURCE_ERRORS__r_undefined 0
#define RESOURCE_ERRORS__r_stack 1
//--
/* TODO:[oc-merge] unfold */
#define R_UNDEFINED RESOURCE_ERRORS(r_undefined)
#define R_STACK RESOURCE_ERRORS(r_stack)

/* =========================================================================== */

/* OGRAMA: OLD VERSION ---------------------------------------------------- */ 
/* #define TYPE_ERROR(Type) (32+Type) */ /* includes also domain errors */ 

/* #define INSTANTIATION_ERROR 1
#define READ_PAST_EOS_ERROR 2
#define NO_READ_PERMISSION 3
#define NO_WRITE_PERMISSION 4
#define NO_SUCH_FILE 5
#define NO_OPEN_PERMISSION 6
#define NO_ACCESS_PERMISSION 7
#define SYSTEM_ERROR 8 OGRAMA */

/* Type codes for TYPE_ERROR  //) */
/* #define STRICT_ATOM 0
#define ATOMIC 1
#define TY_BYTE 2
#define CALLABLE 3
#define COMPOUND 4
#define EVALUABLE 5
#define IN_BYTE 6
#define INTEGER 7
#define LIST 8
#define NUMBER 9
#define PREDICATE_INDICATOR 10
#define VARIABLE 11 
*/

/*
#define CHARACTER_CODE_LIST 32 First domain code 
#define STREAM_OR_ALIAS 33
#define SOURCE_SINK 34  OGRAMA */
/* END OLD VERSION ----------------------------------------------------------- */

/* Exceptions (backport from optim_comp) */
/* TODO: pass worker as argument to macros? */

/* usage: goto, continue, break is forbidden inside CODE! */
#define EXCEPTION__CATCH(CODE, HANDLER) ({ \
  SIGJMP_BUF catch_exception__handler; \
  SIGJMP_BUF *catch_exception__old_handler; \
  catch_exception__old_handler = w->misc->errhandler; \
  w->misc->errhandler = &catch_exception__handler; \
  if (SIGSETJMP(catch_exception__handler)) { \
    /* just in case of a worker expansion */ \
    w = desc->worker_registers; \
    w->misc->errhandler = catch_exception__old_handler; \
    HANDLER; \
  } else { \
    CODE; \
  } \
})

#define UNLOCATED_EXCEPTION(Code) {             \
    ErrCode = Code;                             \
    ErrFuncName = "unknown";                    \
    ErrFuncArity = -1;                          \
    ErrArgNo = 0;                               \
    Culprit = TaggedZero;                       \
    EXCEPTION__THROW;                           \
  }

#define EXCEPTION__THROW SIGLONGJMP(*w->misc->errhandler, 1)

/* Throwing exceptions from builtins */

#define ERR__FUNCTOR(NAME, ARITY) \
  static char *const err__name = NAME; static const int err__arity = ARITY; 

#define BUILTIN_ERROR(Code,Culpr,ArgNo) ({ \
  ErrCode = Code; \
  ErrFuncName = err__name; \
  ErrFuncArity = err__arity; \
  ErrArgNo = ArgNo; Culprit = Culpr; \
  EXCEPTION__THROW; \
})

#define ERROR_IN_ARG(Arg,ArgNo,ReqType) ({ \
  BUILTIN_ERROR(IsVar(Arg) ? INSTANTIATION_ERROR : TYPE_ERROR(ReqType), \
                Arg, ArgNo); \
})

/* =========================================================================== */

/* MakeLST(To,Car,Cdr):
   
   Set 'To' to a term tagged_t LST
   whose car and cdr are 'Car' and Cdr'.

   'To' may be identical to 'Car' or 'Cdr'.
*/
#define MakeLST(To,Car,Cdr) \
{ tagged_t makelst_car = (Car); \
  HeapPush(w->global_top,makelst_car); \
  HeapPush(w->global_top,Cdr); \
  To = Tag(LST,HeapOffset(w->global_top,-2)); \
}

#define LSTCELLS 2 /* Cells allocated in MakeLST */

/* MakeSTR(To,Functor):
   
   Set 'To' to a term tagged_t STR
   whose principal functor is 'Functor'.  
   Space is allocated for the arguments, but they are not filled in.
*/
#define MakeSTR(To,Functor) \
{ \
  HeapPush(w->global_top,Functor); \
  To = Tag(STR,HeapOffset(w->global_top,-1)); \
  w->global_top = HeapOffset(w->global_top,Arity(Functor)); \
}

#define CBOOL__UnifyCons(U,V) \
{ tagged_t m_t0, m_u=U, m_t1=V; \
  SwitchOnVar(m_t1,m_t0,{BindHVA(m_t1,m_u);}, \
                    {BindCVA(m_t1,m_u);}, \
                    {BindSVA(m_t1,m_u);}, \
                    {if (m_t1!=m_u) return FALSE;}) \
}

/* =========================================================================== */

#define EXPAND_ATOM_BUFFER(new_max_atom_length) \
{ \
     Atom_Buffer = \
       checkrealloc_ARRAY(char, \
                          Atom_Buffer_Length,          \
                          new_max_atom_length,         \
                          Atom_Buffer);                \
    Atom_Buffer_Length = new_max_atom_length; \
    UpdateHeapMargins(); \
}

/* =========================================================================== */

extern intmach_t (*eng_goal_from_thread_id)(THREAD_ID id);

#endif /* _DATADEFS_H */
